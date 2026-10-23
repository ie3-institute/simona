/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

import edu.ie3.simona.event.ResultEvent.EmTrajectoryStepResult
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.em.opt.impl.ObjectiveFactory
import edu.ie3.simona.model.em.opt.impl.ObjectiveFactory.{
  AssetStepSymbols,
  AssetSymbolContainer,
}
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.AssetEnergyBoundaries
import edu.ie3.simona.service.Data.SecondaryData
import edu.ie3.simona.service.DataTimeType
import edu.ie3.util.scala.quantities.DefaultQuantities.{onePU, zeroKW}
import optimus.algebra.*
import optimus.common.Measure.TimeMeasurements
import optimus.optimization.MPModel
import optimus.optimization.enums.{SolutionStatus, SolverLib}
import optimus.optimization.model.MPVar
import squants.energy.EnergyConversions.EnergyNumeric
import squants.energy.PowerConversions.PowerNumeric
import squants.{Dimensionless, Energy, Power, Time}

import java.util.UUID
import scala.collection.immutable.SortedMap

object FlexibilityOptimization {

  type MPSymbol = MPVar | Const

  final case class OptimizationParams(
      flexOptionsById: Iterable[(UUID, EnergyBoundariesFlexOptions)],
      receivedData: Iterable[SecondaryData] = Iterable.empty,
      target: Power = zeroKW,
      timeParams: TimeParams,
      objectiveFactory: ObjectiveFactory[? <: AssetStepSymbols],
      solverLib: SolverLib,
      tightenBoundaries: Boolean,
  )

  final case class TimeParams(
      sampleTime: Time,
      predictionHorizon: Time,
      currentTick: Long,
  ) {

    lazy val ticks: Seq[Long] =
      DataTimeType.createTicks(predictionHorizon, sampleTime, currentTick)

  }

  final case class OptimizationResult(
      params: OptimizationParams,
      assetSymbols: Iterable[AssetSymbolContainer[? <: AssetStepSymbols]],
      solutionStatus: SolutionStatus,
      objectiveValue: Option[Double],
      timeMeasurements: TimeMeasurements,
      model: MPModel,
  ) {

    def energyTrajectories: Iterable[EmTrajectoryStepResult] = {
      val flexOptionsMap = params.flexOptionsById.toMap

      assetSymbols
        .flatMap { container =>
          val uuid = container.assetUuid

          val flex = flexOptionsMap.getOrElse(
            uuid,
            throw new CriticalFailureException(
              s"Flex options not found for asset $uuid"
            ),
          )

          val tightenedFlex =
            EnergyBoundariesFlexOptions.tighten(flex, params.timeParams.ticks)

          val resultsByTick = container.resultsByTick(params.timeParams.ticks)

          // results can be empty in some edge cases, e.g. EVCS with no connected EV
          if resultsByTick.nonEmpty then {

            val (currentTick, firstSymbols) =
              resultsByTick.headOption.getOrElse(
                throw new CriticalFailureException(
                  "Empty results"
                )
              )

            val currentState = firstSymbols.map(_.getStepStartEnergyResult).sum

            val currentStepRes = EmTrajectoryStepResult(
              uuid,
              currentTick,
              currentTick,
              currentState,
              currentState,
              currentState,
            )

            resultsByTick
              .map { case (_, allSymbols) =>
                val stepEndTick = allSymbols.headOption
                  .getOrElse(
                    throw new CriticalFailureException(
                      "Empty results"
                    )
                  )
                  .parameters
                  .stepEndTick

                val state = allSymbols.map(_.getStepEndEnergyResult).sum

                val energyLimits = tightenedFlex.energyBoundaries.flatMap {
                  _.energyLimits.maxBefore(stepEndTick + 1).map(_._2)
                }
                val (eMin, eMax) =
                  if energyLimits.nonEmpty then {
                    (
                      energyLimits.map(_.getLower).sum,
                      energyLimits.map(_.getUpper).sum,
                    )
                  } else {
                    // in case energy limits are empty, there was no flexibility
                    (state, state)
                  }

                EmTrajectoryStepResult(
                  uuid,
                  currentTick,
                  stepEndTick,
                  state,
                  eMin,
                  eMax,
                )

              }
              .toSeq
              .prepended(currentStepRes)

          } else Seq.empty
        }
    }
  }

  def optimize(params: OptimizationParams): OptimizationResult = {
    given model: MPModel = MPModel(params.solverLib)

    val ticks = params.timeParams.ticks

    val adaptedFlexOptionsById =
      if params.tightenBoundaries then
        params.flexOptionsById.map { case (uuid, fo) =>
          uuid -> EnergyBoundariesFlexOptions.tighten(fo, ticks)
        }
      else params.flexOptionsById

    val (allAssetSymbols, objective) =
      buildModel(
        adaptedFlexOptionsById,
        params.timeParams.sampleTime,
        ticks,
        params.target,
        params.receivedData,
        params.objectiveFactory,
      )

    model.minimize(objective)

    model.start(timeLimit = 10000)
    model.release()

    val actualObjectiveValue =
      Option.when(model.getStatus == SolutionStatus.OPTIMAL)(
        params.objectiveFactory.getComparableObjectiveValue(
          allAssetSymbols.sortByTick(ticks),
          params.target,
          params.receivedData,
        )
      )

    val timeMeasurements = model.getTimeMeasurements

    OptimizationResult(
      params = params,
      assetSymbols = allAssetSymbols,
      solutionStatus = model.getStatus,
      objectiveValue = actualObjectiveValue,
      timeMeasurements = timeMeasurements,
      model = model,
    )

  }

  def extractSetPoints(
      flexOptionsById: Iterable[(UUID, EnergyBoundariesFlexOptions)],
      result: OptimizationResult,
  ): Iterable[(UUID, Power)] = {
    // we're only interested in the solutions for the current time step
    val flexOptionsMap = flexOptionsById.toMap
    result.assetSymbols.map {
      case AssetSymbolContainer(assetUuid, assetSymbols) =>
        val setPoint = assetSymbols.map {
          // Taking only the first result for set points
          _.headOption
            .getOrElse(
              throw new CriticalFailureException(
                s"Empty results for asset $assetUuid"
              )
            ) match {
            case (_, res) =>
              // Operating point of first result
              res.getOperatingPowerResult
          }
        }
          // Add up solutions for all asset assigned to the same UUID
          .sum

        // Make sure that set point is within allowed power.
        // Floating point rounding errors might move it slightly outside the interval.
        val flex = flexOptionsMap.getOrElse(
          assetUuid,
          throw new CriticalFailureException(
            s"Flex options not found for $assetUuid"
          ),
        )
        val adaptedSetPoint =
          setPoint
            .max(flex.headPowerLimits.getLower)
            .min(flex.headPowerLimits.getUpper)

        assetUuid -> adaptedSetPoint
    }
  }

  /** Constructs the optimization model by creating the required variables and
    * constraints and the objective. Asset symbols and objective are created
    * using the objective factory.
    *
    * @param flexOptions
    *   The [[EnergyBoundariesFlexOptions]] to base the optimization model on.
    * @param sampleTime
    *   The amount of time between steps.
    * @param ticks
    *   The ticks (including current tick and last predicted tick) of the time
    *   steps to consider in the optimization. The ticks should all be exactly
    *   the sample time duration (in seconds) apart from each other.
    * @param target
    *   The target power to aim for. This parameter might not be considered by
    *   all objective factories.
    *
    * @param receivedData
    *   The secondary data received by the EM agent. Empty if none was received.
    * @param objectiveFactory
    *   The factory creating asset symbols and the optimization objective to
    *   use.
    *
    * @param model
    *   The optimization model to add variables and constraints to.
    * @tparam AS
    *   The type of asset symbols that the objective factory returns.
    * @return
    *   The created asset symbols containers and the objective expression.
    */
  private def buildModel[AS <: AssetStepSymbols](
      flexOptions: Iterable[(UUID, EnergyBoundariesFlexOptions)],
      sampleTime: Time,
      ticks: Seq[Long],
      target: Power,
      receivedData: Iterable[SecondaryData],
      objectiveFactory: ObjectiveFactory[AS],
  )(using
      model: MPModel
  ): (Iterable[AssetSymbolContainer[AS]], Expression) = {

    val assetSymbols = addAssetConstraints(
      flexOptions,
      sampleTime,
      ticks,
      objectiveFactory,
    )

    val objective = objectiveFactory.build(
      flexOptions,
      assetSymbols.sortByTick(ticks),
      target,
      receivedData,
    )

    (assetSymbols, objective)
  }

  /** Creates and adds constraints for the given flex options for given sample
    * time. States and operating points are linked according to the time steps.
    *
    * @param flexOptions
    *   The [[EnergyBoundariesFlexOptions]] to create asset symbols from.
    * @param sampleTime
    *   The amount of time between the steps.
    * @param ticks
    *   The ticks (including current tick and last predicted tick) of the time
    *   steps to consider in the optimization. The ticks should all be exactly
    *   the sample time duration (in seconds) apart from each other.
    *
    * @param objectiveFactory
    *   The factory that creates asset symbols.
    * @param model
    *   The optimization model to add variables and constraints to.
    * @return
    *   Containers that holds all results, including state and operation
    *   variables.
    */
  private def addAssetConstraints[AS <: AssetStepSymbols](
      flexOptions: Iterable[(UUID, EnergyBoundariesFlexOptions)],
      sampleTime: Time,
      ticks: Seq[Long],
      objectiveFactory: ObjectiveFactory[AS],
  )(using model: MPModel): Iterable[AssetSymbolContainer[AS]] = {
    flexOptions.map { case (assetUUID, fo) =>
      val allAssetSymbols = fo.energyBoundaries
        .map { assetBoundaries =>
          val startState = Const(assetBoundaries.currentEnergy.toKilowattHours)

          val (assetSymbols, _) = assetBoundaries.tickDisconnect
            .map { tickDisconnect =>
              val firstTick = ticks.headOption.getOrElse(
                throw new CriticalFailureException("Ticks are empty!")
              )
              val sampleTicks = sampleTime.toSeconds

              // make sure that possible energy boundaries at
              // disconnect tick are still included
              val disconnectStep =
                (tickDisconnect - firstTick).toDouble / sampleTicks
              val adaptedTickDisconnect =
                firstTick + math.ceil(disconnectStep) * sampleTicks

              // we only determine energy until disconnect tick
              // (after that, the asset is unavailable)
              ticks.takeWhile(_ <= adaptedTickDisconnect)
            }
            .getOrElse(ticks)
            .sliding(2)
            .foldLeft[(SortedMap[Long, AS], MPSymbol)](
              (SortedMap.empty, startState)
            ) {
              case (
                    (previousResults, previousState),
                    Seq(stepStartTick, stepEndTick),
                  ) =>
                val assetParams = createAssetParameters(
                  assetBoundaries,
                  stepStartTick,
                  stepEndTick,
                  sampleTime,
                )

                val symbols = objectiveFactory.createAssetSymbols(
                  assetParams,
                  previousState,
                )

                (
                  previousResults.updated(stepStartTick, symbols),
                  symbols.stepEndState,
                )

              case _ =>
                // assets constraints need to be created at least for two time steps
                // (including eventual tickDisconnect restrictions)
                throw new CriticalFailureException(
                  s"Cannot create asset constraints for less than two time steps (ticks: ${ticks.size})"
                )
            }

          assetSymbols
        }

      AssetSymbolContainer(assetUUID, allAssetSymbols)
    }
  }

  /** Creates [[AssetStepParameters]] from the given energy boundaries for the
    * time interval with given ending tick.
    *
    * @param energyBoundaries
    *   The asset energy boundaries to use.
    * @param stepEndTick
    *   The step ending tick.
    * @return
    *   The results for this asset and time step.
    */
  private def createAssetParameters(
      energyBoundaries: AssetEnergyBoundaries,
      stepStartTick: Long,
      stepEndTick: Long,
      sampleTime: Time,
  ): AssetStepParameters = {

    val (_, powerLimits) = energyBoundaries.powerLimits
      .maxBefore(stepStartTick + 1)
      .getOrElse(throw new CriticalFailureException("No power limits found!"))

    if powerLimits.getLower == powerLimits.getUpper
    then {
      // there is no flexibility at all, thus we don't need any state to keep track of

      FixedPowerStepParameters(
        power = powerLimits.getUpper,
        stepStartTick = stepStartTick,
        stepEndTick = stepEndTick,
        sampleTime = sampleTime,
      )
    } else {
      // we do have some flexibility at this point in time, model it

      val (_, energyLimitsEnd) = energyBoundaries.energyLimits
        .maxBefore(stepEndTick + 1)
        .getOrElse(
          throw new CriticalFailureException("No energy limits found!")
        )

      VariablePowerStepParameters(
        pMin = powerLimits.getLower,
        pMax = powerLimits.getUpper,
        eMin = energyLimitsEnd.getLower,
        eMax = energyLimitsEnd.getUpper,
        etaCharge = energyBoundaries.etaCharge,
        etaDischarge = energyBoundaries.etaDischarge,
        stepStartTick = stepStartTick,
        stepEndTick = stepEndTick,
        sampleTime = sampleTime,
      )
    }

  }

  /** Object that holds parameters for the optimization of an asset at some time
    * step. Can be created from [[EnergyBoundariesFlexOptions]] and be used to
    * create [[AssetStepSymbols]] depending on the objective factory.
    */
  abstract class AssetStepParameters {

    /** The tick at the start of the time step interval, i.e. the tick at which
      * the operation of the step starts.
      */
    val stepStartTick: Long

    /** The tick at the end of the time step interval, i.e. the tick at which
      * the operation of the step ends (and a next step might start).
      */
    val stepEndTick: Long

    val sampleTime: Time
  }

  /** Holds parameters for an asset that is not providing flexibility at the
    * time step, but is requiring a fixed power.
    *
    * @param power
    *   The amount of fixed power at this time step.
    */
  final case class FixedPowerStepParameters(
      power: Power,
      override val stepStartTick: Long,
      override val stepEndTick: Long,
      override val sampleTime: Time,
  ) extends AssetStepParameters

  /** Holds parameters for an asset that is providing some flexibility at the
    * time step, which is restricted by the given power and energy limits as
    * well as charging and discharging efficiencies.
    *
    * @param pMin
    *   The minimum power allowed.
    * @param pMax
    *   The maximum power allowed.
    * @param eMin
    *   The minimum state of energy allowed at the end of this time step.
    * @param eMax
    *   The maximum state of energy allowed at the end of this time step.
    * @param etaCharge
    *   The charging efficiency.
    * @param etaDischarge
    *   The discharging efficiency.
    */
  final case class VariablePowerStepParameters(
      pMin: Power,
      pMax: Power,
      eMin: Energy,
      eMax: Energy,
      etaCharge: Dimensionless,
      etaDischarge: Dimensionless,
      override val stepStartTick: Long,
      override val stepEndTick: Long,
      override val sampleTime: Time,
  ) extends AssetStepParameters {
    def isInefficient: Boolean =
      etaCharge < onePU || etaDischarge < onePU
  }

  extension (expr: Expression) {

    /** Returns the set value for given variable or constant. For variables,
      * optimization has to have found a solution before calling this.
      *
      * @return
      *   The value of this expression.
      */
    def getValue: Double =
      expr match {
        case const: Const => const.value
        case variable: MPVar =>
          variable.value.getOrElse(
            throw new CriticalFailureException(
              s"No result present for variable $variable"
            )
          )
        case Term(scalar, vars) =>
          val scalarVal = scalar.value
          val varVals = vars.map(_.getValue)

          varVals.product * scalarVal
        case ConstProduct(scalar, expr) =>
          scalar.value * expr.getValue
        case Product(a, b) =>
          a.getValue * b.getValue
        case Plus(a, b) =>
          a.getValue + b.getValue
        case Minus(a, b) =>
          a.getValue - b.getValue

      }

  }
}
