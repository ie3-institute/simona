/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.em.opt.impl.ObjectiveFactory
import edu.ie3.simona.model.em.opt.impl.ObjectiveFactory.{
  AssetStepSymbols,
  AssetSymbolContainer,
}
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.AssetEnergyBoundaries
import edu.ie3.simona.service.Data.SecondaryData
import edu.ie3.util.scala.quantities.DefaultQuantities.{onePU, zeroKW}
import optimus.algebra.*
import optimus.optimization.MPModel
import optimus.optimization.enums.{SolutionStatus, SolverLib}
import optimus.optimization.model.MPVar
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

    lazy val ticks: Seq[Long] = {
      val sampleTicks = sampleTime.toSeconds.toLong
      val lastPredictedTick =
        currentTick + predictionHorizon.toSeconds.toLong

      Range.Long.inclusive(currentTick, lastPredictedTick, sampleTicks)
    }

  }

  final case class OptimizationResult(
      assetSymbols: Iterable[AssetSymbolContainer[? <: AssetStepSymbols]],
      solutionStatus: SolutionStatus,
      objectiveValue: Option[Double],
  )

  def optimize(params: OptimizationParams): OptimizationResult = {
    given model: MPModel = MPModel(params.solverLib)

    val ticks = params.timeParams.ticks

    val (allAssetSymbols, objective) =
      buildModel(
        params.flexOptionsById,
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
          params.flexOptionsById,
          allAssetSymbols,
          params.target,
          params.receivedData,
        )
      )

    OptimizationResult(
      assetSymbols = allAssetSymbols,
      solutionStatus = model.getStatus,
      objectiveValue = actualObjectiveValue,
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
          setPoint.max(flex.powerLimits.getLower).min(flex.powerLimits.getUpper)

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
    *
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
    * @tparam AV
    *   The type of asset symbols that the objective factory returns.
    * @return
    *   The created asset symbols containers and the objective expression.
    */
  def buildModel[AV <: AssetStepSymbols](
      flexOptions: Iterable[(UUID, EnergyBoundariesFlexOptions)],
      sampleTime: Time,
      ticks: Seq[Long],
      target: Power,
      receivedData: Iterable[SecondaryData],
      objectiveFactory: ObjectiveFactory[AV],
  )(using
      model: MPModel
  ): (Iterable[AssetSymbolContainer[AV]], Expression) = {

    val assetSymbols = addAssetConstraints(
      flexOptions,
      sampleTime,
      ticks,
      objectiveFactory,
    )

    val objective = objectiveFactory.build(
      flexOptions,
      assetSymbols,
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
  private def addAssetConstraints[AV <: AssetStepSymbols](
      flexOptions: Iterable[(UUID, EnergyBoundariesFlexOptions)],
      sampleTime: Time,
      ticks: Seq[Long],
      objectiveFactory: ObjectiveFactory[AV],
  )(using model: MPModel): Iterable[AssetSymbolContainer[AV]] = {
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
            .foldLeft[(SortedMap[Long, AV], MPSymbol)](
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
                  previousState,
                )

                val vars = objectiveFactory.createAssetSymbols(assetParams)

                (
                  previousResults.updated(stepStartTick, vars),
                  vars.getStepEndStateSymbol,
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
    * @param previousState
    *   The previous state symbol.
    * @return
    *   The results for this asset and time step.
    */
  private def createAssetParameters(
      energyBoundaries: AssetEnergyBoundaries,
      stepStartTick: Long,
      stepEndTick: Long,
      sampleTime: Time,
      previousState: MPSymbol,
  ): AssetStepParameters = {

    // we are interested in the energy limits at the end of the step interval,
    // since they tell us in which energy the power of this step interval may
    // result in
    val (limitsTickEnd, energyLimitsEnd) = energyBoundaries.energyLimits
      .maxBefore(stepEndTick + 1)
      .getOrElse(throw new CriticalFailureException("No energy limits found!"))

    // the energy limits at the beginning of the interval can in some
    // circumstances provide information on constraints
    val energyLimitsStart =
      energyBoundaries.energyLimits.maxBefore(limitsTickEnd).map {
        case (_, limits) => limits
      }

    if energyLimitsEnd.getLower == energyLimitsEnd.getUpper &&
      energyLimitsStart.forall(limits => limits.getLower == limits.getUpper)
    then {
      // there is no flexibility at all, thus we don't need any state to keep track of

      val stepStartEnergy = energyLimitsStart
        // we try to use the last energy value, if available
        .map(_.getUpper)
        // ... or this is the initial step, thus we start with initial energy
        .getOrElse(energyBoundaries.currentEnergy)
      val stepEndEnergy = energyLimitsEnd.getUpper

      val energyChange = stepEndEnergy - stepStartEnergy

      FixedPowerStepParameters(
        energyChange = energyChange,
        stepEndEnergy = stepEndEnergy,
        stepStartTick = stepStartTick,
        stepEndTick = stepEndTick,
        sampleTime = sampleTime,
      )
    } else {
      // we do have some flexibility at this point in time, model it

      VariablePowerStepParameters(
        previousStateEnergy = previousState,
        pMin = energyBoundaries.powerLimits.getLower,
        pMax = energyBoundaries.powerLimits.getUpper,
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
    * time step, but is requiring a fixed amount of energy to be added or
    * subtracted from the state energy. This necessarily results in fixed amount
    * of power at this time step.
    *
    * @param energyChange
    *   The amount of energy change required at this time step.
    * @param stepEndEnergy
    *   The state of energy at the end of this time step.
    */
  final case class FixedPowerStepParameters(
      energyChange: Energy,
      stepEndEnergy: Energy,
      override val stepStartTick: Long,
      override val stepEndTick: Long,
      override val sampleTime: Time,
  ) extends AssetStepParameters

  /** Holds parameters for an asset that is providing some flexibility at the
    * time step, which is restricted by the given power and energy limits as
    * well as charging and discharging efficiencies.
    *
    * @param previousStateEnergy
    *   The previous energy state variable or constant.
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
      previousStateEnergy: MPSymbol,
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
