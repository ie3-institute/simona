/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.em.EmModelStrat
import edu.ie3.simona.model.em.opt.OptimizingFlexStrat.*
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.AssetEnergyBoundaries
import edu.ie3.simona.service.Data.SecondaryData
import edu.ie3.simona.service.Data.SecondaryData.{
  ProsumerPrice,
  SecondarySeriesData,
}
import edu.ie3.simona.service.{
  DataTimeType,
  ServiceRegistrationData,
  ServiceType,
}
import edu.ie3.util.scala.quantities.DefaultQuantities.onePU
import optimus.algebra.{Const, Expression}
import optimus.optimization.MPModel
import optimus.optimization.enums.{SolutionStatus, SolverLib}
import optimus.optimization.model.{MPFloatVar, MPVar}
import org.slf4j.{Logger, LoggerFactory}
import squants.energy.PowerConversions.PowerNumeric
import squants.{Dimensionless, Energy, Power, Time}

import java.util.UUID
import scala.collection.immutable.SortedMap

/** Energy management strategy that optimizes flexibility usage over a fixed
  * amount of time steps into the future. Takes [[EnergyBoundariesFlexOptions]]
  * as inputs, which provide the required energy and power limits for
  * constraining asset operation.
  *
  * @param sampleTime
  *   The amount of time between the steps.
  * @param predictionHorizon
  *   The amount of time that is predicted into the future, i.e. the last step
  *   is this amount of time away from the current point in simulation time.
  *   Should be a multiple of [[sampleTime]].
  * @param objectiveFactory
  *   The factory creating asset symbols and the optimization objective to use.
  */
final case class OptimizingFlexStrat(
    sampleTime: Time,
    predictionHorizon: Time,
    objectiveFactory: ObjectiveFactory[? <: AssetStepSymbols],
) extends EmModelStrat[EnergyBoundariesFlexOptions] {

  private val logger: Logger = LoggerFactory.getLogger(
    s"${classOf[OptimizingFlexStrat].getSimpleName}(${objectiveFactory.getClass.getSimpleName})"
  )

  override def getServiceRegistrationData: ServiceRegistrationData = {
    ServiceRegistrationData(
      objectiveFactory.getRequiredSecondaryServices,
      DataTimeType.CurrentAndForecast(
        forecastLength = predictionHorizon,
        forecastResolution = sampleTime,
      ),
    )
  }

  /** The power target might not be considered by all types of objectives.
    */
  override def determineFlexControl(
      flexOptions: Iterable[(? <: AssetInput, EnergyBoundariesFlexOptions)],
      target: Power,
      currentTick: Long,
      receivedData: Seq[SecondaryData],
  ): Iterable[(UUID, Power)] = {

    given model: MPModel = MPModel(SolverLib.oJSolver)

    val sampleTicks = sampleTime.toSeconds.toLong
    val lastPredictedTick = currentTick + predictionHorizon.toSeconds.toLong

    val ticks =
      Range.Long.inclusive(currentTick, lastPredictedTick, sampleTicks)

    val flexOptionsById =
      flexOptions.map { case (asset: AssetInput, fo) => asset.getUuid -> fo }

    val (allAssetSymbols, objectiveContainer) =
      buildModel(
        flexOptionsById,
        sampleTime,
        ticks,
        target,
        receivedData,
        objectiveFactory,
      )

    model.minimize(objectiveContainer.objective)

    model.start()

    if model.getStatus != SolutionStatus.OPTIMAL then
      throw new CriticalFailureException(
        s"Optimization ended with unexpected status ${model.getStatus}, ${SolutionStatus.OPTIMAL} was expected."
      )

    objectiveContainer.accuracyChecks
      .filter(_.getError > softConstraintThreshold)
      .foreach { constraint =>
        logger.warn(constraint.getWarningMessage)
      }

    // we're only interested in the solutions for the current time step
    val flexOptionsMap = flexOptionsById.toMap
    val assetCtrl = allAssetSymbols.map {
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

        // make sure that set point is within allowed power.
        // floating point rounding errors might move it slightly outside the interval.
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

    model.release()

    assetCtrl
  }

  override def adaptFlexOptions(
      assetInput: AssetInput,
      flexOptions: EnergyBoundariesFlexOptions,
  ): EnergyBoundariesFlexOptions = flexOptions

}

object OptimizingFlexStrat {

  /** Threshold for the error of soft constraints after optimization. Every soft
    * constraint error should stay below this threshold.
    */
  private val softConstraintThreshold: Double = 1e-3

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
    * @param receivedData
    *   The secondary data received by the EM agent. Empty if none was received.
    * @param objectiveFactory
    *   The factory creating asset symbols and the optimization objective to
    *   use.
    * @param model
    *   The optimization model to add variables and constraints to.
    * @tparam AV
    *   The type of asset symbols that the objective factory returns.
    * @return
    *   The created asset symbols containers and the objective container.
    */
  def buildModel[AV <: AssetStepSymbols](
      flexOptions: Iterable[(UUID, EnergyBoundariesFlexOptions)],
      sampleTime: Time,
      ticks: Seq[Long],
      target: Power,
      receivedData: Seq[SecondaryData],
      objectiveFactory: ObjectiveFactory[AV],
  )(using
      model: MPModel
  ): (Iterable[AssetSymbolContainer[AV]], ObjectiveContainer) = {

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

    (
      assetSymbols,
      ObjectiveContainer(objective, Iterable.empty), // fixme
    )

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
              // we only determine energy until tickDisconnect
              // (after that, the asset is unavailable)
              ticks.takeWhile(_ <= tickDisconnect)
            }
            .getOrElse(ticks)
            .sliding(2)
            .foldLeft[(SortedMap[Long, AV], Expression)](
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
                  vars.getStateSymbol,
                )

              case _ =>
                // assets constraints need to be created at least for two time steps
                // (including eventual tickDisconnect restrictions)
                throw new CriticalFailureException(
                  s"Cannot create asset constraints for less than two time steps (given: ${ticks.size})"
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
      previousState: Expression,
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
        .map(_.getUpper)
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
      previousStateEnergy: Expression,
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

  /** Class holding symbols (variables or constants) used in optimization for an
    * asset at a single time step.
    */
  trait AssetStepSymbols {

    def getStateSymbol: Expression

    /** Returns the resulting operating power.
      *
      * This method should only be called after optimization has successfully
      * completed. Otherwise, results might not be available and an exception
      * might be thrown.
      */
    def getOperatingPowerResult: Power

    /** Returns the resulting state of energy.
      *
      * This method should only be called after optimization has successfully
      * completed. Otherwise, results might not be available and an exception
      * might be thrown.
      */
    def getStateOfEnergyResult: Energy

  }

  /** Container holding all optimization variables and soft constraints (as
    * [[AssetStepSymbols]]) for one or more assets referenced by a single UUID.
    *
    * @param assetUuid
    *   The UUID of the assets.
    * @param results
    *   All [[AssetStepSymbols]] of the assets, including state and operation
    *   variables. The step variables are referenced by stepStartTick within the
    *   sorted map. The sequence contains one or more variable maps, similar to
    *   how [[EnergyBoundariesFlexOptions]] can contain several asset
    *   boundaries.
    */
  final case class AssetSymbolContainer[AV <: AssetStepSymbols](
      assetUuid: UUID,
      results: Seq[SortedMap[Long, AV]],
  )

  /** Container holding the complete objective to minimize (which already
    * includes the soft constraint expressions) and relevant soft constraint
    * containers.
    *
    * @param objective
    *   The objective, including all soft constraint expressions.
    * @param accuracyChecks
    *   All soft constraints.
    */
  final case class ObjectiveContainer(
      objective: Expression,
      accuracyChecks: Iterable[ResultAccuracyCheck],
  )

  /** Trait to be implemented by factories of power objectives.
    *
    * @tparam AV
    *   The type of [[AssetStepSymbols]] that the objective factory produces and
    *   uses.
    */
  trait ObjectiveFactory[AV <: AssetStepSymbols] {

    /** @return
      *   All secondary services required by the optimization model.
      */
    def getRequiredSecondaryServices: Iterable[ServiceType]

    /** Creates asset symbols of type [[AV]] from given [[AssetStepParameters]]
      * according to the requirements of the objective factory.
      *
      * @param assetParams
      *   The asset parameters to use for creating asset symbols.
      * @param model
      *   The optimization model to add symbols and constraints to.
      * @return
      *   The asset symbols for the given asset and time step.
      */
    def createAssetSymbols(assetParams: AssetStepParameters)(using
        model: MPModel
    ): AV

    /** Builds an objective to minimize given the asset symbols and an objective
      * factory.
      *
      * @param flexOptions
      *   The flex options that connected assets provided.
      * @param assetSymbols
      *   The asset symbols to optimize for.
      * @param target
      *   The target power for each time step.
      * @param receivedData
      *   The received secondary data.
      * @param model
      *   The optimization model to add variables and constraints to.
      * @return
      *   The full objective, excluding eventual soft constraints.
      */
    def build(
        flexOptions: Iterable[(UUID, EnergyBoundariesFlexOptions)],
        assetSymbols: Iterable[AssetSymbolContainer[AV]],
        target: Power,
        receivedData: Seq[SecondaryData],
    )(using model: MPModel): Expression

    /** Extracts a price series map (if available) from the given received
      * secondary data. If no price series is available, an exception is thrown.
      *
      * @param receivedData
      *   The received data to extract prices from.
      * @return
      *   A map from tick to price data.
      */
    protected def extractPriceSeries(
        receivedData: Seq[SecondaryData]
    ): SortedMap[Long, ProsumerPrice] =
      receivedData
        .collectFirst { case SecondarySeriesData(series) =>
          series.map {
            case (tick, priceData: ProsumerPrice) =>
              tick -> priceData
            case (_, unexpectedData) =>
              throw new CriticalFailureException(
                s"Unexpected secondary data $unexpectedData"
              )
          }
        }
        .getOrElse(
          throw new CriticalFailureException(
            s"No price series data was provided."
          )
        )

    /** Re-orders the [[AssetStepSymbols]] inside given
      * [[AssetSymbolContainer]]s to be grouped by their by tick.
      *
      * @param assetSymbols
      *   The [[AssetSymbolContainer]]s to be reordered.
      * @return
      *   The [[AssetStepSymbols]] ordered by tick.
      */
    protected def sortSymbolsByTick(
        assetSymbols: Iterable[AssetSymbolContainer[AV]]
    ): SortedMap[Long, Iterable[AV]] = {
      // asset symbols should all have the same ticks,
      // since they should have all been created with the same ticks
      val ticks = assetSymbols.headOption
        .flatMap(_.results.headOption.map(_.keys.toSeq))
        .getOrElse(Seq.empty)

      // sort all asset symbols by tick
      ticks
        .map { stepStartTick =>
          stepStartTick -> assetSymbols.flatMap {
            _.results.flatMap(_.get(stepStartTick))
          }
        }
        .to(SortedMap)
    }

    /** Creates an unbounded continuous variable that is constrained to be
      * greater than all given expressions.
      *
      * @param segments
      *   The segments to create the epigraph variable for.
      * @param name
      *   The name of the variable.
      * @param model
      *   The optimization model to add the variable and constraints to.
      * @return
      *   The new epigraph variable.
      */
    protected def createEpigraphVar(segments: Seq[Expression], name: String)(
        using model: MPModel
    ): MPFloatVar = {
      val epigraph = MPFloatVar(name)
      segments.foreach(segment => model.add(epigraph >:= segment))

      epigraph
    }

    /** Creates a positive continuous variable that is constrained to be greater
      * than both the positive and negative version of given value.
      *
      * @param value
      *   The expression to create the absolute variable for.
      * @param name
      *   The name of the variable.
      * @param model
      *   The optimization model to add the variable and constraints to.
      * @return
      *   The new absolute variable.
      */
    protected def createAbsoluteVariable(value: Expression, name: String)(using
        model: MPModel
    ): MPFloatVar = {
      val valueAbs = MPFloatVar.positive(name)
      model.add(valueAbs >:= value)
      model.add(valueAbs >:= -value)
      valueAbs
    }

  }

  extension (expr: MPVar | Const) {

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
      }
  }

}
