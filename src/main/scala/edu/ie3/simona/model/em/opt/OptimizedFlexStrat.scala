/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.em.EmModelStrat
import edu.ie3.simona.model.em.opt.OptimizedFlexStrat.*
import edu.ie3.simona.model.em.opt.SoftConstraint.AbsValueSoftConstraint
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.AssetEnergyBoundaries
import edu.ie3.util.interval.ClosedInterval
import edu.ie3.util.scala.quantities.DefaultQuantities.{onePU, zeroKWh}
import optimus.algebra.{Const, Double2Const, Expression, Zero}
import optimus.optimization.MPModel
import optimus.optimization.enums.{SolutionStatus, SolverLib}
import optimus.optimization.model.{MPFloatVar, MPVar}
import org.slf4j.Logger
import squants.energy.Kilowatts
import squants.{Each, Power, Time}

import java.util.UUID

/** Energy management strategy that optimizes flexibility usage over a fixed
  * amount of time steps into the future. Takes [[EnergyBoundariesFlexOptions]]
  * as inputs, which provide the required energy boundaries and power limits for
  * constraining asset operation.
  *
  * @param sampleTime
  *   The amount of time between the steps.
  * @param predictionHorizon
  *   The amount of time that is predicted into the future, i.e. the last step
  *   is this amount of time away from the current point in simulation time.
  *   Should be a multiple of [[sampleTime]].
  * @param powerObjectiveFactory
  *   A factory creating the optimization objective to use.
  * @param logger
  *   The logger to use.
  */
final case class OptimizedFlexStrat(
    sampleTime: Time,
    predictionHorizon: Time,
    powerObjectiveFactory: PowerObjectiveFactory,
    logger: Logger,
) extends EmModelStrat[EnergyBoundariesFlexOptions] {

  override def determineFlexControl(
      flexOptions: Iterable[(? <: AssetInput, EnergyBoundariesFlexOptions)],
      target: Power,
      currentTick: Long,
  ): Iterable[(UUID, Power)] = {

    given model: MPModel = MPModel(SolverLib.oJSolver)

    val sampleTicks = sampleTime.toSeconds.toLong
    val lastPredictedTick = currentTick + predictionHorizon.toSeconds.toLong

    val ticks =
      Range.Long.inclusive(currentTick, lastPredictedTick, sampleTicks)

    val assetVars = addAssetConstraints(
      flexOptions.map { case (asset: AssetInput, fo) => asset.getUuid -> fo },
      sampleTime,
      ticks,
    )

    val objectiveContainer =
      buildObjective(assetVars, target, powerObjectiveFactory)

    model.minimize(objectiveContainer.objective)

    model.start()

    if model.getStatus != SolutionStatus.OPTIMAL then
      throw new CriticalFailureException(
        s"Optimization ended with unexpected status ${model.getStatus}, ${SolutionStatus.OPTIMAL} was expected."
      )

    objectiveContainer.softConstraints
      .filter(_.getError > softConstraintThreshold)
      .foreach { constraint =>
        logger.warn(constraint.getWarningMessage)
      }

    // we're only interested in the solutions for the current time step
    val assetCtrl = assetVars.map {
      case AssetVarContainer(assetUuid, results) =>
        val setPoint = results
          .map {
            // Taking only the first result for set points
            _.headOption
              .getOrElse(
                throw new CriticalFailureException(
                  s"Empty results for asset $assetUuid"
                )
              )
              // Operating point of first result
              .getOperationResult
          }
          .reduceOption(_ + _)
          .getOrElse(
            throw new CriticalFailureException(
              s"No results present for asset $assetUuid"
            )
          )
        assetUuid -> setPoint
    }

    model.release()

    assetCtrl
  }

  override def adaptFlexOptions(
      assetInput: AssetInput,
      flexOptions: EnergyBoundariesFlexOptions,
  ): EnergyBoundariesFlexOptions = flexOptions

}

object OptimizedFlexStrat {

  /** Threshold for the error of soft constraints after optimization. Every soft
    * constraint error should stay below this threshold.
    */
  private val softConstraintThreshold: Double = 1e-3

  /** Creates and adds constraints for the given flex options for given sample
    * times. States and operating points are linked according to the time steps.
    *
    * @param flexOptions
    *   The flex options that connected assets provided.
    * @param sampleTime
    *   The amount of time between the steps.
    * @param ticks
    *   The ticks (including current tick to last predicted tick) of the time
    *   steps to add constraints for. Should all be sample time duration apart
    *   from each other.
    * @param model
    *   The optimization model to add variables and constraints to.
    * @return
    *   Containers that holds all results, including state and operation
    *   variables.
    */
  def addAssetConstraints(
      flexOptions: Iterable[(UUID, EnergyBoundariesFlexOptions)],
      sampleTime: Time,
      ticks: Seq[Long],
  )(using model: MPModel): Iterable[AssetVarContainer] = {
    flexOptions.map { case (assetUUID, fo) =>
      val results = fo.energyBoundaries
        .map(adaptEnergyBoundaries)
        .map { boundaries =>
          ticks.tail.foldLeft[IndexedSeq[StepResults]](IndexedSeq.empty) {
            case (previousResults, tick) =>
              val previousState = previousResults.lastOption.flatMap(_.state)

              val res = addAssetStep(
                boundaries,
                tick,
                sampleTime,
                previousState,
              )

              previousResults.appended(res)
          }
        }

      AssetVarContainer(assetUUID, results)
    }
  }

  /** Creates and adds constraints for a single asset and time step. Relevant
    * variables and soft constraints are returned in a [[StepResults]]
    * container.
    *
    * @param energyBoundaries
    *   The asset energy boundaries to use.
    * @param tick
    *   The tick to add constraints for.
    * @param sampleTime
    *   The amount of time between the steps.
    * @param maybePreviousState
    *   Optionally, the previous state variable.
    * @param model
    *   The optimization model to add variables and constraints to.
    * @return
    *   The results for this asset and time step.
    */
  private def addAssetStep(
      energyBoundaries: AssetEnergyBoundaries,
      tick: Long,
      sampleTime: Time,
      maybePreviousState: Option[Expression],
  )(using model: MPModel): StepResults = {

    val energyLimits = energyBoundaries.energyLimits
      .maxBefore(tick + 1)
      .map { case (_, limits) =>
        limits
      }
      .getOrElse(throw new CriticalFailureException("No energy limits found!"))

    val formerEnergyLimits =
      energyBoundaries.energyLimits.maxBefore(tick).map { case (_, limits) =>
        limits
      }

    if energyLimits.getLower == energyLimits.getUpper &&
      formerEnergyLimits.forall(limits => limits.getLower == limits.getUpper)
    then {
      // there is no flexibility at all, thus we don't need any state to keep track of

      val formerEnergy = formerEnergyLimits.map(_.getUpper).getOrElse(zeroKWh)
      val currentEnergy = energyLimits.getUpper

      val fixedPower = (currentEnergy - formerEnergy) / sampleTime
      StepResults(Const(fixedPower.toKilowatts), None, None)
    } else {
      // we do have some flexibility at this point in time, model it

      // we use charging efficiency for both charging and discharging,
      // since we use the adapted storage model here
      val eta = energyBoundaries.etaCharge

      // determining a previous state
      val previousState = maybePreviousState.getOrElse {

        // we have been given no former state as a parameter. Either...
        formerEnergyLimits
          // ... there was no flexibility in the last step, thus we use the last energy value
          .filter(limits => limits.getLower == limits.getUpper)
          .map(limits => Const(limits.getUpper.toKilowattHours))
          // ... or this is the initial step, thus we start at 0
          .getOrElse(Const(0d))
      }

      // modeling the operating point (power),
      // valid between that previous and new state
      val p = MPFloatVar(
        symbol = "p",
        lowerBound = energyBoundaries.powerLimits.getLower.toKilowatts,
        upperBound = energyBoundaries.powerLimits.getUpper.toKilowatts,
      )

      // modeling the new state (stored energy)
      val newState =
        if energyLimits.getLower == energyLimits.getUpper then
          Const(energyLimits.getUpper.toKilowattHours)
        else
          MPFloatVar(
            symbol = "state",
            lowerBound = energyLimits.getLower.toKilowattHours,
            upperBound = energyLimits.getUpper.toKilowattHours,
          )

      val softConstraint =
        if eta == onePU then {
          // there are no charging/discharging losses, we can keep it simple

          model.add(newState := previousState + p * sampleTime.toHours)
          None
        } else {
          // there are charging/discharging losses, thus use the full model

          val pAbsMax =
            energyBoundaries.powerLimits.getUpper.max(
              -energyBoundaries.powerLimits.getLower
            )

          val pAbs = MPFloatVar(
            symbol = "pAbs",
            lowerBound = 0,
            upperBound = pAbsMax.toKilowatts,
          )

          model.add(pAbs >:= p)
          model.add(pAbs >:= -p)

          model.add(
            newState := previousState + (p - pAbs * (1 - eta.toEach)) * sampleTime.toHours
          )

          Some(AbsValueSoftConstraint(p, pAbs, eta))
        }

      StepResults(p, Some(newState), softConstraint)
    }

  }

  /** Creates flex options that are equivalent to the original with regard to
    * optimization, but these can be optimized with a linear model. In order to
    * achieve this, a common efficiency needs to be calculated for charging and
    * discharging operations, eliminating the need to distinguish between
    * charging and discharging when formulating the state constraint.
    * Furthermore, energy limits are adapted to work with the adapted
    * efficiency.
    *
    * @param boundaries
    *   The energy boundaries to be adapted.
    * @return
    *   The adapted energy boundaries.
    */
  def adaptEnergyBoundaries(
      boundaries: AssetEnergyBoundaries
  ): AssetEnergyBoundaries = {
    val etaCh = boundaries.etaCharge.toEach
    val etaDis = boundaries.etaDischarge.toEach

    val etaAvg = (2 * etaCh * etaDis) / (1 + etaCh * etaDis)

    val newEnergyLimits = boundaries.energyLimits.map { case (tick, limits) =>
      val newLower = (limits.getLower / etaCh) * etaAvg
      val newUpper = (limits.getUpper / etaCh) * etaAvg

      tick -> ClosedInterval(newLower, newUpper)
    }

    val etaAvgEach = Each(etaAvg)

    boundaries.copy(
      energyLimits = newEnergyLimits,
      etaCharge = etaAvgEach,
      etaDischarge = etaAvgEach,
    )

  }

  /** Builds an objective to minimize given the asset variables and an objective
    * factory.
    *
    * @param assetVars
    *   The asset vars to optimize for.
    * @param target
    *   The target power for each time step.
    * @param powerObjectiveFactory
    *   The factor for the objective to optimize at every time step.
    * @param model
    *   The optimization model to add variables and constraints to.
    * @return
    *   An [[ObjectiveContainer]] holding the objective and soft constraints.
    */
  def buildObjective(
      assetVars: Iterable[AssetVarContainer],
      target: Power,
      powerObjectiveFactory: PowerObjectiveFactory,
  )(using model: MPModel): ObjectiveContainer = {
    // asset vars should all have the same amount of operation vars,
    // since they should have all been created with the same sample time steps
    val timeSteps = assetVars.headOption
      .flatMap(_.results.headOption.map(_.size))
      .getOrElse(0)

    val (objectiveResult, softConstraintsResult) =
      Range(0, timeSteps)
        // first, sort all results by time step
        .map { timeStep =>
          assetVars.flatMap {
            _.results.map(_(timeStep))
          }
        }
        // then, build objective for every time step and combine them
        .foldLeft[(Expression, Seq[SoftConstraint])](Zero, Seq.empty) {
          case ((objective, allConstraints), results) =>
            val difference = results
              .map(_.operation)
              .reduceOption[Expression](_ + _)
              .getOrElse(Zero)
            val powerObjective = powerObjectiveFactory.build(difference, target)

            val constraints =
              results.flatMap(_.softConstraint)
            val constraintsExpression = constraints
              .map(_.getExpression)
              .reduceLeftOption(_ + _)
              .getOrElse(Zero)

            (
              objective + constraintsExpression + powerObjective,
              allConstraints.appendedAll(constraints),
            )
        }

    ObjectiveContainer(objectiveResult, softConstraintsResult)
  }

  /** Container holding all optimization steps (including variables and soft
    * constraints) for one asset.
    *
    * @param assetUuid
    *   The UUID of the asset.
    * @param results
    *   All step results of the asset, including state and operation variables.
    */
  final case class AssetVarContainer(
      assetUuid: UUID,
      results: Seq[IndexedSeq[StepResults]],
  )

  /** Container holding the relevant variables and potentially a soft constraint
    * for a specific asset and optimization time step.
    *
    * @param operation
    *   The operating point between the previous state and [[state]].
    * @param state
    *   Optionally the state that follows the operating point. It holds an
    *   energy value that signify the upwards and downwards change of energy,
    *   relating to the energy potential at the starting tick (which is defined
    *   to be zero).
    * @param softConstraint
    *   Optionally a soft constraint.
    */
  final case class StepResults(
      operation: Const | MPVar,
      state: Option[Expression],
      softConstraint: Option[SoftConstraint],
  ) {

    def getOperationResult: Power = Kilowatts(operation match {
      case const: Const => const.value
      case variable: MPVar =>
        variable.value.getOrElse(
          throw new CriticalFailureException(
            s"No result present for variable $variable"
          )
        )
    })

  }

  /** Container holding the complete objective to minimize and relevant soft
    * constraints.
    *
    * @param objective
    *   The objective, including all soft constraint expressions.
    * @param softConstraints
    *   All soft constraints.
    */
  final case class ObjectiveContainer(
      objective: Expression,
      softConstraints: Iterable[SoftConstraint],
  )

}
