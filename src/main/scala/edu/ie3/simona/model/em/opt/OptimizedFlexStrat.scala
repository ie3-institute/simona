/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.em.EmModelStrat
import edu.ie3.util.interval.ClosedInterval
import edu.ie3.simona.model.em.opt.OptimizedFlexStrat.*
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.ParticipantEnergyBoundaries
import edu.ie3.simona.ontology.messages.flex.MathFlexOptions.{
  OperationVars,
  SoftConstraint,
}
import optimus.algebra.{Const, Double2Const, Expression, Zero}
import optimus.optimization.MPModel
import optimus.optimization.enums.{SolutionStatus, SolverLib}
import optimus.optimization.model.{MPFloatVar, MPVar}
import org.slf4j.Logger
import squants.energy.Kilowatts
import squants.{Each, Power, Time}

import java.util.UUID

/** Flex strategy that optimizes over a fixed amount of time steps into the
  * future. Works with [[EnergyBoundariesFlexOptions]], which describe
  * constraints on present and future behavior of a participant.
  *
  * @param sampleTime
  *   The amount of time between the steps.
  * @param predictionHorizon
  *   The amount of time that is predicted into the future, i.e. the last step
  *   is this amount of time away from now.
  * @param powerObjectiveFactory
  *   The objective to optimize for.
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

    val ticks = Range.Long(currentTick, lastPredictedTick, sampleTicks)

    val assetVars = addAssetConstraints(
      flexOptions.map { case (asset: AssetInput, fo) => asset.getUuid -> fo },
      sampleTime,
      ticks,
    )

    val objectiveContainer =
      buildObjective(assetVars, target, sampleTime, powerObjectiveFactory)

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
            _.headOption
              .getOrElse(
                throw new CriticalFailureException(
                  s"Empty results for asset $assetUuid"
                )
              )
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

  /** Creates and adds constraints for the given assets for all given sample
    * times. States and operating points are strung together according to the
    * sample times.
    *
    * @param flexOptions
    *   The flex options that all assets provided.
    * @param ticks
    *   The ticks of the sample times to add constraints for.
    * @param sampleTime
    * @param model
    *   The optimization model to use.
    * @return
    *   A container that holds all state and operation variables.
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
          ticks.foldLeft[IndexedSeq[StepResults]](IndexedSeq.empty) {
            case (previousResults, tick) =>
              val previousState = previousResults.headOption.flatMap(_.state)

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

  private def addAssetStep(
      energyBoundaries: ParticipantEnergyBoundaries,
      tick: Long,
      duration: Time,
      lastState: Option[Expression],
  )(using model: MPModel): StepResults = {

    val energyLimits = energyBoundaries.energyLimits
      .maxBefore(tick + 1)
      .map { case (_, limits) =>
        limits
      }
      .getOrElse(throw new CriticalFailureException("No energy limits found!"))

    if energyLimits.getUpper == energyLimits.getLower then {
      // there is no flexibility at all, thus we don't need any state to keep track of

      val fixedPower = energyLimits.getUpper / duration
      StepResults(None, Const(fixedPower.toKilowatts), None)
    } else {
      // we do have some flexibility at this point in time, model it

      // we use charging efficiency for both charging and discharging,
      // since we use the adapted storage model here
      val eta = energyBoundaries.etaCharge

      // determining a previous state
      val previousState = lastState.getOrElse {

        val formerEnergyLimits =
          energyBoundaries.energyLimits.maxBefore(tick).map {
            case (_, limits) => limits
          }

        // we have been given no former state as a parameter. Either...
        formerEnergyLimits
          .map { limits =>
            if limits.getLower == limits.getUpper then
              // ... there was no flexibility in the last step, thus we use the last energy value
              Const(limits.getUpper.toKilowattHours)
            else
              throw new CriticalFailureException(
                "No former state was given, although there was flexibility in the last step"
              )
          }
          // ... or this is the initial step, thus we start at 0
          .getOrElse(Const(0d))
      }

      // modeling the operating point (power),
      // valid between that previous and new state
      val p = MPFloatVar(
        "p",
        energyBoundaries.powerLimits.getLower.toKilowatts,
        energyBoundaries.powerLimits.getUpper.toKilowatts,
      )

      // modeling the new state (stored energy)
      val newState = MPFloatVar(
        "state",
        energyLimits.getLower.toKilowattHours,
        energyLimits.getUpper.toKilowattHours,
      )

      val softConstraint =
        if eta == Each(1) then {
          // there are no charging/discharging losses, we can keep it simple

          model.add(newState := previousState + p * duration.toHours)
          None
        } else {
          // there are charging/discharging losses, thus use the full model

          val pAbsMax = energyBoundaries.powerLimits.getUpper.max(
            -energyBoundaries.powerLimits.getLower
          )

          val pAbs =
            MPFloatVar(
              "pAbs",
              0,
              pAbsMax.toKilowatts,
            )

          model.add(pAbs >:= p)
          model.add(pAbs >:= -p)

          model.add(
            newState := previousState + (p - pAbs * (1 - eta.toEach)) * duration.toHours
          )

          Some(new SoftConstraint {

            override def getExpression: Expression = {
              // Total penalty is slightly larger than the losses
              // calculated by StorageMathFlexOptions. Thus, the
              // value of pAbs should be pushed down to the absolute
              // of p.
              val epsilon = 1e-6
              pAbs * (1 - eta.toEach + epsilon) * duration.toHours
            }

            override def getError: Double = {
              val (pValue, pAbsValue) = getVals
              math.abs(math.abs(pValue) - pAbsValue)
            }

            override def getWarningMessage: String = {
              val (pValue, pAbsValue) = getVals
              s"Soft constraint for storage: Approximated absolute power value $pAbsValue and absolute power value |$pValue| are $getError apart."
            }

            private def getVals: (Double, Double) = p.value
              .zip(pAbs.value)
              .getOrElse(
                throw new CriticalFailureException(
                  "Solution are expected to be determined at this point!"
                )
              )

          })
        }

      StepResults(Some(newState), p, softConstraint)
    }

  }

  def adaptEnergyBoundaries(
      boundaries: ParticipantEnergyBoundaries
  ) = {
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

  final case class StepResults(
      state: Option[MPVar],
      operation: Const | MPVar,
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

  /** Builds an objective to minimize given the asset variables and an objective
    * factory.
    *
    * @param assetVars
    *   The asset vars to optimize for.
    * @param target
    *   The target power for each time step.
    * @param sampleTime
    *   The sample time.
    * @param powerObjectiveFactory
    *   The factor for the objective to optimize at every time step.
    * @param model
    *   The optimization model to use.
    * @return
    *   An [[ObjectiveContainer]] holding the objective and soft constraints.
    */
  def buildObjective(
      assetVars: Iterable[AssetVarContainer],
      target: Power,
      sampleTime: Time,
      powerObjectiveFactory: PowerObjectiveFactory,
  )(using model: MPModel): ObjectiveContainer = {
    // asset vars should all have the same amount of operation vars,
    // since they should have all been created with the same sample time steps
    val timeSteps = assetVars.headOption
      .flatMap(_.results.headOption.map(_.size))
      .getOrElse(0)

    val (objectiveResult, softConstraintsResult) =
      Range(0, timeSteps)
        .flatMap { timeStep =>
          assetVars.map {
            _.results.map(_(timeStep))
          }
        }
        .foldLeft[(Expression, Seq[SoftConstraint])](Zero, Seq.empty) {
          case ((objective, allConstraints), results) =>
            val difference = results.foldLeft[Expression](Zero) {
              case (powers, res) =>
                powers + res.operation
            } - target.toKilowatts

            val constraints =
              results.flatMap(_.softConstraint)
            val constraintsExpression = constraints
              .map(_.getExpression)
              .reduceLeftOption(_ + _)
              .getOrElse(Zero)

            val powerObjective = powerObjectiveFactory.build(difference)

            (
              objective + constraintsExpression + powerObjective,
              allConstraints.appendedAll(constraints),
            )
        }

    ObjectiveContainer(objectiveResult, softConstraintsResult)
  }

  /** Container holding all variables for one asset.
    *
    * @param assetUuid
    *   The UUID of the asset.
    * @param states
    *   All state variables of the asset.
    * @param operationVars
    *   The operation variables of the asset.
    */
  final case class AssetVarContainer(
      assetUuid: UUID,
      results: Seq[IndexedSeq[StepResults]],
  )

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
