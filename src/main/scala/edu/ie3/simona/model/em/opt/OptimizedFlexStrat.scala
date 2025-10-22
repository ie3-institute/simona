/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.em.EmModelStrat
import edu.ie3.simona.model.em.opt.EnergyLimitFlexModel.StepResults
import edu.ie3.simona.model.em.opt.OptimizedFlexStrat.*
import edu.ie3.simona.ontology.messages.flex.EnergyLimitFlexOptions
import edu.ie3.simona.ontology.messages.flex.MathFlexOptions.{
  OperationVars,
  SoftConstraint,
}
import optimus.algebra.{Double2Const, Expression, Zero}
import optimus.optimization.MPModel
import optimus.optimization.enums.{SolutionStatus, SolverLib}
import org.slf4j.Logger
import squants.{Power, Time}

import java.util.UUID

/** Flex strategy that optimizes over a fixed amount of time steps into the
  * future. Works with [[EnergyLimitFlexOptions]], which describe constraints on
  * present and future behavior of a participant.
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
) extends EmModelStrat[EnergyLimitFlexOptions] {

  override def determineFlexControl(
      flexOptions: Iterable[(? <: AssetInput, EnergyLimitFlexOptions)],
      target: Power,
      currentTick: Long,
  ): Iterable[(UUID, Power)] = {

    given model: MPModel = MPModel(SolverLib.oJSolver)

    val sampleTicks = sampleTime.toSeconds.toLong
    val lastPredictedTick = currentTick + predictionHorizon.toSeconds.toLong

    val ticks = Range.Long(currentTick, lastPredictedTick, sampleTicks)

    val assetVars = flexOptions.map { case (asset: AssetInput, fo) =>
      addAssetConstraints(asset.getUuid, fo, sampleTime, ticks)
    }

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
      flexOptions: EnergyLimitFlexOptions,
  ): EnergyLimitFlexOptions = flexOptions
}

object OptimizedFlexStrat {

  /** Threshold for the error of soft constraints after optimization. Every soft
    * constraint error should stay below this threshold.
    */
  private val softConstraintThreshold: Double = 1e-3

  /** Creates and adds constraints for the given asset for all given sample
    * times. States and operating points are strung together according to the
    * sample times.
    *
    * @param assetUuid
    *   The UUID of the asset.
    * @param flexOptions
    *   The flex options that the asset provided.
    * @param ticks
    *   The ticks of the sample times to add constraints for.
    * @param sampleTime
    * @param model
    *   The optimization model to use.
    * @tparam SV
    *   The type of state variables.
    * @tparam OV
    *   The type of operation variables.
    * @return
    *   A container that holds all state and operation variables.
    */
  def addAssetConstraints[SV, OV <: OperationVars](
      assetUuid: UUID,
      flexOptions: EnergyLimitFlexOptions,
      sampleTime: Time,
      ticks: Seq[Long],
  )(using model: MPModel): AssetVarContainer = {

    val results = flexOptions.energyBoundaries
      .map(EnergyLimitFlexModel.adaptEnergyBoundaries)
      .map { boundaries =>
        ticks.foldLeft[IndexedSeq[StepResults]](IndexedSeq.empty) {
          case (previousResults, tick) =>
            val previousState = previousResults.headOption.flatMap(_.state)

            val res = EnergyLimitFlexModel.addStep(
              boundaries,
              tick,
              sampleTime,
              previousState,
            )

            previousResults.appended(res)
        }
      }

    AssetVarContainer(assetUuid, results)
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
