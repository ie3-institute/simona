/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.em.OptimizedFlexStrat.*
import edu.ie3.simona.ontology.messages.flex.MathFlexOptions
import edu.ie3.simona.ontology.messages.flex.MathFlexOptions.{
  OperationVars,
  SoftConstraint,
}
import optimus.algebra.{Double2Const, Expression, Zero}
import optimus.optimization.MPModel
import optimus.optimization.enums.{SolutionStatus, SolverLib}
import optimus.optimization.model.MPFloatVar
import org.slf4j.Logger
import squants.{Power, Time}

import java.util.UUID

/** Flex strategy that optimizes over a fixed amount of time steps into the
  * future. Works with [[MathFlexOptions]], which describe constraints on
  * present and future behavior of a participant.
  *
  * @param sampleTime
  *   The amount of time in between steps.
  * @param predictionHorizon
  *   The amount of time that is considered in the future, i.e. the last step is
  *   this amount of time away from now.
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
) extends EmModelStrat[MathFlexOptions[?, ? <: OperationVars]] {

  override def determineFlexControl(
      flexOptions: Iterable[
        (? <: AssetInput, MathFlexOptions[?, ? <: OperationVars])
      ],
      target: Power,
      currentTick: Long,
  ): Iterable[(UUID, Power)] = {

    given model: MPModel = MPModel(SolverLib.oJSolver)

    val sampleTicks = sampleTime.toSeconds.toLong
    val lastPredictedTick = currentTick + predictionHorizon.toSeconds.toLong

    val ticks = Range.Long(currentTick, lastPredictedTick, sampleTicks)

    val assetVars = flexOptions.map { case (asset: AssetInput, fo) =>
      addAssetConstraints(asset.getUuid, fo, ticks)
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
      case AssetVarContainer(assetUuid, _, operationVars) =>
        val firstOp = operationVars(0)
        assetUuid -> firstOp.getPowerSolution.getOrElse(
          throw new CriticalFailureException(
            s"No solution present for operation variables ${firstOp.getPowerExpression}"
          )
        )
    }

    model.release()

    assetCtrl
  }

  override def adaptFlexOptions(
      assetInput: AssetInput,
      flexOptions: MathFlexOptions[?, ? <: OperationVars],
  ): MathFlexOptions[?, ? <: OperationVars] = flexOptions
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
      flexOptions: MathFlexOptions[SV, OV],
      ticks: Seq[Long],
  )(using model: MPModel): AssetVarContainer[SV, OV] = {

    val firstTick = ticks.headOption.getOrElse(
      throw new CriticalFailureException(
        "No ticks to add constraints for were given."
      )
    )
    val otherTicks = ticks.tail

    val initialState = flexOptions.addInitialState(firstTick)

    val (allStates, allOperationVars) =
      otherTicks.foldLeft(IndexedSeq(initialState), IndexedSeq.empty[OV]) {
        case ((states, operationVars), tick) =>
          val addOp = flexOptions.addOperationConstraints(states.last)
          val addState =
            flexOptions.addNewStateConstraints(
              states.last,
              addOp,
              tick,
            )

          (states.appended(addState), operationVars.appended(addOp))
      }

    AssetVarContainer(assetUuid, allStates, allOperationVars)
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
      assetVars: Iterable[AssetVarContainer[?, ? <: OperationVars]],
      target: Power,
      sampleTime: Time,
      powerObjectiveFactory: PowerObjectiveFactory,
  )(using model: MPModel): ObjectiveContainer = {
    // asset vars should all have the same amount of operation vars,
    // since they should have all been created with the same sample time steps
    val timeSteps = assetVars.headOption.map(_.operationVars.size).getOrElse(0)

    val (objectiveResult, softConstraintsResult) =
      Range(0, timeSteps)
        .map { timeStep =>
          assetVars.map {
            _.operationVars(timeStep)
          }
        }
        .foldLeft[(Expression, Seq[SoftConstraint])](Zero, Seq.empty) {
          case ((objective, allConstraints), opVars) =>
            val difference = opVars.foldLeft[Expression](Zero) {
              case (powers, op) =>
                powers + op.getPowerExpression
            } - target.toKilowatts

            val constraints =
              opVars.flatMap(_.getSoftConstraint(sampleTime))
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

  /** Trait for factories of power objectives. An objective is created for the
    * sum of power of a single time step.
    */
  trait PowerObjectiveFactory {

    /** Creates an objective for a single time step involving the sum of power.
      *
      * @param totalPower
      *   The sum of power of all assets for a time step.
      * @param model
      *   The optimization model to use.
      * @return
      *   The objective as an expression.
      */
    def build(totalPower: Expression)(using model: MPModel): Expression
  }

  /** Creates an objective that simply minimizes the absolute value of the sum
    * of power by using an epigraph constraint.
    */
  object MinAbsPowerObjectiveFactory extends PowerObjectiveFactory {

    override def build(
        totalPower: Expression
    )(using model: MPModel): Expression = {
      val d = MPFloatVar.positive("d")
      model.add(d >:= totalPower)
      model.add(d >:= -totalPower)

      d
    }

  }

  /** Creates an objective that uses a piecewise-linear (over-)approximation of
    * the quadratic function on the sum of power. The convex epigraph is used to
    * derive a linear constraint. Effectively, higher power values are punished
    * more than lower ones.
    *
    * The piecewise approximation is created with a fixed number of segments
    * (secant lines) up until given last segment.
    *
    * @param segmentCount
    *   The number of segments (secant lines) to create. A high number of
    *   segments might impact efficiency.
    * @param lastSegment
    *   The value of the last segment boundary. This should be set close to the
    *   maximum value that is to be expected, otherwise the approximation
    *   becomes inaccurate.
    */
  class LinearizedQuadraticPowerObjectiveFactory(
      segmentCount: Int,
      lastSegment: Double,
  ) extends PowerObjectiveFactory {

    override def build(
        totalPower: Expression
    )(using model: MPModel): Expression = {

      val powerAbs = MPFloatVar.positive("powerAbs")
      model.add(powerAbs >:= totalPower)
      model.add(powerAbs >:= -totalPower)

      val segmentSize = lastSegment / segmentCount

      val t = MPFloatVar.positive("t")

      Range.inclusive(0, segmentCount).map(_ * segmentSize).sliding(2).foreach {
        case Seq(uCurrent, uNext) =>
          val m = uCurrent + uNext
          val b = -uCurrent * uNext

          model.add(t >:= m * powerAbs + b)
      }

      // normalize the final value so that it maximizes
      // somewhat close to the absolute value
      val normalizationFactor = 1 / lastSegment

      t * normalizationFactor
    }

  }

  /** Container holding all variables for one asset.
    *
    * @param assetUuid
    *   The UUID of the asset.
    * @param states
    *   All state variables of the asset.
    * @param operationVars
    *   The operation variables of the asset.
    * @tparam SV
    *   The type of state variables.
    * @tparam OV
    *   The type of operation variables.
    */
  final case class AssetVarContainer[SV, OV <: OperationVars](
      assetUuid: UUID,
      states: IndexedSeq[SV],
      operationVars: IndexedSeq[OV],
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
