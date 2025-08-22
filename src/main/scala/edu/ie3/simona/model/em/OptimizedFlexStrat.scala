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
import edu.ie3.simona.ontology.messages.flex.MathFlexOptions.OperationVars
import optimus.algebra.{Double2Const, Expression, Zero}
import optimus.optimization.MPModel
import optimus.optimization.enums.{SolutionStatus, SolverLib}
import optimus.optimization.model.MPFloatVar
import squants.{Power, Time}

import java.util.UUID

// todo properly catching, tagging and throwing failures in modelshell
final case class OptimizedFlexStrat(
    stepResolution: Time,
    predictionHorizon: Time,
    powerObjective: PowerObjectiveFactory,
) extends EmModelStrat[MathFlexOptions[?, ? <: OperationVars]] {

  override def determineFlexControl(
      flexOptions: Iterable[
        (? <: AssetInput, MathFlexOptions[?, ? <: OperationVars])
      ],
      target: Power,
  ): Iterable[(UUID, Power)] = {

    implicit val model: MPModel = MPModel(SolverLib.oJSolver)

    val currentTick: Long = ???
    val tickResolution = stepResolution.toSeconds.toLong
    val lastTick = currentTick + predictionHorizon.toSeconds.toLong

    val ticks = Range.Long(currentTick, lastTick, tickResolution)

    val assetVars = flexOptions.map { case (asset: AssetInput, fo) =>
      addAssetConstraints(asset.getUuid, fo, ticks)
    }

    val objective =
      buildObjective(assetVars, target, stepResolution, powerObjective)

    model.minimize(objective)

    model.start()

    if (model.getStatus != SolutionStatus.OPTIMAL)
      throw new CriticalFailureException(
        s"Optimization ended with unexpected status ${model.getStatus}, ${SolutionStatus.OPTIMAL} was expected."
      )

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

  final case class AssetVarContainer[SV, OV <: OperationVars](
      assetUuid: UUID,
      states: IndexedSeq[SV],
      operationVars: IndexedSeq[OV],
  )

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

  def buildObjective(
      assetVars: Iterable[AssetVarContainer[?, ? <: OperationVars]],
      target: Power,
      stepResolution: Time,
      powerObjectiveBuilder: PowerObjectiveFactory,
  )(using model: MPModel): Expression = {
    // asset vars should all have the same amount of operation vars
    val timeSteps = assetVars.headOption.map(_.operationVars.size).getOrElse(0)

    Range(0, timeSteps)
      .map { timeStep =>
        assetVars.map {
          _.operationVars(timeStep)
        }
      }
      .foldLeft[Expression](Zero) { case (objective, opVars) =>
        val difference = opVars.foldLeft[Expression](Zero) {
          case (powers, op) =>
            powers + op.getPowerExpression
        } - target.toKilowatts

        val softConstraints =
          opVars.flatMap(_.getSoftConstraints(stepResolution)).reduceLeft(_ + _)

        val powerObjective = powerObjectiveBuilder.build(difference)

        objective + softConstraints + powerObjective
      }
  }

  trait PowerObjectiveFactory {
    def build(power: Expression)(using model: MPModel): Expression
  }

  object MinAbsPowerObjectiveFactory extends PowerObjectiveFactory {

    override def build(
        power: Expression
    )(using model: MPModel): Expression = {
      val d = MPFloatVar.positive("d")
      model.add(d >:= power)
      model.add(d >:= -power)

      d
    }

  }

  class LinearizedQuadraticPowerObjectiveFactory(
      stepCount: Int,
      lastStep: Double,
  ) extends PowerObjectiveFactory {

    override def build(
        power: Expression
    )(using model: MPModel): Expression = {

      val powerAbs = MPFloatVar.positive("powerAbs")
      model.add(powerAbs >:= power)
      model.add(powerAbs >:= -power)

      val stepSize = lastStep / stepCount

      val t = MPFloatVar.positive("t")

      val normalizeDivisor = lastStep * lastStep

      Range.inclusive(0, stepCount).map(_ * stepSize).sliding(2).foreach {
        case Seq(uCurrent, uNext) =>
          val m = uCurrent + uNext
          val b = -uCurrent * uNext

          model.add(t >:= m * powerAbs + b)
      }

      t * (1 / lastStep)
    }

  }

}
