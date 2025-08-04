/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.em.OptimizedFlexStrat.{
  AssetVarContainer,
  addConstraints,
}
import edu.ie3.simona.ontology.messages.flex.MathProgrammingFlexOptions
import edu.ie3.simona.ontology.messages.flex.MathProgrammingFlexOptions.OperationVars
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
) extends EmModelStrat[MathProgrammingFlexOptions[?, ?]] {

  override def determineFlexControl(
      flexOptions: Iterable[
        (? <: AssetInput, MathProgrammingFlexOptions[?, ?])
      ],
      target: Power,
  ): Iterable[(UUID, Power)] = {

    implicit val model: MPModel = MPModel(SolverLib.oJSolver)

    val timeSteps = predictionHorizon.divide(stepResolution).toInt

    val allAssetVars = flexOptions.map { case (asset: AssetInput, fo) =>
      addConstraints(asset.getUuid, fo, timeSteps, stepResolution)
    }

    val objective = Range(0, timeSteps)
      .map { timeStep =>
        allAssetVars.map {
          _.operationVars(timeStep)
        }
      }
      .foldLeft[Expression](Zero) { case (sum, opVars) =>
        val difference = opVars.foldLeft[Expression](Zero) { case (all, op) =>
          sum + op.getPowerExpression
        } - target.toKilowatts

        val softConstraints =
          opVars.flatMap(_.getSoftConstraints).reduceLeft(_ + _)

        val d = MPFloatVar(0, Double.PositiveInfinity)
        model.add(d >:= difference)
        model.add(d >:= -difference)

        sum + d + softConstraints
      }

    model.minimize(objective)

    model.start()

    if (model.getStatus != SolutionStatus.OPTIMAL)
      throw new CriticalFailureException(
        s"Optimization ended with unexpected status ${model.getStatus}, ${SolutionStatus.OPTIMAL} was expected."
      )

    val assetCtrl = allAssetVars.map {
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
      flexOptions: MathProgrammingFlexOptions[?, ?],
  ): MathProgrammingFlexOptions[?, ?] = flexOptions
}

object OptimizedFlexStrat {

  final case class AssetVarContainer[SV, OV <: OperationVars](
      assetUuid: UUID,
      states: IndexedSeq[SV],
      operationVars: IndexedSeq[OV],
  )

  def addConstraints[SV, OV <: OperationVars](
      assetUuid: UUID,
      flexOptions: MathProgrammingFlexOptions[SV, OV],
      timeSteps: Int,
      stepResolution: Time,
  )(using model: MPModel): AssetVarContainer[SV, OV] = {
    val state0 = flexOptions.addInitialState

    val (allStates, allOperationVars) =
      Range(0, timeSteps).foldLeft(IndexedSeq(state0), IndexedSeq.empty[OV]) {
        case ((states, operationVars), step) =>
          val addOp = flexOptions.addOperationConstraints(states.last)
          val addState =
            flexOptions.addNewStateConstraints(
              states.last,
              addOp,
              stepResolution,
            )

          (states.appended(addState), operationVars.appended(addOp))
      }

    AssetVarContainer(assetUuid, allStates, allOperationVars)
  }

}
