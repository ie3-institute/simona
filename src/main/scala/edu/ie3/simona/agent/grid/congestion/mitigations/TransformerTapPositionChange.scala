/*
 * © 2025-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion.mitigations

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.GridAgent.{askInferior, unsupported}
import edu.ie3.simona.agent.grid.GridAgentCoordinator.StepFinished
import edu.ie3.simona.agent.grid.congestion.CongestionManagementMessages.*
import edu.ie3.simona.agent.grid.congestion.VoltageRange
import edu.ie3.simona.agent.grid.congestion.mitigations.TappingMessages.*
import edu.ie3.simona.agent.grid.data.CongestionManagementData
import edu.ie3.simona.agent.grid.data.GridAgentData.{
  AwaitingData,
  GridAgentConstantData,
}
import edu.ie3.simona.model.control.TappingGroupModel
import edu.ie3.simona.model.grid.Transformer3wPowerFlowCase.{
  PowerFlowCaseB,
  PowerFlowCaseC,
}
import edu.ie3.simona.model.grid.TransformerTapping
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroPU
import org.apache.pekko.actor.typed.scaladsl.{
  ActorContext,
  Behaviors,
  StashBuffer,
}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import squants.Dimensionless

trait TransformerTapPositionChange {

  /** Method that defines the [[Behavior]] for changing the tapping for
    * transformers.
    *
    * @param stateData
    *   of the actor
    * @param constantData
    *   constant data of the [[GridAgent]]
    * @param buffer
    *   for stashed messages
    * @return
    *   a [[Behavior]]
    */
  private[grid] def updateTransformerTapping(
      stateData: CongestionManagementData,
      awaitingData: AwaitingData[(VoltageRange, Set[TransformerTapping])],
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Message],
  ): Behavior[GridAgent.Message] = Behaviors.receivePartial {
    case (ctx, StartStep) =>
      val subnet = stateData.gridAgentBaseData.gridEnv.gridModel.subnetNo

      // request congestion check if we have inferior grids
      askInferior(
        stateData.inferiorGridRefs,
        (ref, _) => RequestVoltageOptions(ref, subnet),
      )(using ctx)

      Behaviors.same

    case (ctx, voltageOptionRequest: RequestVoltageOptions) =>
      answerRequest(stateData, awaitingData, voltageOptionRequest, ctx)

    case (ctx, VoltageRangeResponse(sender, value)) =>
      processReceivedData(stateData, awaitingData, sender, value, ctx)

    case (ctx, VoltageDeltaResponse(delta)) =>
      handleUpdatedDataFromSuperior(stateData, awaitingData, delta, ctx)

    case (ctx, msg) =>
      unsupported(msg, ctx.log)
      Behaviors.same
  }

  private def answerRequest(
      stateData: CongestionManagementData,
      awaitingData: AwaitingData[(VoltageRange, Set[TransformerTapping])],
      request: RequestVoltageOptions,
      ctx: ActorContext[GridAgent.Message],
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Message],
  ): Behavior[GridAgent.Message] = {
    val sender = request.sender
    val subgrid = request.subgrid

    // check if waiting for inferior data is needed
    if awaitingData.nonComplete then {
      ctx.log.debug(
        s"Received request for voltage range before all data from inferior grids were received. Stashing away."
      )

      // stash away the message, because we need to wait for data from inferior grids
      buffer.stash(request)
    } else {
      // calculate the voltage range for this grid
      val gridEnv = stateData.gridAgentBaseData.gridEnv
      val gridModel = gridEnv.gridModel
      val gridComponents = gridModel.gridComponents

      // filter all transformers that are connecting this grid to the superior grid
      val nodesInSuperiorGrid =
        gridComponents.nodes.filter(_.subnet == subgrid).map(_.uuid)
      val transformers = gridComponents.transformers.filter(t =>
        nodesInSuperiorGrid.contains(t.hvNodeUuid)
      )
      val transformers3w = gridComponents.transformers3w.filter { t =>
        t.powerFlowCase match {
          case PowerFlowCaseB =>
            nodesInSuperiorGrid.contains(t.hvNodeUuid)
          case PowerFlowCaseC =>
            nodesInSuperiorGrid.contains(t.hvNodeUuid)
        }
      }

      val allTransformers: Set[TransformerTapping] =
        transformers ++ transformers3w

      // calculate the voltage range with the received data
      val range = VoltageRange(
        stateData.powerFlowResults,
        gridModel.voltageLimits,
        gridModel.gridComponents,
        awaitingData.receivedData,
        gridModel.subnetNo,
      )

      ctx.log.debug(
        s"For Grid ${stateData.gridAgentBaseData.gridEnv.gridModel.subnetNo}, voltage range: $range"
      )

      sender ! VoltageRangeResponse(
        ctx.self,
        (range, allTransformers),
      )
    }

    updateTransformerTapping(stateData, awaitingData)
  }

  private def processReceivedData(
      stateData: CongestionManagementData,
      awaitingData: AwaitingData[(VoltageRange, Set[TransformerTapping])],
      sender: ActorRef[GridAgent.Message],
      value: (VoltageRange, Set[TransformerTapping]),
      ctx: ActorContext[GridAgent.Message],
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Message],
  ): Behavior[GridAgent.Message] = {
    // updating the state data with received data from inferior grids
    val updatedData = awaitingData.addData(sender, value)

    if stateData.gridAgentBaseData.isSuperior then {
      // there should be no voltage change in the superior grid,
      // because the slack grid should always have 1 pu

      ctx.self ! VoltageDeltaResponse(zeroPU)
      updateTransformerTapping(stateData, updatedData)
    } else {
      // un-stash all messages
      buffer.unstashAll(updateTransformerTapping(stateData, updatedData))
    }
  }

  private def handleUpdatedDataFromSuperior(
      stateData: CongestionManagementData,
      awaitingData: AwaitingData[(VoltageRange, Set[TransformerTapping])],
      delta: Dimensionless,
      ctx: ActorContext[GridAgent.Message],
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Message],
  ): Behavior[GridAgent.Message] = {
    // if we are the superior grid to another grid, we check for transformer tapping option
    // and send the new delta to the inferior grid
    ctx.log.warn(
      s"Grid ${stateData.gridAgentBaseData.gridEnv.gridModel.subnetNo}, received delta: $delta"
    )

    if stateData.inferiorGridRefs.nonEmpty then {
      // we calculate a voltage delta for all inferior grids
      val receivedData = awaitingData.receivedData

      // map the actor ref to the possible voltage range
      val refMap = receivedData.map { case (ref, (range, _)) =>
        ref -> range
      }

      val actorRefToTappingModels
          : Map[ActorRef[GridAgent.Message], Set[TransformerTapping]] =
        receivedData.map { case (ref, (_, tappings)) => ref -> tappings }

      // groups all tapping models
      // necessary, because to make sure the tapping is change by the same value between two grids,
      // we need to know all transformers that are relevant as well as all actor refs to check their
      // possible voltage ranges

      TappingGroupModel
        .buildModels(
          actorRefToTappingModels,
          stateData.gridAgentBaseData.gridEnv.gridModel.gridComponents.transformers3w,
        )
        .foreach { group =>
          val deltaV = group.updateTapPositions(delta, refMap)
          group.refs.foreach(_ ! VoltageDeltaResponse(deltaV + delta))
        }
    }

    constantData.gridAgentCoordinator ! StepFinished(ctx.self)
    GridAgent.waitForNextStep(stateData)
  }

}
