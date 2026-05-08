/*
 * © 2025-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion.detection

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.GridAgent.{
  askInferior,
  finishCongestionManagement,
  unsupported,
}
import edu.ie3.simona.agent.grid.GridAgentCoordinator.CongestionResult
import edu.ie3.simona.agent.grid.congestion.CongestionManagementMessages.*
import edu.ie3.simona.agent.grid.congestion.Congestions
import edu.ie3.simona.agent.grid.congestion.detection.DetectionMessages.*
import edu.ie3.simona.agent.grid.data.CongestionManagementData
import edu.ie3.simona.agent.grid.data.GridAgentData.{
  AwaitingData,
  GridAgentConstantData,
}
import org.apache.pekko.actor.typed.scaladsl.{
  ActorContext,
  Behaviors,
  StashBuffer,
}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

trait CongestionDetection {

  /** Method that defines the [[Behavior]] for checking if there are any
    * congestion in the grid.
    * @param stateData
    *   Of the actor.
    * @param constantData
    *   Constant data of the [[GridAgent]].
    * @param buffer
    *   For stashed messages.
    * @return
    *   A [[Behavior]]
    */
  private[grid] def checkForCongestion(
      stateData: CongestionManagementData,
      awaitingData: AwaitingData[Congestions],
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Message],
  ): Behavior[GridAgent.Message] = Behaviors.receivePartial {
    case (ctx, StartStep) =>
      // request congestion check if we have inferior grids
      askInferior(
        stateData.inferiorGridRefs,
        (ref, _) => CongestionCheckRequest(ref),
      )(using ctx)

      Behaviors.same

    case (ctx, congestionRequest: CongestionCheckRequest) =>
      answerRequest(
        stateData,
        awaitingData,
        congestionRequest,
        ctx,
      )

    case (ctx, response: CongestionResponse) =>
      processReceivedData(stateData, awaitingData, response, ctx)

    case (ctx, msg) =>
      unsupported(msg, ctx.log)
      Behaviors.same
  }

  private def answerRequest(
      stateData: CongestionManagementData,
      awaitingData: AwaitingData[Congestions],
      congestionRequest: CongestionCheckRequest,
      ctx: ActorContext[GridAgent.Message],
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Message],
  ): Behavior[GridAgent.Message] = {
    // check if waiting for inferior data is needed
    if awaitingData.nonComplete then {
      ctx.log.debug(
        s"Received request for congestions before all data from inferior grids were received. Stashing away."
      )

      // stash away the message, because we need to wait for data from inferior grids
      buffer.stash(congestionRequest)
      checkForCongestion(stateData, awaitingData)

    } else {
      // check if there are any congestions in the grid
      val congestions = stateData.congestions

      if congestions.hasCongestion then {
        ctx.log.info(
          s"In the grid ${stateData.subgridNo}, the following congestions were found: $congestions"
        )
      }

      // sends the results to the superior grid
      congestionRequest.sender ! CongestionResponse(
        ctx.self,
        congestions.combine(awaitingData.values),
      )

      // wait for the next step, since the detection is completed
      GridAgent.waitForNextStep(stateData)
    }
  }

  private def processReceivedData(
      stateData: CongestionManagementData,
      awaitingData: AwaitingData[Congestions],
      response: CongestionResponse,
      ctx: ActorContext[GridAgent.Message],
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Message],
  ): Behavior[GridAgent.Message] = {
    // updating the state data with received data from inferior grids
    val updatedData = awaitingData.addData(response.sender, response.value)

    if stateData.gridAgentBaseData.isSuperior && updatedData.isComplete then {
      val updatedCongestions = stateData.congestions.combine(updatedData.values)

      constantData.gridAgentCoordinator ! CongestionResult(
        ctx.self,
        updatedCongestions,
      )

      GridAgent.waitForNextStep(stateData)
    } else {
      // un-stash all messages
      buffer.unstashAll(checkForCongestion(stateData, updatedData))
    }
  }

}
