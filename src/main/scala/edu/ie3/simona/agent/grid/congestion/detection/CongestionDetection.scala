/*
 * © 2025. TU Dortmund University,
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
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentConstantData
import edu.ie3.simona.agent.grid.congestion.CongestionManagementMessages.*
import edu.ie3.simona.agent.grid.congestion.detection.DetectionMessages.*
import edu.ie3.simona.agent.grid.congestion.data.{
  AwaitingData,
  CongestionManagementData,
}
import edu.ie3.simona.agent.grid.congestion.Congestions
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.typed.scaladsl.{
  ActorContext,
  Behaviors,
  StashBuffer,
}

trait CongestionDetection {

  /** Method that defines the [[Behavior]] for checking if there are any
    * congestion in the grid.
    * @param stateData
    *   of the actor
    * @param constantData
    *   constant data of the [[GridAgent]]
    * @param buffer
    *   for stashed messages
    * @return
    *   a [[Behavior]]
    */
  private[grid] def checkForCongestion(
      stateData: CongestionManagementData,
      awaitingData: AwaitingData[Congestions],
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = Behaviors.receivePartial {
    case (ctx, StartStep) =>
      // request congestion check if we have inferior grids
      askInferior(
        stateData.inferiorGridRefs,
        CongestionCheckRequest,
        ReceivedCongestions,
        ctx,
      )(using stateData.timeout)

      Behaviors.same

    case (ctx, congestionRequest: CongestionCheckRequest) =>
      answerRequest(
        stateData,
        awaitingData,
        congestionRequest,
        ctx,
      )

    case (ctx, ReceivedCongestions(congestions)) =>
      processReceivedData(
        stateData,
        awaitingData,
        congestions,
        ctx,
      )

    case (ctx, FinishStep) =>
      // inform my inferior grids about the end of the congestion management
      stateData.inferiorGridRefs.keys.foreach(
        _ ! FinishStep
      )

      // directly finish congestion management, since we don't have any steps
      finishCongestionManagement(stateData, ctx)

    case (ctx, msg) =>
      unsupported(msg, ctx.log)
      Behaviors.same
  }

  private def answerRequest(
      stateData: CongestionManagementData,
      awaitingData: AwaitingData[Congestions],
      congestionRequest: CongestionCheckRequest,
      ctx: ActorContext[GridAgent.Request],
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = {
    // check if waiting for inferior data is needed
    if awaitingData.notDone then {
      ctx.log.debug(
        s"Received request for congestions before all data from inferior grids were received. Stashing away."
      )

      // stash away the message, because we need to wait for data from inferior grids
      buffer.stash(congestionRequest)
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
    }

    checkForCongestion(stateData, awaitingData)
  }

  private def processReceivedData(
      stateData: CongestionManagementData,
      awaitingData: AwaitingData[Congestions],
      congestions: Vector[(ActorRef[GridAgent.Request], Congestions)],
      ctx: ActorContext[GridAgent.Request],
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = {
    // updating the state data with received data from inferior grids
    val updatedData = awaitingData.handleReceivingData(congestions)

    if stateData.gridAgentBaseData.isSuperior then {
      // if we are the superior grid, we find the next behavior

      val congestions = stateData.congestions.combine(updatedData.values)

      // checking for any congestion in the complete grid
      if !congestions.hasCongestion then {
        ctx.log.info(
          s"No congestions found. Finishing the congestion management."
        )

        ctx.self ! FinishStep
        checkForCongestion(stateData, updatedData)
      } else {
        ctx.log.debug(
          s"Congestion overall: $congestions"
        )

        val timestamp =
          constantData.simStartTime.plusSeconds(stateData.currentTick)

        ctx.log.info(
          s"There were some congestions that could not be resolved for timestamp: $timestamp."
        )

        ctx.self ! FinishStep
        checkForCongestion(stateData, updatedData)
      }

    } else {
      // un-stash all messages
      buffer.unstashAll(checkForCongestion(stateData, updatedData))
    }
  }

}
