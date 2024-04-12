/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.grid.GridAgent.{idle, pipeToSelf}
import edu.ie3.simona.agent.grid.GridAgentData.CongestionManagementData.Congestions
import edu.ie3.simona.agent.grid.GridAgentData.{
  CongestionManagementData,
  GridAgentBaseData,
  GridAgentConstantData,
}
import edu.ie3.simona.agent.grid.GridAgentMessages._
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import org.apache.pekko.actor.typed.scaladsl.AskPattern.Askable
import org.apache.pekko.actor.typed.scaladsl.{
  ActorContext,
  Behaviors,
  StashBuffer,
}
import org.apache.pekko.actor.typed.{ActorRef, Behavior, Scheduler}
import org.apache.pekko.util.Timeout

import scala.concurrent.duration.SECONDS
import scala.concurrent.{ExecutionContext, Future}

/** Trait that is normally mixed into every [[GridAgent]] to enable distributed
  * congestion management (DCM) algorithm execution. It is considered to be the
  * standard behaviour of a [[GridAgent]].
  */
trait DCMAlgorithm {

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
      stateData: CongestionManagementData
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = Behaviors.receivePartial {

    case (ctx, Check) =>
      // ask all inferior grids for a congestion check
      askInferiorGridsForCongestionCheck(
        stateData.inferiorRefs
      )(ctx)

      Behaviors.same
    case (ctx, congestionRequest @ CongestionCheckRequest(sender)) =>
      // check if waiting for inferior data is needed
      if (stateData.awaitingInferiorData) {
        ctx.log.debug(
          s"Received request for congestions before all data from inferior grids were received. Stashing away."
        )

        // stash away the message, because we need to wait for data from inferior grids
        buffer.stash(congestionRequest)
      } else {
        // check if there are any congestions in the grid
        // sends the results to the superior grid
        sender ! CongestionResponse(
          stateData.congestions.combine(
            stateData.inferiorCongestions.values.flatten
          ),
          ctx.self,
        )
      }

      Behaviors.same
    case (ctx, ReceivedCongestions(congestions)) =>
      // updating the state data with received data from inferior grids
      val updatedStateData = stateData.handleReceivingData(
        congestions.map { msg => msg.sender -> Some(msg.congestions) }.toMap
      )

      if (updatedStateData.gridAgentBaseData.isSuperior) {
        // if we are the superior grid, we find the next behavior

        val congestions = updatedStateData.congestions.combine(
          updatedStateData.inferiorCongestions.values.flatten
        )

        findNextStep(congestions, updatedStateData, ctx)
      } else {
        // un-stash all messages
        buffer.unstashAll(checkForCongestion(updatedStateData))
      }

    case (_, NextStepRequest(next)) =>
      // inform my inferior grids about the next behavior
      stateData.inferiorRefs.foreach(
        _ ! NextStepRequest(next)
      )

      // switching to the next behavior
      next(stateData)

    case (ctx, GotoIdle) =>
      // inform my inferior grids about the end of the congestion management
      stateData.inferiorRefs.foreach(
        _ ! GotoIdle
      )

      // do my cleanup stuff
      ctx.log.debug("Doing my cleanup stuff")

      // / clean copy of the gridAgentBaseData
      val cleanedGridAgentBaseData = GridAgentBaseData.clean(
        stateData.gridAgentBaseData,
        stateData.gridAgentBaseData.superiorGridNodeUuids,
        stateData.gridAgentBaseData.inferiorGridGates,
      )

      // / inform scheduler that we are done with the whole simulation and request new trigger for next time step
      constantData.environmentRefs.scheduler ! Completion(
        constantData.activationAdapter,
        Some(stateData.currentTick + constantData.resolution),
      )

      // return to Idle
      idle(cleanedGridAgentBaseData)
  }

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
  private def updateTransformerTapping(
      stateData: CongestionManagementData
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = Behaviors.receivePartial {
    case (_, StartStep) =>
      Behaviors.same
    case (ctx, FinishStep) =>
      // inform my inferior grids about the end of this step
      stateData.inferiorRefs.foreach(_ ! FinishStep)

      // switching to simulating grid
      val currentTick = stateData.currentTick

      ctx.self ! WrappedActivation(Activation(currentTick))

      val updatedData = stateData.gridAgentBaseData.copy(
        congestionManagementParams =
          stateData.gridAgentBaseData.congestionManagementParams
            .copy(hasRunTransformerTapping = true)
      )

      GridAgent.simulateGrid(updatedData, currentTick)
  }

  /** Triggers an execution of the pekko `ask` pattern for all congestions in
    * inferior grids (if any) of this [[GridAgent]].
    *
    * @param subGridActorRefs
    *   a set of [[ActorRef]]s to all inferior grids
    * @param ctx
    *   actor context
    * @param askTimeout
    *   a timeout for the request
    */
  private def askInferiorGridsForCongestionCheck(
      subGridActorRefs: Set[ActorRef[GridAgent.Request]]
  )(implicit
      ctx: ActorContext[GridAgent.Request],
      askTimeout: Timeout = Timeout(10, SECONDS),
  ): Unit = {

    // request congestion check if we have inferior grids
    if (subGridActorRefs.nonEmpty) {
      implicit val ec: ExecutionContext = ctx.executionContext
      implicit val scheduler: Scheduler = ctx.system.scheduler

      val future = Future
        .sequence(
          subGridActorRefs.map { inferiorGridAgentRef =>
            inferiorGridAgentRef
              .ask(ref => CongestionCheckRequest(ref))
              .map { case response: CongestionResponse => response }
          }.toVector
        )
        .map(res => ReceivedCongestions(res))
      pipeToSelf(future, ctx)
    }
  }

  /** Method to determine the next congestion management step.
    * @param congestions
    *   information if there is any congestion in the grid
    * @param stateData
    *   current state data
    * @param ctx
    *   actor context
    * @param constantData
    *   constant data of the [[GridAgent]]
    * @param buffer
    *   for stashed messages
    * @return
    *   a [[Behavior]]
    */
  private def findNextStep(
      congestions: Congestions,
      stateData: CongestionManagementData,
      ctx: ActorContext[GridAgent.Request],
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = {

    // checking for any congestion in the complete grid
    if (
      !congestions.voltageCongestions && !congestions.lineCongestions && !congestions.transformerCongestions
    ) {
      ctx.log.debug(
        s"No congestions found. Finishing the congestion management."
      )

      ctx.self ! GotoIdle
      checkForCongestion(stateData)
    } else {




      ???
    }
  }

}
