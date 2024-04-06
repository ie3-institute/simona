/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.grid.CongestionManagementData.Congestions
import edu.ie3.simona.agent.grid.GridAgent.pipeToSelf
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentConstantData
import edu.ie3.simona.agent.grid.GridAgentMessage.{
  InternalMessage,
  WrappedActivation,
}
import edu.ie3.simona.ontology.messages.Activation
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
  case object Check extends InternalMessage
  case class CongestionRequest(sender: ActorRef[GridAgentMessage])
      extends InternalMessage
  case class CongestionResponse(
      congestions: Congestions,
      sender: ActorRef[GridAgentMessage],
  ) extends InternalMessage

  case class ReceivedCongestions(congestions: Vector[CongestionResponse])
      extends InternalMessage

  case class NextStepRequest(
      next: (CongestionManagementData, Int) => Behavior[GridAgentMessage]
  ) extends InternalMessage

  case object StartStep extends InternalMessage
  case object FinishStep extends InternalMessage

  case object FinishCongestionManagement extends InternalMessage

  /** Method that defines the [[Behavior]] for checking if there are any
    * congestion in the grid.
    * @param stateData
    *   of the actor
    * @param step
    *   the number of the next congestion management step
    * @param constantData
    *   constant data of the [[GridAgent]]
    * @param buffer
    *   for stashed messages
    * @return
    *   a [[Behavior]]
    */
  def checkForCongestion(
      stateData: CongestionManagementData,
      step: Int,
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgentMessage],
  ): Behavior[GridAgentMessage] = Behaviors.receivePartial {

    case (ctx, Check) =>
      // ask all inferior grids for a congestion check
      askInferiorGridsForCongestionCheck(
        stateData.inferiorRefs
      )(ctx)

      checkForCongestion(stateData, step)
    case (ctx, congestionRequest @ CongestionRequest(sender)) =>
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

        findNextStep(congestions, updatedStateData, step, ctx)
      } else {
        // un-stash all messages
        buffer.unstashAll(checkForCongestion(updatedStateData, step))
      }

    case (_, NextStepRequest(next)) =>
      // inform my inferior grids about the next behavior
      stateData.inferiorRefs.foreach(
        _ ! NextStepRequest(next)
      )

      // switching to the next behavior
      next.apply(stateData, step)

    case (ctx, FinishCongestionManagement) =>
      // inform my inferior grids about the end of the congestion management
      stateData.inferiorRefs.foreach(
        _ ! FinishCongestionManagement
      )

      // switching to simulating grid
      val currentTick = stateData.currentTick

      ctx.self ! WrappedActivation(Activation(currentTick))
      GridAgent.simulateGrid(stateData.gridAgentBaseData, currentTick)
  }

  /** Method that defines the [[Behavior]] for changing the tapping for
    * transformers.
    *
    * @param stateData
    *   of the actor
    * @param step
    *   the number of the current congestion management step
    * @param constantData
    *   constant data of the [[GridAgent]]
    * @param buffer
    *   for stashed messages
    * @return
    *   a [[Behavior]]
    */
  private def updateTransformerTapping(
      stateData: CongestionManagementData,
      step: Int,
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgentMessage],
  ): Behavior[GridAgentMessage] = Behaviors.receivePartial {
    case (ctx, StartStep) =>
      Behaviors.same
    case (ctx, FinishStep) =>
      // inform my inferior grids about the end of this step
      stateData.inferiorRefs.foreach(_ ! FinishStep)

      ctx.self ! Check
      checkForCongestion(stateData, step + 1)
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
      subGridActorRefs: Set[ActorRef[GridAgentMessage]]
  )(implicit
      ctx: ActorContext[GridAgentMessage],
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
              .ask(ref => CongestionRequest(ref))
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
    * @param step
    *   the number of the next step
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
      step: Int,
      ctx: ActorContext[GridAgentMessage],
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgentMessage],
  ): Behavior[GridAgentMessage] = {

    // checking for any congestion in the complete grid
    if (
      !congestions.voltageCongestions && !congestions.lineCongestions && !congestions.transformerCongestions
    ) {
      ctx.log.debug(
        s"No congestions found. Finishing the congestion management."
      )

      ctx.self ! FinishCongestionManagement
      checkForCongestion(stateData, step)
    } else {
      step match {
        case 0 =>
          ctx.self ! NextStepRequest(updateTransformerTapping)
          checkForCongestion(stateData, step)

        // TODO: Add more congestion management steps

        case _ =>
          ctx.self ! FinishCongestionManagement
          checkForCongestion(stateData, step)
      }
    }
  }

}
