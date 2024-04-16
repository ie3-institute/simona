/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.grid.CongestionManagementParams.CongestionManagementSteps._
import edu.ie3.simona.agent.grid.GridAgent.pipeToSelf
import edu.ie3.simona.agent.grid.GridAgentData.{
  CongestionManagementData,
  GridAgentBaseData,
  GridAgentConstantData,
}
import edu.ie3.simona.agent.grid.GridAgentMessages._
import edu.ie3.simona.ontology.messages.Activation
import org.apache.pekko.actor.typed.scaladsl.AskPattern.Askable
import org.apache.pekko.actor.typed.scaladsl.{
  ActorContext,
  Behaviors,
  StashBuffer,
}
import org.apache.pekko.actor.typed.{Behavior, Scheduler}
import org.apache.pekko.util.Timeout

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

    case (ctx, StartStep) =>
      // request congestion check if we have inferior grids
      if (stateData.inferiorRefs.nonEmpty) {
        implicit val askTimeout: Timeout = Timeout.create(
          stateData.gridAgentBaseData.congestionManagementParams.timeout
        )
        implicit val ec: ExecutionContext = ctx.executionContext
        implicit val scheduler: Scheduler = ctx.system.scheduler

        val future = Future
          .sequence(
            stateData.inferiorRefs.map { inferiorGridAgentRef =>
              inferiorGridAgentRef
                .ask(ref => CongestionCheckRequest(ref))
                .map { case response: CongestionResponse => response }
            }.toVector
          )
          .map(res => ReceivedCongestions(res))
        pipeToSelf(future, ctx)
      }

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
        val congestions = stateData.congestions

        if (congestions.any) {
          ctx.log.warn(
            s"In the grid ${stateData.gridAgentBaseData.gridEnv.gridModel.subnetNo}, the following congestions were found: $congestions"
          )
        }

        // sends the results to the superior grid
        sender ! CongestionResponse(
          congestions.combine(
            stateData.inferiorCongestionMap.values.flatten
          ),
          ctx.self,
        )
      }

      Behaviors.same

    case (ctx, ReceivedCongestions(congestions)) =>
      // updating the state data with received data from inferior grids
      val updatedStateData = stateData.handleReceivingData(congestions)

      if (updatedStateData.gridAgentBaseData.isSuperior) {
        // if we are the superior grid, we find the next behavior

        val congestions = updatedStateData.congestions.combine(
          updatedStateData.inferiorCongestionMap.values.flatten
        )

        // checking for any congestion in the complete grid
        if (!congestions.any) {
          ctx.log.warn(
            s"No congestions found. Finishing the congestion management."
          )

          ctx.self ! GotoIdle
          checkForCongestion(updatedStateData)
        } else {
          ctx.log.warn(s"Congestions: $congestions")

          val steps =
            updatedStateData.gridAgentBaseData.congestionManagementParams

          val msg =
            if (congestions.voltageCongestions && steps.runTransformerTapping) {
              NextStepRequest(TransformerTapping)
            } else if (
              congestions.lineCongestions && steps.runTopologyChanges
            ) {
              NextStepRequest(TopologyChanges)
            } else if (congestions.any && steps.useFlexOptions) {
              NextStepRequest(UsingFlexibilities)
            } else {
              val timestamp =
                constantData.simStartTime.plusSeconds(stateData.currentTick)

              ctx.log.info(
                s"There were some congestions that could not be resolved for timestamp: $timestamp."
              )
              GotoIdle
            }

          ctx.self ! msg
          checkForCongestion(updatedStateData)
        }

      } else {
        // un-stash all messages
        buffer.unstashAll(checkForCongestion(updatedStateData))
      }

    case (ctx, NextStepRequest(next)) =>
      // inform my inferior grids about the next behavior
      stateData.inferiorRefs.foreach(
        _ ! NextStepRequest(next)
      )

      // switching to the next behavior
      ctx.self ! StartStep

      next match {
        case TransformerTapping =>
          buffer.unstashAll(updateTransformerTapping(stateData))
        case TopologyChanges => buffer.unstashAll(useTopologyChanges(stateData))
        case UsingFlexibilities => buffer.unstashAll(useFlexOptions(stateData))
      }

    case (ctx, GotoIdle) =>
      // inform my inferior grids about the end of the congestion management
      stateData.inferiorRefs.foreach(
        _ ! GotoIdle
      )

      // clean up agent and go back to idle
      GridAgent.gotoIdle(
        stateData.gridAgentBaseData,
        stateData.currentTick,
        Some(stateData.powerFlowResults),
        ctx,
      )

    case (ctx, msg: GridAgent.Request) =>
      ctx.log.error(s"Received unsupported msg: $msg. Stash away!")
      buffer.stash(msg)
      Behaviors.same
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
  // TODO: Implement a proper behavior
  private[grid] def updateTransformerTapping(
      stateData: CongestionManagementData
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = Behaviors.receivePartial {
    case (ctx, StartStep) =>
      if (stateData.gridAgentBaseData.isSuperior) {
        // for now this step is skipped
        ctx.log.warn(
          s"Using transformer taping to resolve a congestion is not implemented yet. Skipping this step!"
        )

        ctx.self ! FinishStep
      }

      Behaviors.same

    case (ctx, FinishStep) =>
      // inform my inferior grids about the end of this step
      stateData.inferiorRefs.foreach(_ ! FinishStep)

      // switching to simulating grid
      val currentTick = stateData.currentTick

      val gridAgentBaseData = stateData.gridAgentBaseData
      val updatedData = gridAgentBaseData.copy(congestionManagementParams =
        gridAgentBaseData.congestionManagementParams
          .copy(hasRunTransformerTapping = true)
      )

      // simulate grid after changing the transformer tapping
      clearAndGotoSimulateGrid(updatedData, currentTick, ctx)

    case (ctx, msg: GridAgent.Request) =>
      ctx.log.error(s"Received unsupported msg: $msg. Stash away!")
      buffer.stash(msg)
      Behaviors.same
  }

  // TODO: Implement a proper behavior
  private[grid] def useTopologyChanges(stateData: CongestionManagementData)(
      implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = Behaviors.receivePartial {
    case (ctx, StartStep) =>
      if (stateData.gridAgentBaseData.isSuperior) {
        // for now this step is skipped
        ctx.log.warn(
          s"Using transformer taping to resolve a congestion is not implemented yet. Skipping this step!"
        )

        ctx.self ! FinishStep
      }

      Behaviors.same

    case (ctx, FinishStep) =>
      // inform my inferior grids about the end of this step
      stateData.inferiorRefs.foreach(_ ! FinishStep)

      val gridAgentBaseData = stateData.gridAgentBaseData
      val updatedData = gridAgentBaseData.copy(congestionManagementParams =
        gridAgentBaseData.congestionManagementParams
          .copy(hasRunTopologyChanges = true)
      )

      ctx.self ! StartStep
      checkForCongestion(stateData.copy(gridAgentBaseData = updatedData))

    case (ctx, msg: GridAgent.Request) =>
      ctx.log.error(s"Received unsupported msg: $msg. Stash away!")
      buffer.stash(msg)
      Behaviors.same
  }

  // TODO: Implement a proper behavior
  private[grid] def useFlexOptions(stateData: CongestionManagementData)(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = Behaviors.receivePartial {
    case (ctx, StartStep) =>
      if (stateData.gridAgentBaseData.isSuperior) {
        // for now this step is skipped
        ctx.log.warn(
          s"Using transformer taping to resolve a congestion is not implemented yet. Skipping this step!"
        )

        ctx.self ! FinishStep
      }

      Behaviors.same

    case (ctx, FinishStep) =>
      // inform my inferior grids about the end of this step
      stateData.inferiorRefs.foreach(_ ! FinishStep)

      GridAgent.gotoIdle(
        stateData.gridAgentBaseData,
        stateData.currentTick,
        Some(stateData.powerFlowResults),
        ctx,
      )

    case (ctx, msg: GridAgent.Request) =>
      ctx.log.error(s"Received unsupported msg: $msg. Stash away!")
      buffer.stash(msg)
      Behaviors.same
  }

  private def clearAndGotoSimulateGrid(
      gridAgentBaseData: GridAgentBaseData,
      currentTick: Long,
      ctx: ActorContext[GridAgent.Request],
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = {

    val cleanedData = GridAgentBaseData.clean(
      gridAgentBaseData,
      gridAgentBaseData.superiorGridNodeUuids,
      gridAgentBaseData.inferiorGridGates,
    )

    ctx.self ! WrappedActivation(Activation(currentTick))
    GridAgent.simulateGrid(
      cleanedData.copy(congestionManagementParams =
        gridAgentBaseData.congestionManagementParams
      ),
      currentTick,
    )
  }
}
