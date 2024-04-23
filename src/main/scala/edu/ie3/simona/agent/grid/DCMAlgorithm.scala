/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.grid.GridAgent.pipeToSelf
import edu.ie3.simona.agent.grid.CongestionManagementSupport.CongestionManagementSteps._
import edu.ie3.simona.agent.grid.CongestionManagementSupport.Congestions
import edu.ie3.simona.agent.grid.GridAgentData.{
  AwaitingData,
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
trait DCMAlgorithm extends CongestionManagementSupport {

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
                .map { case response: CongestionResponse =>
                  (response.sender, response.congestions)
                }
            }.toVector
          )
          .map(res => ReceivedCongestions(res))
        pipeToSelf(future, ctx)
      }

      Behaviors.same

    case (ctx, congestionRequest @ CongestionCheckRequest(sender)) =>
      // check if waiting for inferior data is needed
      if (awaitingData.isDone) {
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
          congestions.combine(awaitingData.values),
          ctx.self,
        )
      }

      Behaviors.same

    case (ctx, ReceivedCongestions(congestions)) =>
      // updating the state data with received data from inferior grids
      val updatedData = awaitingData.handleReceivingData(congestions)

      if (stateData.gridAgentBaseData.isSuperior) {
        // if we are the superior grid, we find the next behavior

        val congestions = stateData.congestions.combine(updatedData.values)

        // checking for any congestion in the complete grid
        if (!congestions.any) {
          ctx.log.warn(
            s"No congestions found. Finishing the congestion management."
          )

          ctx.self ! GotoIdle
          checkForCongestion(stateData, updatedData)
        } else {
          ctx.log.warn(s"Congestions: $congestions")

          val steps = stateData.gridAgentBaseData.congestionManagementParams

          val msg =
            if (
              (congestions.voltageCongestions || congestions.lineCongestions) && steps.runTransformerTapping
            ) {
              NextStepRequest(TransformerTapping)
            } else if (
              congestions.assetCongestion && steps.runTopologyChanges
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
          checkForCongestion(stateData, updatedData)
        }

      } else {
        // un-stash all messages
        buffer.unstashAll(checkForCongestion(stateData, updatedData))
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
          buffer.unstashAll(
            updateTransformerTapping(
              stateData,
              AwaitingData(stateData.inferiorGrids),
            )
          )
        case TopologyChanges =>
          buffer.unstashAll(
            useTopologyChanges(stateData, AwaitingData(stateData.inferiorGrids))
          )
        case UsingFlexibilities =>
          buffer.unstashAll(
            useFlexOptions(stateData, AwaitingData(stateData.inferiorGrids))
          )
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
      stateData: CongestionManagementData,
      awaitingData: AwaitingData[_],
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

      // simulate grid after changing the transformer tapping
      clearAndGotoSimulateGrid(
        stateData.cleanAfterTransformerTapping,
        stateData.currentTick,
        ctx,
      )
  }

  // TODO: Implement a proper behavior
  private[grid] def useTopologyChanges(
      stateData: CongestionManagementData,
      awaitingData: AwaitingData[_],
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = Behaviors.receivePartial {
    case (ctx, StartStep) =>
      if (stateData.gridAgentBaseData.isSuperior) {
        // for now this step is skipped
        ctx.log.warn(
          s"Using topology changes to resolve a congestion is not implemented yet. Skipping this step!"
        )

        ctx.self ! FinishStep
      }

      Behaviors.same

    case (ctx, FinishStep) =>
      // inform my inferior grids about the end of this step
      stateData.inferiorRefs.foreach(_ ! FinishStep)

      // simulate grid after using topology changes
      clearAndGotoSimulateGrid(
        stateData.cleanAfterTopologyChange,
        stateData.currentTick,
        ctx,
      )
  }

  // TODO: Implement a proper behavior
  private[grid] def useFlexOptions(
      stateData: CongestionManagementData,
      awaitingData: AwaitingData[_],
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = Behaviors.receivePartial {
    case (ctx, StartStep) =>
      if (stateData.gridAgentBaseData.isSuperior) {
        // for now this step is skipped
        ctx.log.warn(
          s"Using flex options to resolve a congestion is not implemented yet. Skipping this step!"
        )

        ctx.self ! FinishStep
      }

      Behaviors.same

    case (ctx, FinishStep) =>
      // inform my inferior grids about the end of this step
      stateData.inferiorRefs.foreach(_ ! FinishStep)

      // simulate grid after finishing the congestion management
      clearAndGotoSimulateGrid(
        stateData.cleanAfterFlexOptions,
        stateData.currentTick,
        ctx,
      )
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
