/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.grid.CongestionManagementSupport.CongestionManagementSteps._
import edu.ie3.simona.agent.grid.CongestionManagementSupport.{
  Congestions,
  VoltageRange,
}
import edu.ie3.simona.agent.grid.GridAgent.pipeToSelf
import edu.ie3.simona.agent.grid.GridAgentData.{
  AwaitingData,
  CongestionManagementData,
  GridAgentBaseData,
  GridAgentConstantData,
}
import edu.ie3.simona.agent.grid.GridAgentMessages._
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.apache.pekko.actor.typed.scaladsl.AskPattern.Askable
import org.apache.pekko.actor.typed.scaladsl.{
  ActorContext,
  Behaviors,
  StashBuffer,
}
import org.apache.pekko.actor.typed.{ActorRef, Behavior, Scheduler}
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
      askInferior(
        stateData,
        CongestionCheckRequest,
        ReceivedCongestions,
        ctx,
      )

      Behaviors.same

    case (ctx, congestionRequest @ CongestionCheckRequest(sender)) =>
      // check if waiting for inferior data is needed
      if (!awaitingData.isDone) {
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
          ctx.self,
          congestions.combine(awaitingData.values),
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
  private[grid] def updateTransformerTapping(
      stateData: CongestionManagementData,
      awaitingData: AwaitingData[VoltageRange],
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = Behaviors.receivePartial {
    case (ctx, StartStep) =>
      // request congestion check if we have inferior grids
      askInferior(
        stateData,
        RequestVoltageOptions,
        ReceivedVoltageRange,
        ctx,
      )

      Behaviors.same

    case (ctx, voltageRangeRequest @ RequestVoltageOptions(sender)) =>
      // check if waiting for inferior data is needed
      if (!awaitingData.isDone) {
        ctx.log.debug(
          s"Received request for congestions before all data from inferior grids were received. Stashing away."
        )

        // stash away the message, because we need to wait for data from inferior grids
        buffer.stash(voltageRangeRequest)
      } else {
        // calculate the voltage range for this grid
        val gridEnv = stateData.gridAgentBaseData.gridEnv
        val gridModel = gridEnv.gridModel
        val gridComponents = gridModel.gridComponents

        val (_, _, tappingModels) = getTransformerInfos(
          stateData.inferiorGrids,
          gridEnv.subgridGateToActorRef,
          gridComponents,
        )

        val range = calculateVoltageOptions(
          stateData.powerFlowResults,
          gridModel.voltageLimits,
          gridModel.gridComponents,
          tappingModels,
          awaitingData.mappedValues,
        )

        sender ! VoltageRangeResponse(
          ctx.self,
          range,
        )
      }

      Behaviors.same

    case (ctx, ReceivedVoltageRange(voltageRange)) =>
      // updating the state data with received data from inferior grids
      val updatedData = awaitingData.handleReceivingData(voltageRange)

      if (stateData.gridAgentBaseData.isSuperior) {
        // there should be no voltage change in the superior grid,
        // because the slack grid should always have 1 pu

        ctx.self ! VoltageDeltaResponse(0.asPu)
        updateTransformerTapping(stateData, updatedData)
      } else {
        // un-stash all messages
        buffer.unstashAll(updateTransformerTapping(stateData, updatedData))
      }

    case (ctx, VoltageDeltaResponse(delta)) =>
      // if we are the superior grid to another grid, we check for transformer tapping option
      // and send the new delta to the inferior grid

      if (stateData.inferiorRefs.nonEmpty) {
        // we calculate a voltage delta for all inferior grids
        val inferiorData = awaitingData.mappedValues

        val gridEnv = stateData.gridAgentBaseData.gridEnv
        val gridModel = gridEnv.gridModel
        val gridComponents = gridModel.gridComponents

        val (transformer2ws, transformer3ws, _) = getTransformerInfos(
          stateData.inferiorGrids,
          gridEnv.subgridGateToActorRef,
          gridComponents,
        )

        val transformer3wMap =
          gridComponents.transformers3w.map(t => t.uuid -> t).toMap

        val modelMap = transformer2ws
          .groupBy(_._2) ++ transformer3ws.groupBy(_._2.uuid).map {
          case (uuid, refMap) =>
            transformer3wMap(uuid) -> refMap
        }

        modelMap.foreach { case (model, refMap) =>
          val refs = refMap.keySet

          if (model.hasAutoTap) {
            // the given transformer can be tapped, calculate the new tap pos

            val suggestion =
              VoltageRange.combineSuggestions(refs.map(inferiorData))

            val tapOption = model.computeDeltaTap(suggestion)
            val deltaV = if (tapOption == 0) {
              // we can not change the voltage as we would like to
              if (suggestion.isLessThan(0.asPu)) {
                // if suggestion < 0, we decrease the voltage as much as we can

                val tapChange = model.maxTapDecrease
                model.decrTapPos(tapChange)

                model.deltaV.multiply(tapChange)
              } else {
                // we increase the voltage as much as we can
                val tapChange = model.maxTapIncrease
                model.decrTapPos(tapChange)

                model.deltaV.multiply(tapChange)
              }
            } else {
              // we can change the voltage without a problem
              model.updateTapPos(tapOption)
              model.deltaV.multiply(tapOption)
            }

            refs.foreach(_ ! VoltageDeltaResponse(deltaV.divide(100)))
          } else {
            // no tapping possible, just send the delta to the inferior grid
            refs.foreach(_ ! VoltageDeltaResponse(delta))
          }
        }
      }

      // all work is done in this grid, finish this step
      ctx.self ! FinishStep
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

  /** Method to ask all inferior grids a [[CMRequest]].
    *
    * @param stateData
    *   current state data
    * @param askMsgBuilder
    *   function to build the asked message
    * @param resMsgBuilder
    *   function to build the returned message
    * @param ctx
    *   actor context to use
    * @tparam T
    *   type of data
    */
  private def askInferior[T](
      stateData: CongestionManagementData,
      askMsgBuilder: ActorRef[GridAgent.Request] => CMRequest,
      resMsgBuilder: Vector[(ActorRef[GridAgent.Request], T)] => CMResponse[T],
      ctx: ActorContext[GridAgent.Request],
  ): Unit = {

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
              .ask(askMsgBuilder)
              .map { case response: CMReceiveResponse[T] =>
                (response.sender, response.value)
              }
          }.toVector
        )
        .map(resMsgBuilder)
      pipeToSelf(future, ctx)
    }

  }

  /** Method to clear all data and go to the [[DBFSAlgorithm.simulateGrid]].
    *
    * @param gridAgentBaseData
    *   to clear
    * @param currentTick
    *   to use
    * @param ctx
    *   actor context
    * @param constantData
    *   constant grid agent data
    * @param buffer
    *   for buffered messages
    * @return
    *   a new [[Behavior]]
    */
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
