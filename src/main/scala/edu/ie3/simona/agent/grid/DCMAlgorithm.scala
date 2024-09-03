/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.grid.CongestionManagementSupport.CongestionManagementSteps._
import edu.ie3.simona.agent.grid.CongestionManagementSupport.{
  Congestions,
  TappingGroup,
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
import edu.ie3.simona.model.grid.TransformerTapping
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
      if (awaitingData.notDone) {
        ctx.log.debug(
          s"Received request for congestions before all data from inferior grids were received. Stashing away."
        )

        // stash away the message, because we need to wait for data from inferior grids
        buffer.stash(congestionRequest)
      } else {
        // check if there are any congestions in the grid
        val congestions = stateData.congestions

        if (congestions.any) {
          ctx.log.info(
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
          ctx.log.debug(
            s"Congestion overall: $congestions"
          )

          val params = stateData.gridAgentBaseData.congestionManagementParams

          val msg =
            if (
              (congestions.voltageCongestions || congestions.lineCongestions) && params.runTransformerTapping
            ) {
              NextStepRequest(TransformerTapping)
            } else if (
              congestions.assetCongestion && params.runTopologyChanges
            ) {
              NextStepRequest(TopologyChanges)
            } else if (congestions.any && params.useFlexOptions) {
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
      stateData.inferiorGridRefs.foreach(
        _ ! NextStepRequest(next)
      )

      // switching to the next behavior
      ctx.self ! StartStep

      next match {
        case TransformerTapping =>
          // if next state is tranformer tapping
          buffer.unstashAll(
            updateTransformerTapping(
              stateData,
              AwaitingData(stateData.inferiorGridRefs),
            )
          )
        case TopologyChanges =>
          // if next step is topology change
          buffer.unstashAll(
            useTopologyChanges(
              stateData,
              AwaitingData(stateData.inferiorGridRefs),
            )
          )
        case UsingFlexibilities =>
          // if next step is using flexibilities
          buffer.unstashAll(
            useFlexOptions(stateData, AwaitingData(stateData.inferiorGridRefs))
          )
      }

    case (ctx, GotoIdle) =>
      // inform my inferior grids about the end of the congestion management
      stateData.inferiorGridRefs.foreach(
        _ ! GotoIdle
      )

      // clean up agent and go back to idle
      val powerFlowResults = stateData.powerFlowResults.copy(congestionResults =
        Seq(stateData.getCongestionResult(constantData.simStartTime))
      )

      // return to idle
      GridAgent.gotoIdle(
        stateData.gridAgentBaseData,
        stateData.currentTick,
        Some(powerFlowResults),
        ctx,
      )

    case (ctx, msg) =>
      ctx.log.debug(s"Received unsupported msg: $msg. Stash away!")
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
      awaitingData: AwaitingData[(VoltageRange, Set[TransformerTapping])],
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = Behaviors.receivePartial {
    case (ctx, StartStep) =>
      val subnet = stateData.gridAgentBaseData.gridEnv.gridModel.subnetNo

      // request congestion check if we have inferior grids
      askInferior(
        stateData,
        ref => RequestVoltageOptions(ref, subnet),
        ReceivedVoltageRange,
        ctx,
      )

      Behaviors.same

    case (ctx, voltageRangeRequest @ RequestVoltageOptions(sender, subnet)) =>
      // check if waiting for inferior data is needed
      if (awaitingData.notDone) {
        ctx.log.debug(
          s"Received request for voltage range before all data from inferior grids were received. Stashing away."
        )

        // stash away the message, because we need to wait for data from inferior grids
        buffer.stash(voltageRangeRequest)
      } else {
        // calculate the voltage range for this grid
        val gridEnv = stateData.gridAgentBaseData.gridEnv
        val gridModel = gridEnv.gridModel
        val gridComponents = gridModel.gridComponents

        // filter all transformers that are connecting this grid to the superior grid
        val nodesInSuperiorGrid =
          gridComponents.nodes.filter(_.subnet == subnet).map(_.uuid)
        val transformers = gridComponents.transformers.filter(t =>
          nodesInSuperiorGrid.contains(t.hvNodeUuid)
        )
        val transformers3w = gridComponents.transformers3w.filter(t =>
          nodesInSuperiorGrid.contains(t.hvNodeUuid)
        )

        val allTransformers = (transformers ++ transformers3w).map(
          _.asInstanceOf[TransformerTapping]
        )

        // calculate the voltage range with the received data
        val range = calculatePossibleVoltageRange(
          stateData.powerFlowResults,
          gridModel.voltageLimits,
          gridModel.gridComponents,
          awaitingData.mappedValues,
          gridModel.subnetNo,
        )

        ctx.log.warn(
          s"For Grid ${stateData.gridAgentBaseData.gridEnv.gridModel.subnetNo}, voltage range: $range"
        )

        sender ! VoltageRangeResponse(
          ctx.self,
          (range, allTransformers),
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
      ctx.log.debug(
        s"Grid ${stateData.gridAgentBaseData.gridEnv.gridModel.subnetNo}, received delta: $delta"
      )

      if (stateData.inferiorGridRefs.nonEmpty) {
        // we calculate a voltage delta for all inferior grids

        val receivedData = awaitingData.mappedValues

        // map the actor ref to the possible voltage range
        val refMap = receivedData.map { case (ref, (range, _)) =>
          ref -> range
        }

        // groups all tapping models
        // necessary, because to make sure the tapping is change by the same value between two grids,
        // we need to know all transformers that are relevant as well as all actor refs to check their
        // possible voltage ranges
        val groups =
          groupTappingModels(
            receivedData.map { case (ref, (_, tappings)) => ref -> tappings },
            stateData.gridAgentBaseData.gridEnv.gridModel.gridComponents.transformers3w,
          )

        groups.foreach { case TappingGroup(refs, tappingModels) =>
          // get all possible voltage ranges of the inferior grids
          val inferiorRanges = refs.map(refMap)

          // check if all transformers support tapping
          if (tappingModels.forall(_.hasAutoTap)) {
            // the given transformer can be tapped, calculate the new tap pos

            val suggestion =
              VoltageRange.combineAndUpdate(inferiorRanges, delta)

            // calculating the tap changes for all transformers and the resulting voltage delta
            val (tapChange, deltaV) = calculateTapAndVoltage(
              suggestion,
              tappingModels.toSeq,
            )

            // change the tap pos of all transformers
            tapChange.foreach { case (tapping, tapChange) =>
              tapChange compare 0 match {
                case 1 =>
                  // change > 0 -> increase
                  tapping.incrTapPos(tapChange)
                case -1 =>
                  // change < 0 -> decrease
                  tapping.decrTapPos(Math.abs(tapChange))
                case 0 =>
                // no change, do nothing
              }
            }

            ctx.log.debug(
              s"For inferior grids $refs, suggestion: $suggestion, delta: $deltaV"
            )

            // send the resulting voltage delta to all inferior grids
            refs.foreach(_ ! VoltageDeltaResponse(deltaV.add(delta)))
          } else {
            // no tapping possible, just send the delta to the inferior grid
            refs.foreach(_ ! VoltageDeltaResponse(delta))
          }
        }
      }

      // all work is done in this grid, therefore finish this step
      // simulate grid after changing the transformer tapping
      buffer.unstashAll(
        clearAndGotoSimulateGrid(
          stateData.cleanAfterTransformerTapping,
          stateData.currentTick,
          ctx,
        )
      )

    case (ctx, msg) =>
      ctx.log.debug(s"Received unsupported msg: $msg. Stash away!")
      buffer.stash(msg)
      Behaviors.same
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
      stateData.inferiorGridRefs.foreach(_ ! FinishStep)

      // simulate grid after using topology changes
      clearAndGotoSimulateGrid(
        stateData.cleanAfterTopologyChange,
        stateData.currentTick,
        ctx,
      )

    case (ctx, msg) =>
      ctx.log.debug(s"Received unsupported msg: $msg. Stash away!")
      buffer.stash(msg)
      Behaviors.same
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
      stateData.inferiorGridRefs.foreach(_ ! FinishStep)

      // simulate grid after finishing the congestion management
      clearAndGotoSimulateGrid(
        stateData.cleanAfterFlexOptions,
        stateData.currentTick,
        ctx,
      )

    case (ctx, msg) =>
      ctx.log.debug(s"Received unsupported msg: $msg. Stash away!")
      buffer.stash(msg)
      Behaviors.same
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
    if (stateData.inferiorGridRefs.nonEmpty) {
      // creating implicit vals
      implicit val askTimeout: Timeout = Timeout.create(
        stateData.gridAgentBaseData.congestionManagementParams.timeout
      )
      implicit val ec: ExecutionContext = ctx.executionContext
      implicit val scheduler: Scheduler = ctx.system.scheduler

      // asking process
      val future = Future
        .sequence(
          stateData.inferiorGridRefs.map { inferiorGridAgentRef =>
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

    // clear all data
    val cleanedData = GridAgentBaseData.clean(
      gridAgentBaseData,
      gridAgentBaseData.superiorGridNodeUuids,
      gridAgentBaseData.inferiorGridGates,
    )

    // keeping the congestion management params
    val data = cleanedData.copy(congestionManagementParams =
      gridAgentBaseData.congestionManagementParams
    )

    // activate a new simulation for the current tick
    ctx.self ! WrappedActivation(Activation(currentTick))

    // goto simulate grid
    buffer.unstashAll(GridAgent.simulateGrid(data, currentTick))
  }
}
