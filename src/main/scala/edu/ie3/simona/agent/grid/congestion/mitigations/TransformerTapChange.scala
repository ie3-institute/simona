/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion.mitigations

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.GridAgent.{
  askInferior,
  finishCongestionManagement,
  unsupported,
}
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentConstantData
import edu.ie3.simona.agent.grid.congestion.CongestionManagementMessages.*
import edu.ie3.simona.agent.grid.congestion.data.{
  AwaitingData,
  CongestionManagementData,
}
import edu.ie3.simona.agent.grid.congestion.mitigations.TappingMessages.*
import edu.ie3.simona.agent.grid.congestion.{Congestions, VoltageRange}
import edu.ie3.simona.model.control.TappingGroupModel
import edu.ie3.simona.model.grid.Transformer3wPowerFlowCase.{
  PowerFlowCaseB,
  PowerFlowCaseC,
}
import edu.ie3.simona.model.grid.TransformerTapping
import edu.ie3.util.quantities.QuantityUtils.asPu
import org.apache.pekko.actor.typed.scaladsl.{
  ActorContext,
  Behaviors,
  StashBuffer,
}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import tech.units.indriya.ComparableQuantity

import javax.measure.quantity.Dimensionless

trait TransformerTapChange {

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
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = Behaviors.receivePartial {
    case (ctx, StartStep) =>
      val subnet = stateData.gridAgentBaseData.gridEnv.gridModel.subnetNo

      // request congestion check if we have inferior grids
      askInferior(
        stateData.inferiorGridRefs,
        ref => RequestVoltageOptions(ref, subnet),
        ReceivedVoltageRange.apply,
        ctx,
      )(using stateData.timeout)

      Behaviors.same

    case (ctx, voltageOptionRequest: RequestVoltageOptions) =>
      answerRequest(stateData, awaitingData, voltageOptionRequest, ctx)

    case (ctx, ReceivedVoltageRange(voltageRange)) =>
      processReceivedData(stateData, awaitingData, voltageRange, ctx)

    case (ctx, VoltageDeltaResponse(delta)) =>
      handleUpdatedDataFromSuperior(stateData, awaitingData, delta, ctx)

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
      awaitingData: AwaitingData[(VoltageRange, Set[TransformerTapping])],
      request: RequestVoltageOptions,
      ctx: ActorContext[GridAgent.Request],
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = {
    val sender = request.sender
    val subgrid = request.subgrid

    // check if waiting for inferior data is needed
    if (awaitingData.notDone) {
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
        nodesInSuperiorGrid.contains(t.lvNodeUuid)
      )
      val transformers3w = gridComponents.transformers3w.filter { t =>
        t.powerFlowCase match {
          case PowerFlowCaseB =>
            nodesInSuperiorGrid.contains(t.mvNodeUuid)
          case PowerFlowCaseC =>
            nodesInSuperiorGrid.contains(t.lvNodeUuid)
        }
      }

      val allTransformers: Set[TransformerTapping] =
        transformers ++ transformers3w

      // calculate the voltage range with the received data
      val range = VoltageRange(
        stateData.powerFlowResults,
        gridModel.voltageLimits,
        gridModel.gridComponents,
        awaitingData.mappedValues,
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
      voltageRange: Seq[
        (ActorRef[GridAgent.Request], (VoltageRange, Set[TransformerTapping]))
      ],
      ctx: ActorContext[GridAgent.Request],
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = {
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
  }

  private def handleUpdatedDataFromSuperior(
      stateData: CongestionManagementData,
      awaitingData: AwaitingData[(VoltageRange, Set[TransformerTapping])],
      delta: ComparableQuantity[Dimensionless],
      ctx: ActorContext[GridAgent.Request],
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = {
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

      val actorRefToTappingModels
          : Map[ActorRef[GridAgent.Request], Set[TransformerTapping]] =
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
          val deltaV = group.updateTapPositions(delta, refMap, ctx.log)
          group.refs.foreach(_ ! VoltageDeltaResponse(deltaV.add(delta)))
        }
    }

    ctx.self ! FinishStep
    updateTransformerTapping(stateData, awaitingData)
  }

}
