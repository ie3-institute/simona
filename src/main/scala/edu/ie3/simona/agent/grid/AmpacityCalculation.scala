/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.grid.AmpacityCalculationMessages.{
  AmpacityCalcRequest,
  AmpacityCalcResponse,
}
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.GridAgent.{
  InternalReplyWithSender,
  InternalRequest,
  askInferior,
  unsupported,
}
import edu.ie3.simona.agent.grid.congestion.CongestionManagementMessages.*
import edu.ie3.simona.agent.grid.data.AmpacityCalculationData
import edu.ie3.simona.agent.grid.data.GridAgentData.GridAgentConstantData
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.model.grid.ampacity.LineStateResult
import org.apache.pekko.actor.typed.scaladsl.{
  ActorContext,
  Behaviors,
  StashBuffer,
}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

trait AmpacityCalculation {

  /** Method that defines the [[Behavior]] for calculating the ampacity.
    * @param stateData
    *   Of the actor.
    * @param constantData
    *   Constant data of the [[GridAgent]].
    * @param buffer
    *   For stashed messages.
    * @return
    *   A [[Behavior]]
    */
  private[grid] def calcAmpacity(
      stateData: AmpacityCalculationData
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Message],
  ): Behavior[GridAgent.Message] = Behaviors.receivePartial {
    case (ctx, StartStep) =>
      askInferior(
        stateData.inferiorGridRefs,
        (ref, _) => AmpacityCalcRequest(ref),
      )(using ctx)

      Behaviors.same

    case (ctx, ampacityCalcRequest: AmpacityCalcRequest) =>
      answerRequest(
        stateData,
        ampacityCalcRequest,
        ctx,
      )

    case (ctx, msg) =>
      unsupported(msg, ctx.log)
      Behaviors.same
  }

  private def answerRequest(
      stateData: AmpacityCalculationData,
      ampacityCalcRequest: AmpacityCalcRequest,
      ctx: ActorContext[GridAgent.Message],
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Message],
  ): Behavior[GridAgent.Message] = {
    calcAmpacity(stateData)
  }

  private def processReceivedData(
      stateData: AmpacityCalculationData,
      response: AmpacityCalcResponse,
      ctx: ActorContext[GridAgent.Message],
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Message],
  ): Behavior[GridAgent.Message] = {
    buffer.unstashAll(calcAmpacity(stateData))
  }
}

/** Messages for the ampacity calculation.
  */
object AmpacityCalculationMessages {

  /** Request for to calculate ampacities in an inferior grid.
    *
    * @param sender
    *   That is asking.
    */
  final case class AmpacityCalcRequest(
      sender: ActorRef[GridAgent.Message]
  ) extends InternalRequest

  /** Response of ampacity calculations from an inferior grid.
    *
    * @param sender
    *   Inferior grid ref.
    * @param value
    *   Ampacities in the inferior grid.
    */
  final case class AmpacityCalcResponse(
      override val sender: ActorRef[GridAgent.Message],
      override val value: Seq[LineStateResult],
  ) extends InternalReplyWithSender[Seq[LineStateResult]]

  /** Message that informs the grid agent to start with ampacity calculation.
    */
  case class DoAmpacityCalculation(
      currentTick: Long,
      results: Option[PowerFlowResultEvent],
  ) extends GridAgent.InternalRequest

  /** Message that informs all actors that the next state is the idle state.
    */
  case object GotoIdle extends GridAgent.InternalRequest
}
