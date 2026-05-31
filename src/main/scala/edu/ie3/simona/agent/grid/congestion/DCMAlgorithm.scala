/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import edu.ie3.simona.agent.grid.GridAgent.{Message, simulateGrid}
import edu.ie3.simona.agent.grid.GridAgentMessages.DoPowerFlowTrigger
import edu.ie3.simona.agent.grid.congestion.CongestionManagementMessages.{
  GotoIdle,
  NextStep,
  StartStep,
}
import edu.ie3.simona.agent.grid.congestion.detection.CongestionDetection
import edu.ie3.simona.agent.grid.congestion.mitigations.MitigationSteps.TransformerTapChange
import edu.ie3.simona.agent.grid.congestion.mitigations.TransformerTapPositionChange
import edu.ie3.simona.agent.grid.data.GridAgentData.{
  GridAgentBaseData,
  GridAgentConstantData,
}
import edu.ie3.simona.agent.grid.data.{
  AmpacityCalculationData,
  CongestionManagementData,
}
import edu.ie3.simona.agent.grid.{AmpacityCalculation, GridAgent}
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.util.ReceiveDataMap
import org.apache.pekko.actor.typed.Behavior
import org.apache.pekko.actor.typed.scaladsl.{
  ActorContext,
  Behaviors,
  StashBuffer,
}

/** Trait that is normally mixed into every [[GridAgent]] to enable distributed
  * congestion management (DCM) algorithm execution. It is considered to be the
  * standard behavior of a [[GridAgent]].
  */
trait DCMAlgorithm
    extends CongestionDetection
    with AmpacityCalculation
    with TransformerTapPositionChange {

  /** Method for starting the ampacity calculation.
    *
    * @param gridAgentBaseData
    *   State data of the actor.
    * @param currentTick
    *   The current tick in the simulation.
    * @param subGridNo
    *   The number of the subgrid.
    * @param results
    *   Option for the last power flow results.
    * @param ctx
    *   Actor context.
    * @param constantData
    *   Immutable [[GridAgent]] values.
    * @param buffer
    *   For [[GridAgent.Message]]s.
    * @return
    *   A [[Behavior]].
    */
  private[grid] def startAmpacityCalculation(
      gridAgentBaseData: GridAgentBaseData,
      currentTick: Long,
      subGridNo: Int,
      results: Option[PowerFlowResultEvent],
      ctx: ActorContext[Message],
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[Message],
  ): Behavior[Message] = {
    // build the state data
    val ampacityCalcData =
      AmpacityCalculationData(
        gridAgentBaseData,
        currentTick,
        subGridNo,
        results,
      )

    ctx.self ! StartStep
    GridAgent.calcAmpacity(
      ampacityCalcData
    )
  }

  /** Method for starting the congestion management.
    *
    * @param gridAgentBaseData
    *   State data of the actor.
    * @param currentTick
    *   The current tick in the simulation.
    * @param results
    *   Option for the last power flow results.
    * @param ctx
    *   Actor context.
    * @param constantData
    *   Immutable [[GridAgent]] values.
    * @param buffer
    *   For [[GridAgent.Message]]s.
    * @return
    *   A [[Behavior]].
    */
  private[grid] def startCongestionManagement(
      gridAgentBaseData: GridAgentBaseData,
      currentTick: Long,
      results: Option[PowerFlowResultEvent],
      ctx: ActorContext[Message],
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[Message],
  ): Behavior[Message] = {
    // build the state data
    val congestionManagementData =
      CongestionManagementData(gridAgentBaseData, currentTick, results)

    ctx.self ! StartStep
    GridAgent.checkForCongestion(
      congestionManagementData,
      ReceiveDataMap(congestionManagementData.inferiorGridRefs.keySet),
    )
  }

  private[congestion] def waitForNextStep(
      stateData: CongestionManagementData
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[Message],
  ): Behavior[GridAgent.Message] = Behaviors.receivePartial {
    case (ctx, GotoIdle) =>
      // inform my inferior grids about the end of the congestion management
      stateData.inferiorGridRefs.keys.foreach(_ ! GotoIdle)

      // directly finish congestion management, since we don't have any steps
      finishCongestionManagement(stateData, ctx)

    case (ctx, nextStep: NextStep) =>
      stateData.inferiorGridRefs.keys.foreach(_ ! nextStep)

      nextStep.step match {
        case TransformerTapChange =>
          ctx.self ! StartStep

          updateTransformerTapping(
            stateData,
            ReceiveDataMap(stateData.inferiorGridRefs.keySet),
          )

        case _ =>
          throw new IllegalStateException("This should not happen!")
      }

    case (ctx, doPowerFlowTrigger: DoPowerFlowTrigger) =>
      ctx.self ! doPowerFlowTrigger
      simulateGrid(stateData.gridAgentBaseData, doPowerFlowTrigger.tick)
  }

  /** Method for finishing the congestion management. This method will return to
    * the [[GridAgent.idle()]] state afterward.
    *
    * @param stateData
    *   Congestion management state data.
    * @param ctx
    *   Actor context.
    * @param constantData
    *   Immutable [[GridAgent]] values.
    * @param buffer
    *   For [[GridAgent.Message]]s.
    * @return
    *   A [[Behavior]].
    */
  private[congestion] def finishCongestionManagement(
      stateData: CongestionManagementData,
      ctx: ActorContext[Message],
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[Message],
  ): Behavior[Message] = {
    // clean up agent and go back to idle
    val powerFlowResults = stateData.getAllResults(constantData.simStartTime)

    // return to idle
    GridAgent.gotoIdle(
      stateData.gridAgentBaseData,
      Some(powerFlowResults),
      ctx,
    )
  }

}
