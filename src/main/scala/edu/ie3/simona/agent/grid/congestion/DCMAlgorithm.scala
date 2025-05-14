/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.GridAgent.Request
import edu.ie3.simona.agent.grid.GridAgentData.{
  GridAgentBaseData,
  GridAgentConstantData,
}
import edu.ie3.simona.agent.grid.congestion.CongestionManagementMessages.StartStep
import edu.ie3.simona.agent.grid.congestion.data.{
  AwaitingData,
  CongestionManagementData,
}
import edu.ie3.simona.agent.grid.congestion.detection.CongestionDetection
import edu.ie3.simona.agent.grid.congestion.mitigations.TransformerTapChange
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import org.apache.pekko.actor.typed.Behavior
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, StashBuffer}

/** Trait that is normally mixed into every [[GridAgent]] to enable distributed
  * congestion management (DCM) algorithm execution. It is considered to be the
  * standard behaviour of a [[GridAgent]].
  */
trait DCMAlgorithm extends CongestionDetection with TransformerTapChange {

  /** Method for starting the congestion management.
    * @param gridAgentBaseData
    *   state data of the actor
    * @param currentTick
    *   the current tick in the simulation
    * @param results
    *   option for the last power flow results
    * @param ctx
    *   actor context
    * @param constantData
    *   immutable [[GridAgent]] values
    * @param buffer
    *   for [[GridAgent.Request]]s
    * @return
    *   a [[Behavior]]
    */
  private[grid] def startCongestionManagement(
      gridAgentBaseData: GridAgentBaseData,
      currentTick: Long,
      results: Option[PowerFlowResultEvent],
      ctx: ActorContext[Request],
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[Request],
  ): Behavior[Request] = {

    // get result or build empty data
    val congestionManagementData = results
      .map(res => CongestionManagementData(gridAgentBaseData, currentTick, res))
      .getOrElse(
        CongestionManagementData.empty(gridAgentBaseData, currentTick)
      )

    ctx.self ! StartStep
    GridAgent.checkForCongestion(
      congestionManagementData,
      AwaitingData(congestionManagementData.inferiorGridRefs.keySet),
    )
  }

  private[grid] def doCongestionMitigation(
      stateData: CongestionManagementData,
      ctx: ActorContext[Request],
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[Request],
  ): Behavior[Request] = {
    // first we find an option for the next mitigation step
    val (stepOption, updatedProgress) =
      stateData.mitigationProgress.getNextStepsAndUpdate

    // we update the state data with the updated progress
    val updatedStateData = stateData.copy(mitigationProgress = updatedProgress)

    stepOption match {
      case Some(transformerTapChange: TransformerTapChange) =>
        GridAgent.updateTransformerTapping(
          stateData,
          AwaitingData(stateData.inferiorGridRefs.keySet),
        )

      case _ =>
        // we have no more mitigation steps
        // we finish the mitigation
        finishCongestionManagement(stateData, ctx)
    }

  }

  /** Method for finishing the congestion management. This method will return to
    * the [[GridAgent.idle()]] state afterward.
    * @param stateData
    *   congestion management state data
    * @param ctx
    *   actor context
    * @param constantData
    *   immutable [[GridAgent]] values
    * @param buffer
    *   for [[GridAgent.Request]]s
    * @return
    *   a [[Behavior]]
    */
  private[grid] def finishCongestionManagement(
      stateData: CongestionManagementData,
      ctx: ActorContext[Request],
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[Request],
  ): Behavior[Request] = {
    val timestamp = constantData.simStartTime.plusSeconds(stateData.currentTick)

    ctx.log.info(
      s"There were some congestions that could not be resolved for timestamp: $timestamp."
    )

    // clean up agent and go back to idle
    val powerFlowResults = stateData.getAllResults(constantData.simStartTime)

    // return to idle
    GridAgent.gotoIdle(
      stateData.gridAgentBaseData,
      stateData.currentTick,
      Some(powerFlowResults),
      ctx,
    )
  }

}
