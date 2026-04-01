/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.GridAgent.Message
import edu.ie3.simona.agent.grid.congestion.CongestionManagementMessages.StartStep
import edu.ie3.simona.agent.grid.congestion.detection.CongestionDetection
import edu.ie3.simona.agent.grid.data.CongestionManagementData
import edu.ie3.simona.agent.grid.data.GridAgentData.{
  GridAgentBaseData,
  GridAgentConstantData,
}
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.util.ReceiveDataMap
import org.apache.pekko.actor.typed.Behavior
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, StashBuffer}

/** Trait that is normally mixed into every [[GridAgent]] to enable distributed
  * congestion management (DCM) algorithm execution. It is considered to be the
  * standard behavior of a [[GridAgent]].
  */
trait DCMAlgorithm extends CongestionDetection {

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
  private[grid] def finishCongestionManagement(
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
