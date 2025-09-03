/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.GridAgent.Message
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
import edu.ie3.simona.agent.participant.ParticipantAgent.GridSimulationFinished
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.ontology.messages.Activation
import org.apache.pekko.actor.typed.Behavior
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, StashBuffer}

/** Trait that is normally mixed into every [[GridAgent]] to enable distributed
  * congestion management (DCM) algorithm execution. It is considered to be the
  * standard behaviour of a [[GridAgent]].
  */
trait DCMAlgorithm extends CongestionDetection with TransformerTapChange {

  /** Method for starting the congestion management.
    *
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
    *   for [[GridAgent.Message]]s
    * @return
    *   a [[Behavior]]
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

  private[grid] def doPowerFlow(
      stateData: CongestionManagementData,
      ctx: ActorContext[Message],
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[Message],
  ): Behavior[Message] = {
    val gridAgentBaseData = stateData.gridAgentBaseData
    val tick = stateData.currentTick

    // inform every system participant about a new simulation for the same tick
    gridAgentBaseData.gridEnv.nodeToAssetAgents.values.foreach { actors =>
      actors.foreach { actor =>
        actor ! GridSimulationFinished(tick, tick)
      }
    }

    // / clean copy of the gridAgentBaseData
    val cleanedGridAgentBaseData = GridAgentBaseData.clean(
      gridAgentBaseData,
      gridAgentBaseData.superiorGridNodeUuids,
      gridAgentBaseData.inferiorGridGates,
    )

    ctx.self ! Activation(tick)

    // do another power flow calculation
    GridAgent.simulateGrid(
      cleanedGridAgentBaseData,
      tick,
    )
  }

  /** Method for finishing the congestion management. This method will return to
    * the [[GridAgent.idle()]] state afterward.
    *
    * @param stateData
    *   congestion management state data
    * @param ctx
    *   actor context
    * @param constantData
    *   immutable [[GridAgent]] values
    * @param buffer
    *   for [[GridAgent.Message]]s
    * @return
    *   a [[Behavior]]
    */
  private[grid] def finishCongestionManagement(
      stateData: CongestionManagementData,
      ctx: ActorContext[Message],
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[Message],
  ): Behavior[Message] = {
    val timestamp = constantData.simStartTime.plusSeconds(stateData.currentTick)

    ctx.log.info(
      s"There were some congestions that could not be resolved for timestamp: $timestamp."
    )

    // clean up agent and go back to idle
    val powerFlowResults = stateData.getAllResults(constantData.simStartTime)

    // return to idle
    GridAgent.gotoIdle(
      stateData.resetProgress.gridAgentBaseData,
      stateData.currentTick + constantData.resolution,
      Some(powerFlowResults),
      ctx,
    )
  }

}
