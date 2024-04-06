/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.grid.CongestionManagementData.Congestions
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentBaseData
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import org.apache.pekko.actor.typed.ActorRef

case class CongestionManagementData(
    gridAgentBaseData: GridAgentBaseData,
    currentTick: Long,
    powerFlowData: PowerFlowResultEvent,
    estimatedData: PowerFlowResultEvent,
    congestions: Congestions,
    inferiorCongestions: Map[ActorRef[GridAgentMessage], Option[Congestions]],
) {

  /** Returns true if congestion data from inferior grids is expected and no
    * data was received yet.
    */
  def awaitingInferiorData: Boolean =
    inferiorCongestions.values.exists(_.isEmpty)

  /** Method for updating the data with the received data.
    * @param receivedData
    *   data that was received
    * @return
    *   a updated copy of this data
    */
  def handleReceivingData(
      receivedData: Map[ActorRef[GridAgentMessage], Option[Congestions]]
  ): CongestionManagementData = {
    copy(inferiorCongestions = inferiorCongestions ++ receivedData)
  }

  def inferiorRefs: Set[ActorRef[GridAgentMessage]] =
    gridAgentBaseData.inferiorGridGates
      .map(gridAgentBaseData.gridEnv.subgridGateToActorRef(_))
      .distinct
      .toSet
}

object CongestionManagementData {
  case class Congestions(
      voltageCongestions: Boolean,
      lineCongestions: Boolean,
      transformerCongestions: Boolean,
  ) {

    def combine(options: Iterable[Congestions]): Congestions =
      Congestions(
        voltageCongestions || options.exists(_.voltageCongestions),
        lineCongestions || options.exists(_.lineCongestions),
        transformerCongestions || options.exists(_.transformerCongestions),
      )

  }
}
