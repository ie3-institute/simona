/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.data

import edu.ie3.simona.agent.grid.data.GridAgentData.{
  GridAgentBaseData,
  GridAgentDataInternal,
  GridAgentRef,
}
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.model.grid.ampacity.LineStateResult
import edu.ie3.util.scala.collection.immutable.RichMultiMap.MultiMap

import java.time.ZonedDateTime
import java.util.UUID

/** State data of a grid agent during the ampacity calculation.
  *
  * @param gridAgentBaseData
  *   Agent base data.
  * @param currentTick
  *   Current tick used for additional power flow calculations.
  * @param subgridNo
  *   The number of the subgrid.
  * @param powerFlowResults
  *   Result of the previous power flow calculation.
  */
final case class AmpacityCalculationData(
    gridAgentBaseData: GridAgentBaseData,
    currentTick: Long,
    subgridNo: Int,
    powerFlowResults: Option[PowerFlowResultEvent],
) extends GridAgentDataInternal {

  /** Builds a [[???]] from the power flow results.
    * @param startTime
    *   Of the simulation.
    * @return
    *   An iterable of [[LineStateResult]].
    */
  private def getResultsOfOverloadedAssets(
      startTime: ZonedDateTime
  ): Iterable[LineStateResult] = {
    ???
  }

  lazy val inferiorGridRefs: MultiMap[GridAgentRef, UUID] =
    gridAgentBaseData.inferiorGridRefs

  lazy val superiorGridRefs: MultiMap[GridAgentRef, UUID] =
    gridAgentBaseData.superiorGridRefs

  object AmpacityCalculationData {

    def apply(
        gridAgentBaseData: GridAgentBaseData,
        currentTick: Long,
        subgridNo: Int,
        powerFlowResults: Option[PowerFlowResultEvent] = None,
    ): AmpacityCalculationData = {
      AmpacityCalculationData(
        gridAgentBaseData,
        currentTick,
        subgridNo,
        powerFlowResults,
      )
    }
  }
}
