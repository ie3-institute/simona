/*
 * © 2025-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.data

import edu.ie3.datamodel.models.result.CongestionResult
import edu.ie3.datamodel.models.result.CongestionResult.InputModelType
import edu.ie3.simona.agent.grid.congestion.{CongestedComponents, Congestions}
import edu.ie3.simona.agent.grid.data.GridAgentData.{
  GridAgentBaseData,
  GridAgentDataInternal,
  GridAgentRef,
}
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.util.quantities.QuantityUtils.asPercent
import edu.ie3.util.scala.collection.immutable.RichMultiMap.MultiMap
import squants.Each
import tech.units.indriya.unit.Units

import java.time.ZonedDateTime
import java.util.UUID

/** State data of a grid agent during the congestion management.
  *
  * @param gridAgentBaseData
  *   Agent base data.
  * @param currentTick
  *   Current tick used for additional power flow calculations.
  * @param subgridNo
  *   The number of the subgrid.
  * @param powerFlowResults
  *   Result of the previous power flow calculation.
  * @param congestions
  *   The found congestions.
  */
final case class CongestionManagementData(
    gridAgentBaseData: GridAgentBaseData,
    currentTick: Long,
    subgridNo: Int,
    powerFlowResults: PowerFlowResultEvent,
    congestions: Congestions,
    congestedComponents: CongestedComponents,
) extends GridAgentDataInternal {

  /** Builds a [[CongestionResult]] from the power flow results.
    * @param startTime
    *   Of the simulation.
    * @return
    *   An iterable of [[CongestionResult]].
    */
  private def getCongestionResults(
      startTime: ZonedDateTime
  ): Iterable[CongestionResult] = {
    val voltageLimits = gridAgentBaseData.gridEnv.gridModel.voltageLimits

    val nodes = congestedComponents.voltages.map { nodeRes =>
      new CongestionResult(
        startTime.plusSeconds(currentTick),
        nodeRes.getInputModel,
        InputModelType.NODE,
        subgridNo,
        nodeRes.getvMag().to(Units.PERCENT),
        voltageLimits.vMin.to(Units.PERCENT),
        voltageLimits.vMax.to(Units.PERCENT),
      )
    }

    val lines = congestedComponents.lines.map { case (lineModel, current) =>
      val utilisation = Each(current / lineModel.iNom).toPercent.asPercent

      new CongestionResult(
        startTime.plusSeconds(currentTick),
        lineModel.uuid,
        InputModelType.LINE,
        subgridNo,
        utilisation,
        0.asPercent,
        100.asPercent,
      )
    }

    val transformer2W = congestedComponents.transformer2Ws.map {
      case (transformerModel, power) =>
        val utilisation =
          Each(power / transformerModel.sRated).toPercent.asPercent

        new CongestionResult(
          startTime.plusSeconds(currentTick),
          transformerModel.uuid,
          InputModelType.TRANSFORMER_2W,
          subgridNo,
          utilisation,
          0.asPercent,
          100.asPercent,
        )
    }

    val transformer3W = congestedComponents.transformer3Ws.map {
      case (transformerModel, power) =>
        val utilisation =
          Each(power / transformerModel.sRated).toPercent.asPercent

        new CongestionResult(
          startTime.plusSeconds(currentTick),
          transformerModel.uuid,
          InputModelType.TRANSFORMER_3W,
          subgridNo,
          utilisation,
          0.asPercent,
          100.asPercent,
        )
    }

    nodes ++ lines ++ transformer2W ++ transformer3W
  }

  def getAllResults(startTime: ZonedDateTime): PowerFlowResultEvent =
    powerFlowResults + getCongestionResults(startTime)

  lazy val inferiorGridRefs: MultiMap[GridAgentRef, UUID] =
    gridAgentBaseData.inferiorGridRefs

  lazy val superiorGridRefs: MultiMap[GridAgentRef, UUID] =
    gridAgentBaseData.superiorGridRefs
}

object CongestionManagementData {

  def apply(
      gridAgentBaseData: GridAgentBaseData,
      currentTick: Long,
      powerFlowResults: Option[PowerFlowResultEvent] = None,
  ): CongestionManagementData = {
    val results = powerFlowResults.getOrElse(
      PowerFlowResultEvent(
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq.empty,
      )
    )

    val gridModel = gridAgentBaseData.gridEnv.gridModel

    val congestedComponents = CongestedComponents(
      results,
      gridModel.gridComponents,
      gridModel.voltageLimits,
      gridModel.mainRefSystem.nominalVoltage,
      gridModel.subnetNo,
    )

    CongestionManagementData(
      gridAgentBaseData,
      currentTick,
      gridModel.subnetNo,
      results,
      Congestions(congestedComponents),
      congestedComponents,
    )
  }

  /** Creates [[CongestionManagementData]] without power flow results. With this
    * data the congestion management is skipped.
    * @param gridAgentBaseData
    *   Agent base data.
    * @param currentTick
    *   Of the simulation.
    * @return
    *   A new [[CongestionManagementData]].
    */
  def empty(
      gridAgentBaseData: GridAgentBaseData,
      currentTick: Long,
  ): CongestionManagementData = apply(gridAgentBaseData, currentTick, None)
}
