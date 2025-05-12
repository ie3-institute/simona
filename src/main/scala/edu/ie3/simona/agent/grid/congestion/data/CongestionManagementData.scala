/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion.data

import edu.ie3.datamodel.models.result.CongestionResult
import edu.ie3.datamodel.models.result.CongestionResult.InputModelType
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.GridAgentData.{
  GridAgentBaseData,
  GridAgentDataInternal,
}
import edu.ie3.simona.agent.grid.congestion.{CongestedComponents, Congestions}
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.apache.pekko.actor.typed.ActorRef

import java.time.ZonedDateTime
import java.util.UUID
import scala.concurrent.duration.FiniteDuration

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
final case class CongestionManagementData private (
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
    *   AS new [[CongestionResult]].
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
        nodeRes.getvMag().multiply(100),
        voltageLimits.vMin.multiply(100),
        voltageLimits.vMax.multiply(100),
      )
    }

    val lines = congestedComponents.lines.map { case (lineModel, current) =>
      val utilisation = (current / lineModel.iNom).asPercent

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
        val utilisation = (power / transformerModel.sRated).asPercent

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
        val utilisation = (power / transformerModel.sRated).asPercent

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

  def inferiorGridRefs: Map[ActorRef[GridAgent.Request], Seq[UUID]] =
    gridAgentBaseData.inferiorGridRefs

  def superiorGridRefs: Map[ActorRef[GridAgent.Request], Seq[UUID]] =
    gridAgentBaseData.superiorGridRefs

  def timeout: FiniteDuration =
    gridAgentBaseData.congestionManagementParams.timeout
}

object CongestionManagementData {
  def apply(
      gridAgentBaseData: GridAgentBaseData,
      currentTick: Long,
      powerFlowResults: PowerFlowResultEvent,
  ): CongestionManagementData = {
    val gridModel = gridAgentBaseData.gridEnv.gridModel

    val congestedComponents = CongestedComponents(
      powerFlowResults,
      gridModel.gridComponents,
      gridModel.voltageLimits,
      gridModel.mainRefSystem.nominalVoltage,
      gridModel.subnetNo,
    )

    CongestionManagementData(
      gridAgentBaseData,
      currentTick,
      gridModel.subnetNo,
      powerFlowResults,
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
  ): CongestionManagementData = apply(
    gridAgentBaseData,
    currentTick,
    PowerFlowResultEvent(
      Seq.empty,
      Seq.empty,
      Seq.empty,
      Seq.empty,
      Seq.empty,
    ),
  )
}
