/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import edu.ie3.datamodel.models.result.CongestionResult
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.GridAgentData.{
  GridAgentBaseData,
  GridAgentDataInternal,
}
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import org.apache.pekko.actor.typed.ActorRef

import java.time.ZonedDateTime
import java.util.UUID
import scala.concurrent.duration.FiniteDuration

object CMData {

  /** Case class that holds all received data.
    * @param inferiorGridMap
    *   map: inferior grid to received data
    * @tparam T
    *   type of data
    */
  final case class AwaitingData[T] private (
      inferiorGridMap: Map[ActorRef[GridAgent.Request], Option[T]]
  ) {

    /** Returns true if congestion data from inferior grids is expected and no
      * data was received yet.
      */
    def notDone: Boolean =
      inferiorGridMap.values.exists(_.isEmpty)

    /** Returns the received values
      */
    def values: Iterable[T] = inferiorGridMap.values.flatten.toSeq

    /** Return the mapping of all received values. This should only be called if
      * [[notDone]] == false
      */
    def mappedValues: Map[ActorRef[GridAgent.Request], T] =
      inferiorGridMap.flatMap { case (ref, option) =>
        option.map(value => ref -> value)
      }

    /** Method for updating the data with received data.
      * @param sender
      *   actor ref of the sender
      * @param data
      *   send data
      * @return
      *   an updated object
      */
    def update(sender: ActorRef[GridAgent.Request], data: T): AwaitingData[T] =
      handleReceivingData(Vector((sender, data)))

    /** Method for updating the data with the received data.
      *
      * @param receivedData
      *   data that was received
      * @return
      *   a updated copy of this data
      */
    def handleReceivingData(
        receivedData: Vector[(ActorRef[GridAgent.Request], T)]
    ): AwaitingData[T] = {
      val mappedData = receivedData.map { case (ref, value) =>
        ref -> Some(value)
      }.toMap
      copy(inferiorGridMap = inferiorGridMap ++ mappedData)
    }
  }

  object AwaitingData {
    def apply[T](
        inferiorGridRefs: Set[ActorRef[GridAgent.Request]]
    ): AwaitingData[T] = {
      AwaitingData(inferiorGridRefs.map(ref => ref -> None).toMap)
    }
  }

  /** State data of a grid agent during the congestion management.
    * @param gridAgentBaseData
    *   agent base data
    * @param currentTick
    *   current tick used for additional power flow calculations
    * @param subgridNo
    *   the number of the subgrid
    * @param powerFlowResults
    *   result of the previous power flow calculation
    * @param congestions
    *   the found congestions
    */
  final case class CongestionManagementData private (
      gridAgentBaseData: GridAgentBaseData,
      currentTick: Long,
      subgridNo: Int,
      powerFlowResults: PowerFlowResultEvent,
      congestions: Congestions,
  ) extends GridAgentDataInternal {

    /** Builds a [[CongestionResult]] from the power flow results.
      * @param startTime
      *   of the simulation
      * @return
      *   a new [[CongestionResult]]
      */
    def getCongestionResult(startTime: ZonedDateTime): CongestionResult = {
      val gridModel = gridAgentBaseData.gridEnv.gridModel

      new CongestionResult(
        startTime.plusSeconds(currentTick),
        gridModel.subnetNo,
        gridModel.voltageLimits.vMin,
        gridModel.voltageLimits.vMax,
        congestions.voltageCongestions,
        congestions.lineCongestions,
        congestions.transformerCongestions,
      )
    }

    def getAllResults(startTime: ZonedDateTime): PowerFlowResultEvent =
      powerFlowResults + getCongestionResult(startTime)

    def inferiorGridRefs: Map[ActorRef[GridAgent.Request], Seq[UUID]] =
      gridAgentBaseData.inferiorGridRefs(false)

    def superiorGridRefs: Map[ActorRef[GridAgent.Request], Seq[UUID]] =
      gridAgentBaseData.superiorGridRefs(false)

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

      CongestionManagementData(
        gridAgentBaseData,
        currentTick,
        gridModel.subnetNo,
        powerFlowResults,
        Congestions(
          powerFlowResults,
          gridModel.gridComponents,
          gridModel.voltageLimits,
          gridModel.mainRefSystem.nominalVoltage,
          gridModel.subnetNo,
        ),
      )
    }

    /** Creates [[CongestionManagementData]] without power flow results. With
      * this data the congestion management is skipped.
      * @param gridAgentBaseData
      *   agent base data
      * @param currentTick
      *   of the simulation
      * @return
      *   a new [[CongestionManagementData]]
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

}
