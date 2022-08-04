/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import akka.actor.ActorRef
import edu.ie3.datamodel.exceptions.InvalidGridException
import edu.ie3.datamodel.models.input.connector.ConnectorInput
import edu.ie3.datamodel.models.input.system.{EmInput, SystemParticipantInput}
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.grid.NodeModel
import edu.ie3.simona.model.participant.EmModel.EmRelevantData
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.OperationInterval
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.Power

final case class EmModel private (
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    scalingFactor: Double,
    qControl: QControl,
    connectedAgents: Seq[(ActorRef, SystemParticipantInput)]
) extends SystemParticipant[EmRelevantData](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      Quantities.getQuantity(0, PowerSystemUnits.KILOWATT), // FIXME dummy
      0 // FIXME dummy
    ) {

  /** Determine the power of controllable devices such as storages
    * @return
    */
  def determineDeviceControl(
      data: EmRelevantData
  ): Map[UUID, ComparableQuantity[Power]] = ???

  /** Calculate the active power behaviour of the model
    *
    * @param data
    *   Further needed, secondary data
    * @return
    *   Active power
    */
  protected def calculateActivePower(
      data: EmRelevantData
  ): ComparableQuantity[Power] = ???
}

case object EmModel {

  /** Class that holds all relevant data for Energy Management calculation
    *
    * @param dateTime
    *   date and time of the <b>ending</b> of time frame to calculate
    * @param weatherDataFrameLength
    *   the duration in ticks (= seconds) the provided irradiance is received by
    *   the pv panel
    */
  final case class EmRelevantData(
      // TODO: From PvModel, Check and refactor
      dateTime: ZonedDateTime,
      weatherDataFrameLength: Long,
      lastResults: ValueStore[ApparentPower]
  ) extends CalcRelevantData

  def apply(
      inputModel: EmInput,
      modelConfig: EmRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      connectedAgents: Seq[(ActorRef, SystemParticipantInput)]
  ): EmModel = {
    /* Determine the operation interval */
    val operationInterval: OperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        inputModel.getOperationTime
      )

    EmModel(
      inputModel.getUuid,
      inputModel.getId,
      operationInterval,
      modelConfig.scaling,
      QControl(inputModel.getqCharacteristics),
      connectedAgents
    )
  }
  // TODO:
  /** Checks the availability of node calculation models, that are connected by
    * the given [[ConnectorInput]]. If not both models can be found,
    * [[InvalidGridException]] s are thrown
    *
    * @param connector
    *   Connector, that connects the two queried nodes
    * @param nodes
    *   [[Array]] of [[NodeModel]] calculation models
    * @return
    *   A tuple of both connected nodes
    */
  private def getConnectedNodes(
      connector: ConnectorInput,
      nodes: Set[NodeModel]
  ): (NodeModel, NodeModel) = {
    val nodeAOpt: Option[NodeModel] =
      nodes.find(_.uuid.equals(connector.getNodeA.getUuid))
    val nodeBOpt: Option[NodeModel] =
      nodes.find(_.uuid.equals(connector.getNodeB.getUuid))

    (nodeAOpt, nodeBOpt) match {
      case (Some(nodeA), Some(nodeB)) =>
        (nodeA, nodeB)
      case (None, Some(_)) =>
        throw new InvalidGridException(
          s"NodeA: ${connector.getNodeA.getUuid} for connector ${connector.getUuid} cannot be found."
        )
      case (Some(_), None) =>
        throw new InvalidGridException(
          s"NodeB: ${connector.getNodeB.getUuid} for connector ${connector.getUuid} cannot be found."
        )
      case _ =>
        throw new InvalidGridException(
          s"Nodes (nodeA: ${connector.getNodeA.getUuid}, nodeB: ${connector.getNodeB.getUuid})for connector ${connector.getUuid} cannot be found."
        )
    }
  }

  // TODO: Refactor:
  // Questions: From where does the EmAgent know which participants it controls? -> EmScheduler

  /** Checks the availability of calculation models of connected participants to
    * the Energy Management Agent, that are connected by the given
    * [[ConnectorInput]]. If not both models can be found,
    * [[InvalidGridException]] s are thrown
    *
    * @param connector
    *   Connector, that connects the two queried nodes
    * @param nodes
    *   [[Array]] of [[NodeModel]] calculation models
    * @return
    *   A tuple of both connected nodes
    */
  private def getConnectedParticipants(
      connector: ConnectorInput,
      nodes: Set[NodeModel]
  ): (NodeModel, NodeModel) = {
    val nodeAOpt: Option[NodeModel] =
      nodes.find(_.uuid.equals(connector.getNodeA.getUuid))
    val nodeBOpt: Option[NodeModel] =
      nodes.find(_.uuid.equals(connector.getNodeB.getUuid))

    (nodeAOpt, nodeBOpt) match {
      case (Some(nodeA), Some(nodeB)) =>
        (nodeA, nodeB)
      case (None, Some(_)) =>
        throw new InvalidGridException(
          s"NodeA: ${connector.getNodeA.getUuid} for connector ${connector.getUuid} cannot be found."
        )
      case (Some(_), None) =>
        throw new InvalidGridException(
          s"NodeB: ${connector.getNodeB.getUuid} for connector ${connector.getUuid} cannot be found."
        )
      case _ =>
        throw new InvalidGridException(
          s"Nodes (nodeA: ${connector.getNodeA.getUuid}, nodeB: ${connector.getNodeB.getUuid})for connector ${connector.getUuid} cannot be found."
        )
    }
  }

}
