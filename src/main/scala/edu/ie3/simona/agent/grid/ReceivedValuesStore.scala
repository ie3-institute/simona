/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.simona.agent.grid.GridAgentMessages.{
  GridPowerResponse,
  PowerResponse,
}
import edu.ie3.simona.agent.grid.GridAgentMessages.Responses.ExchangeVoltage
import edu.ie3.simona.agent.grid.ReceivedValuesStore.{
  NodeToReceivedPower,
  NodeToReceivedSlackVoltage,
}
import edu.ie3.simona.agent.participant.ParticipantAgent.ParticipantMessage
import org.apache.pekko.actor.typed.ActorRef

import java.util.UUID

/** Value store that contains all data that should be received by the
  * [[GridAgent]] from other agents. The mapping is structured as the uuid of a
  * node to a tuple of either a vector of actorRefs to
  * Option[ProvidePowerMessage] or single
  * Option[SlackVoltageRequestResponseMessage]. That said, initially all Options
  * are set to None until the data is updated with received values.
  *
  * If a map is empty this indicates, that there is no value expected from
  * either assets/grids ([[nodeToReceivedPower]]) or grids
  * [[nodeToReceivedSlackVoltage]]
  *
  * @param nodeToReceivedPower
  *   mapping of node uuids to received p/q values from inferior [[GridAgent]] s
  *   if any and [[edu.ie3.simona.agent.participant.ParticipantAgent]] (==
  *   assets) if any
  * @param nodeToReceivedSlackVoltage
  *   mapping of node uuids to received slack voltages from superior
  *   [[GridAgent]] s if any
  */
final case class ReceivedValuesStore private (
    nodeToReceivedPower: NodeToReceivedPower,
    nodeToReceivedSlackVoltage: NodeToReceivedSlackVoltage,
)

object ReceivedValuesStore {

  type NodeToReceivedPower =
    Map[UUID, Map[ActorRef[_], Option[PowerResponse]]]
  type NodeToReceivedSlackVoltage =
    Map[UUID, Option[ExchangeVoltage]]

  /** Get an empty, ready to be used instance of [[ReceivedValuesStore]]
    * containing an `empty` mapping of [[NodeToReceivedPower]] and
    * [[NodeToReceivedSlackVoltage]] with all options set to `None` (see
    * [[ReceivedValuesStore]] for details)
    *
    * @param nodeToAssetAgents
    *   mapping of node uuids to [[ActorRef]] s of the asset agents that are
    *   located at the specific node
    * @param inferiorSubGridGateToActorRef
    *   mapping of all inferior [[SubGridGate]] s to the [[ActorRef]] of the
    *   grid agent that is located there
    * @param superiorGridNodeUuids
    *   node uuids of the superior [[GridAgent]] s
    * @return
    *   `empty` [[ReceivedValuesStore]] with pre-initialized options as `None`
    */
  def empty(
      nodeToAssetAgents: Map[UUID, Set[ActorRef[ParticipantMessage]]],
      inferiorSubGridGateToActorRef: Map[SubGridGate, ActorRef[
        GridAgent.Request
      ]],
      superiorGridNodeUuids: Vector[UUID],
  ): ReceivedValuesStore = {
    val (nodeToReceivedPower, nodeToReceivedSlackVoltage) =
      buildEmptyReceiveMaps(
        nodeToAssetAgents,
        inferiorSubGridGateToActorRef,
        superiorGridNodeUuids,
      )
    ReceivedValuesStore(nodeToReceivedPower, nodeToReceivedSlackVoltage)
  }

  /** Composes an empty [[NodeToReceivedPower]] with all options pre-initialized
    * to `None`
    *
    * @param nodeToAssetAgents
    *   mapping of node uuids to [[ActorRef]] s of the asset agents that are
    *   located at the specific node
    * @param inferiorSubGridGateToActorRef
    *   mapping of all inferior [[SubGridGate]] s to the [[ActorRef]] of the
    *   grid agent that is located there
    * @return
    *   `empty` [[NodeToReceivedPower]] with pre-initialized options as `None`
    */
  private def buildEmptyNodeToReceivedPowerMap(
      nodeToAssetAgents: Map[UUID, Set[ActorRef[ParticipantMessage]]],
      inferiorSubGridGateToActorRef: Map[SubGridGate, ActorRef[
        GridAgent.Request
      ]],
  ): NodeToReceivedPower = {
    /* Collect everything, that I expect from my asset agents */
    val assetsToReceivedPower: NodeToReceivedPower = nodeToAssetAgents.collect {
      case (uuid: UUID, actorRefs: Set[ActorRef[ParticipantMessage]]) =>
        (uuid, actorRefs.map(actorRef => actorRef -> None).toMap)
    }

    /* Add everything, that I expect from my subordinate grid agents. */
    inferiorSubGridGateToActorRef
      .map { case (gate, reference) =>
        gate.superiorNode.getUuid -> reference
      }
      .foldLeft(assetsToReceivedPower) {
        case (
              subordinateToReceivedPower,
              couplingNodeUuid -> inferiorSubGridRef,
            ) =>
          /* Check, if there is already something expected for the given coupling node
           * and add reference to the subordinate grid agent */
          val actorRefToMessage = subordinateToReceivedPower
            .getOrElse(
              couplingNodeUuid,
              Map.empty[ActorRef[_], Option[GridPowerResponse]],
            ) + (inferiorSubGridRef -> None)

          /* Update the existing map */
          subordinateToReceivedPower + (couplingNodeUuid -> actorRefToMessage)
      }
  }

  /** Composes an empty [[NodeToReceivedSlackVoltage]] with all options
    * pre-initialized to `None`
    *
    * @param superiorGridNodeUuids
    *   node uuids of the superior [[GridAgent]] s nodes
    * @return
    *   `empty` [[NodeToReceivedSlackVoltage]] with pre-initialized options as
    *   `None`
    */
  private def buildEmptyNodeToReceivedSlackVoltageValuesMap(
      superiorGridNodeUuids: Vector[UUID]
  ): NodeToReceivedSlackVoltage = {
    superiorGridNodeUuids.map(nodeId => nodeId -> None).toMap
  }

  /** Composing method that combines [[buildEmptyNodeToReceivedPowerMap()]] and
    * [[buildEmptyNodeToReceivedSlackVoltageValuesMap()]]
    *
    * @param nodeToAssetAgents
    *   mapping of node uuids to [[ActorRef]] s of the asset agents that are
    *   located at the specific node
    * @param inferiorSubGridGateToActorRef
    *   mapping of all inferior [[SubGridGate]] s to the [[ActorRef]] of the
    *   grid agent that is located there
    * @param superiorGridNodeUuids
    *   node uuids of the superior [[GridAgent]] s
    * @return
    *   `empty` [[NodeToReceivedSlackVoltage]] and [[NodeToReceivedPower]]
    */
  private def buildEmptyReceiveMaps(
      nodeToAssetAgents: Map[UUID, Set[ActorRef[ParticipantMessage]]],
      inferiorSubGridGateToActorRef: Map[SubGridGate, ActorRef[
        GridAgent.Request
      ]],
      superiorGridNodeUuids: Vector[UUID],
  ): (NodeToReceivedPower, NodeToReceivedSlackVoltage) = {
    (
      buildEmptyNodeToReceivedPowerMap(
        nodeToAssetAgents,
        inferiorSubGridGateToActorRef,
      ),
      buildEmptyNodeToReceivedSlackVoltageValuesMap(superiorGridNodeUuids),
    )
  }

}
