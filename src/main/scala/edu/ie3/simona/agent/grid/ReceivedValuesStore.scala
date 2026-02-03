/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.simona.agent.grid.GridAgentMessages.*
import edu.ie3.simona.agent.grid.GridAgentMessages.Responses.ExchangeVoltage
import edu.ie3.simona.agent.grid.ReceivedValuesStore.*
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.util.CollectionUtils.emptyOptionMap
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
  * [[nodeToReceivedSlackVoltage]].
  *
  * @param nodeToReceivedAssetPower
  *   Mapping of node uuids to received p/q values from
  *   [[edu.ie3.simona.agent.participant.ParticipantAgent]] (== assets) if any.
  * @param nodeToReceivedGridPower
  *   Mapping of node uuids to received p/q values from inferior [[GridAgent]]s
  *   if any.
  * @param nodeToReceivedSlackVoltage
  *   Mapping of node uuids to received slack voltages from superior
  *   [[GridAgent]] s if any.
  */
final case class ReceivedValuesStore(
    nodeToReceivedAssetPower: NodeToReceivedPower,
    nodeToReceivedGridPower: NodeToReceivedPower,
    nodeToReceivedSlackVoltage: NodeToReceivedSlackVoltage,
) {

  def nodeToReceivedPower: NodeToReceivedPower =
    nodeToReceivedAssetPower.foldLeft(nodeToReceivedGridPower) {
      case (results, (node, refMap)) =>
        results.updated(node, results.getOrElse(node, Map.empty) ++ refMap)
    }

  def clearAssetPower: ReceivedValuesStore = {
    val updatedMap = nodeToReceivedAssetPower.map { case (node, refMap) =>
      node -> refMap.map { case (ref, _) => ref -> None }
    }

    copy(nodeToReceivedAssetPower = updatedMap)
  }

  def hasAssetPowerChanged: Boolean = nodeToReceivedAssetPower.values.exists(
    _.values.exists(_.exists(_.isInstanceOf[AssetPowerChangedMessage]))
  )
}

object ReceivedValuesStore {

  type NodeToReceivedPower =
    Map[UUID, Map[ActorRef[?], Option[PowerResponse]]]
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
      nodeToAssetAgents: Map[UUID, Set[ActorRef[ParticipantAgent.Request]]],
      inferiorSubGridGateToActorRef: Map[SubGridGate, ActorRef[
        GridAgent.Message
      ]],
      superiorGridNodeUuids: Vector[UUID],
  ): ReceivedValuesStore = {
    val (
      nodeToReceivedAssetPower,
      nodeToReceivedGridPower,
      nodeToReceivedSlackVoltage,
    ) =
      buildEmptyReceiveMaps(
        nodeToAssetAgents,
        inferiorSubGridGateToActorRef,
        superiorGridNodeUuids,
      )
    ReceivedValuesStore(
      nodeToReceivedAssetPower,
      nodeToReceivedGridPower,
      nodeToReceivedSlackVoltage,
    )
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
      nodeToAssetAgents: Map[UUID, Set[ActorRef[ParticipantAgent.Request]]],
      inferiorSubGridGateToActorRef: Map[SubGridGate, ActorRef[
        GridAgent.Message
      ]],
      superiorGridNodeUuids: Vector[UUID],
  ): (NodeToReceivedPower, NodeToReceivedPower, NodeToReceivedSlackVoltage) = {
    val assetsToReceivedPower: NodeToReceivedPower = emptyOptionMap(
      nodeToAssetAgents
    )

    /* Add everything, that I expect from my subordinate grid agents. */
    val nodeToGridRef = inferiorSubGridGateToActorRef
      .map { case (gate, reference) =>
        gate.superiorNode.getUuid -> reference
      }
      .groupMap(_._1)(_._2)

    val gridToReceivedPower: NodeToReceivedPower = emptyOptionMap(nodeToGridRef)

    (
      assetsToReceivedPower,
      gridToReceivedPower,
      superiorGridNodeUuids.map(nodeId => nodeId -> None).toMap,
    )
  }

}
