/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.powerflow

import edu.ie3.simona.agent.grid.GridAgentMessages.*
import edu.ie3.simona.agent.grid.GridAgentMessages.Responses.{
  ExchangePower,
  ExchangeVoltage,
}
import edu.ie3.simona.agent.grid.data.GridAgentData.GridAgentRef
import edu.ie3.simona.agent.grid.powerflow.ReceivedValuesStore.*
import edu.ie3.simona.agent.grid.{GridAgent, GridEnvironment}
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.util.scala.collection.immutable.RichMultiMap.MultiMap
import org.apache.pekko.actor.typed.ActorRef

import java.util.UUID
import scala.collection.mutable

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
    assetToNode: Map[ActorRef[?], UUID],
    private val missing: mutable.Set[ActorRef[?]],
    private val nodeToReceivedAssetPower: NodeToReceivedPower,
    private val nodeToReceivedGridPower: NodeToReceivedPower,
    private val nodeToReceivedSlackVoltage: NodeToReceivedSlackVoltage,
    private val failedPowerFlows: mutable.Set[FailedPowerFlow],
) {

  def addGridPower(
      ref: ActorRef[?],
      exchangePower: Iterable[ExchangePower],
  ): Unit = {
    missing.remove(ref)

    exchangePower.foreach { exchangePower =>
      nodeToReceivedGridPower
        .getOrElseUpdate(exchangePower.nodeUuid, mutable.Set.empty)
        .add(exchangePower)
    }
  }

  def addFailedPowerFlow(failedPowerFlow: FailedPowerFlow): Unit = {
    missing.remove(failedPowerFlow.sender)
    failedPowerFlows.add(failedPowerFlow)
  }

  def addAssetPower(ref: ActorRef[?], response: PowerResponse): Unit = {
    missing.remove(ref)
    nodeToReceivedAssetPower
      .getOrElseUpdate(assetToNode(ref), mutable.Set.empty)
      .add(response)
  }

  def addSlackVoltage(
      sender: GridAgentRef,
      nodalSlackVoltages: Seq[ExchangeVoltage],
  ): Unit = {
    nodalSlackVoltages.foreach { exchangeVoltage =>
      val node = exchangeVoltage.nodeUuid

      nodeToReceivedSlackVoltage.get(node) match {
        case Some(None) =>
          nodeToReceivedSlackVoltage.put(
            exchangeVoltage.nodeUuid,
            Some(exchangeVoltage),
          )

        case Some(Some(_)) =>
          throw new RuntimeException(
            s"Already received slack value for node ${exchangeVoltage.nodeUuid}!"
          )
        case None =>
          throw new RuntimeException(
            s"Received slack value for node ${exchangeVoltage.nodeUuid} from $sender which is not in my slack values nodes list!"
          )
      }
    }
  }

  def nodeToReceivedPower: Map[UUID, Set[PowerResponse]] = {
    val keys = nodeToReceivedAssetPower.keySet ++ nodeToReceivedGridPower.keySet

    keys.map { key =>
      val assetOption =
        nodeToReceivedAssetPower.getOrElse(key, mutable.Set.empty).toSet
      val gridOption =
        nodeToReceivedGridPower.getOrElse(key, mutable.Set.empty).toSet

      key -> (assetOption ++ gridOption)
    }.toMap
  }

  def getSlackVoltage(node: UUID): Option[ExchangeVoltage] =
    nodeToReceivedSlackVoltage.get(node).flatten

  def nodeToSlackVoltage: Map[UUID, Option[ExchangeVoltage]] =
    nodeToReceivedSlackVoltage.toMap

  def slackVoltages: Seq[ExchangeVoltage] =
    nodeToReceivedSlackVoltage.values.flatten.toSeq

  def clearAssetPower(): Unit = {
    nodeToReceivedAssetPower.values.foreach(_.clear())
    missing.addAll(assetToNode.keySet)
  }

  def clearSlackVoltages(): Unit = {
    nodeToReceivedSlackVoltage.keySet.foreach { key =>
      nodeToReceivedSlackVoltage.put(key, None)
    }
  }

  def hasFailedPowerFlow: Boolean = failedPowerFlows.nonEmpty

  def allAssetAndGridPowerValuesReady: Boolean = missing.isEmpty

  def allSlackVoltagesReceived: Boolean =
    nodeToReceivedSlackVoltage.values.forall(_.isDefined)

  def hasAssetPowerChanged: Boolean = nodeToReceivedAssetPower.values.exists(
    _.exists(_.isInstanceOf[AssetPowerChangedMessage])
  )

  def getExpectedPowerResponses: Set[ActorRef[?]] = missing.toSet
}

object ReceivedValuesStore {

  private type NodeToReceivedPower =
    mutable.Map[UUID, mutable.Set[PowerResponse]]
  private type NodeToReceivedSlackVoltage =
    mutable.Map[UUID, Option[ExchangeVoltage]]

  def empty(gridEnv: GridEnvironment): ReceivedValuesStore = empty(
    gridEnv.nodeToAssetAgents,
    gridEnv.inferiorConnections,
    gridEnv.superiorNodeUuids,
  )

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
      inferiorConnections: MultiMap[GridAgentRef, UUID],
      superiorNodeUuids: Set[UUID],
  ): ReceivedValuesStore = {
    val assetToNode: Map[ActorRef[?], UUID] = nodeToAssetAgents.flatMap {
      case (node, refs) =>
        refs.map(ref => ref -> node)
    }

    val missing = mutable.Set.empty[ActorRef[?]]
    missing.addAll(assetToNode.keySet)
    missing.addAll(inferiorConnections.keySet)

    val nodeToGridRef = inferiorConnections
      .flatMap { case (ref, nodes) =>
        nodes.map(node => (node, ref))
      }
      .groupMap(_._1)(_._2)

    val slackVoltageMap: NodeToReceivedSlackVoltage = mutable.Map.from {
      superiorNodeUuids.map(n => n -> None)
    }

    ReceivedValuesStore(
      assetToNode,
      missing,
      mutable.Map.empty,
      mutable.Map.empty,
      slackVoltageMap,
      mutable.Set.empty,
    )
  }

}
