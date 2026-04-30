/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.grid.data.GridAgentData.GridAgentRef
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.model.grid.GridModel
import edu.ie3.util.scala.collection.immutable.RichMultiMap.{MultiMap, valueSet}
import org.apache.pekko.actor.typed.ActorRef

import java.util.UUID

/** Wrapper class containing all information on the grid environment a
  * [[GridAgent]] has access to.
  *
  * @param gridModel
  *   [[GridModel]] with all asset information.
  * @param inferiorConnections
  *   A map of actor refs to all inferior grids.
  * @param superiorConnections
  *   A map of actor refs to all superior grids with the corresponding superior
  *   nodes.
  * @param nodeToAssetAgents
  *   A mapping of all node uuids to a set of asset [[ActorRef]] s at those
  *   nodes.
  * @param refToSubgrid
  *   A mapping of all known references to their subgrid id.
  * @param superiorGridIds
  *   A map of all superior grid uuids to their grid ids.
  */
final case class GridEnvironment(
    gridModel: GridModel,
    inferiorConnections: MultiMap[GridAgentRef, UUID],
    superiorConnections: MultiMap[GridAgentRef, UUID],
    nodeToAssetAgents: MultiMap[UUID, ActorRef[ParticipantAgent.Request]],
    refToSubgrid: Map[GridAgentRef, Int],
    superiorGridIds: Map[UUID, Int],
) {

  lazy val allParticipants: Set[ActorRef[ParticipantAgent.Request]] =
    nodeToAssetAgents.valueSet

  def inferiorNodeUuids: Set[UUID] = inferiorConnections.valueSet

  def superiorNodeUuids: Set[UUID] = superiorConnections.valueSet
}
