/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.model.grid.GridModel
import org.apache.pekko.actor.typed.ActorRef

import java.util.UUID

/** Wrapper class containing all information on the grid environment a
  * [[GridAgent]] has access to.
  *
  * @param gridModel
  *   [[GridModel]] with all asset information.
  * @param nodeToAssetAgents
  *   A mapping of all node uuids to a set of asset [[ActorRef]] s at those
  *   nodes.
  * @param inferiorConnections
  *   A map of actor refs to all inferior grids.
  * @param superiorConnections
  *   A map of actor refs to all superior grids with the corresponding superior
  *   nodes.
  * @param superiorGridIds
  *   A set of all superior grid ids.
  */
final case class GridEnvironment(
    gridModel: GridModel,
    nodeToAssetAgents: Map[UUID, Set[ActorRef[ParticipantAgent.Request]]],
    inferiorConnections: Map[ActorRef[GridAgent.Message], Set[UUID]] =
      Map.empty,
    superiorConnections: Map[ActorRef[GridAgent.Message], Set[UUID]] =
      Map.empty,
    superiorGridIds: Set[Int] = Set.empty,
) {

  def inferiorNodeUuids: Set[UUID] = inferiorConnections.values.flatten.toSet

  def superiorNodeUuids: Set[UUID] = superiorConnections.values.flatten.toSet
}
