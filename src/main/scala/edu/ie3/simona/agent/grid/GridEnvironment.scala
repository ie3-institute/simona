/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.simona.agent.participant2.ParticipantAgent
import edu.ie3.simona.model.grid.GridModel
import org.apache.pekko.actor.typed.ActorRef

import java.util.UUID

/** Wrapper class containing all information on the grid environment a
  * [[GridAgent]] has access to
  *
  * @param gridModel
  *   [[GridModel]] with all asset information
  * @param subgridGateToActorRef
  *   a mapping of all [[SubGridGate]] s to their corresponding [[ActorRef]] s
  * @param nodeToAssetAgents
  *   a mapping of all node uuids to a set of asset [[ActorRef]] s at those
  *   nodes
  */
final case class GridEnvironment(
    gridModel: GridModel,
    subgridGateToActorRef: Map[SubGridGate, ActorRef[GridAgent.Request]],
    nodeToAssetAgents: Map[UUID, Set[ActorRef[ParticipantAgent.Request]]],
)
