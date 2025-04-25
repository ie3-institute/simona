/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.em

import edu.ie3.simona.ontology.messages.flex.FlexOptions
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexRequest,
  FlexResponse,
}
import org.apache.pekko.actor.typed.ActorRef

/** The existence of this data object indicates that the corresponding agent is
  * EM-controlled (by [[emAgent]]).
  *
  * @param emAgent
  *   The parent EmAgent that is controlling this agent.
  * @param flexAdapter
  *   The flex adapter handling [[FlexRequest]] messages
  * @param lastFlexOptions
  *   Last flex options that have been calculated for this agent.
  */
final case class FlexControlledData(
    emAgent: ActorRef[FlexResponse],
    flexAdapter: ActorRef[FlexRequest],
    lastFlexOptions: Option[FlexOptions] = None,
)
