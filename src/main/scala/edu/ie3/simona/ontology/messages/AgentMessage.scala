/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages

import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexActivation,
  FlexRequest,
}

object AgentMessage {

  /** Type unionizing all messages that an agent can receive as activation.
    */
  type ActivationRequest = Activation | FlexRequest

  /** Extension method for the `Activation` and `FlexRequest` types to retrieve
    * the tick associated with the activation.
    */
  extension (activation: ActivationRequest) {
    def tick: Long =
      activation match {
        case a: Activation  => a.tick
        case f: FlexRequest => f.tick
      }

    def force: Boolean = activation match {
      case f: FlexActivation => f.force
      case _                 => false
    }
  }

}
