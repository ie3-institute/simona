/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.event.listener.ResultEventListener.ResultMessage
import edu.ie3.simona.ontology.messages.{
  Activation,
  PowerMessage,
  VoltageMessage
}

/** Trait for [[GridAgent]] messages.
  */
sealed trait GridAgentMessage

object GridAgentMessage {

  /** Wrapper for activation values
    *
    * @param activation
    *   the tick
    */
  private[grid] final case class ActivationAdapter(activation: Activation)
      extends GridAgentMessage

  private[grid] final case class PMAdapter(msg: PowerMessage)
      extends GridAgentMessage

  private[grid] final case class VMAdapter(msg: VoltageMessage)
      extends GridAgentMessage

  private[grid] final case class ValuesAdapter(values: ReceivedValues)
      extends GridAgentMessage

  private[grid] final case class ResultMessageAdapter(msg: ResultMessage)
      extends GridAgentMessage

}
