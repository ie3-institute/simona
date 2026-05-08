/*
 * © 2025-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion.mitigations

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.GridAgent.{
  InternalReply,
  InternalReplyWithSender,
  InternalRequest,
}
import edu.ie3.simona.agent.grid.congestion.VoltageRange
import edu.ie3.simona.model.grid.TransformerTapping
import org.apache.pekko.actor.typed.ActorRef
import squants.Dimensionless

object TappingMessages {

  /** Request for voltage options in the inferior grid.
    * @param sender
    *   That is asking.
    * @param subgrid
    *   Subgrid of the sender.
    */
  final case class RequestVoltageOptions(
      sender: ActorRef[GridAgent.Message],
      subgrid: Int,
  ) extends InternalRequest

  /** Response with voltage options of the inferior grid.
    * @param sender
    *   Inferior grid ref.
    * @param value
    *   Consisting of the voltage range and a set of all transformers to the
    *   superior grid.
    */
  final case class VoltageRangeResponse(
      override val sender: ActorRef[GridAgent.Message],
      override val value: (VoltageRange, Set[TransformerTapping]),
  ) extends InternalReplyWithSender[(VoltageRange, Set[TransformerTapping])]

  /** Message to an inferior grid with the voltage change after the transformers
    * are tapped.
    * @param delta
    *   Voltage change.
    */
  final case class VoltageDeltaResponse(
      delta: Dimensionless
  ) extends InternalReply

}
