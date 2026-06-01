/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion.detection

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.GridAgent.{
  InternalReplyWithSender,
  InternalRequest,
}
import edu.ie3.simona.agent.grid.congestion.Congestions
import org.apache.pekko.actor.typed.ActorRef

object DetectionMessages {

  /** Request for congestion the inferior grid.
    * @param sender
    *   That is asking.
    */
  final case class CongestionCheckRequest(
      sender: ActorRef[GridAgent.Message]
  ) extends InternalRequest

  /** Response with congestions from an inferior grid.
    * @param sender
    *   Inferior grid ref.
    * @param value
    *   Congestions in the inferior grid.
    */
  final case class CongestionResponse(
      override val sender: ActorRef[GridAgent.Message],
      override val value: Congestions,
  ) extends InternalReplyWithSender[Congestions]
}
