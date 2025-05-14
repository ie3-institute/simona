/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion.detection

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.GridAgent.{
  InternalReply,
  InternalReplyWithSender,
  InternalRequest,
}
import edu.ie3.simona.agent.grid.congestion.Congestions
import org.apache.pekko.actor.typed.ActorRef

object DetectionMessages {

  /** Request for congestion the inferior grid.
    * @param sender
    *   that is asking
    */
  final case class CongestionCheckRequest(
      sender: ActorRef[GridAgent.Request]
  ) extends InternalRequest

  /** Response with congestions from an inferior grid.
    * @param sender
    *   inferior grid ref
    * @param value
    *   congestions in the inferior grid
    */
  final case class CongestionResponse(
      override val sender: ActorRef[GridAgent.Request],
      override val value: Congestions,
  ) extends InternalReplyWithSender[Congestions]

  /** Answer with all congestion in all inferior grids.
    * @param values
    *   vector of congestion in inferior grids
    */
  final case class ReceivedCongestions(
      values: Seq[(ActorRef[GridAgent.Request], Congestions)]
  ) extends InternalReply
}
