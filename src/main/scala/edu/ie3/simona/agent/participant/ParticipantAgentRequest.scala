/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import edu.ie3.simona.agent.participant.ParticipantAgent.ParticipantRequest
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  DepartingEvsResponse,
  FreeLotsResponse,
}
import org.apache.pekko.actor.typed.ActorRef

import java.util.UUID

object ParticipantAgentRequest {

  /** Requests number of free lots from evcs
    *
    * @param tick
    *   The latest tick that the data is requested for
    * @param replyTo
    *   The actor to receive the response
    */
  final case class EvFreeLotsRequest(
      override val tick: Long,
      replyTo: ActorRef[FreeLotsResponse],
  ) extends ParticipantRequest

  /** Requests EV models of departing EVs with given UUIDs
    *
    * @param tick
    *   The latest tick that the data is requested for
    * @param departingEvs
    *   The UUIDs of EVs that are requested
    * @param replyTo
    *   The actor to receive the response
    */
  final case class DepartingEvsRequest(
      override val tick: Long,
      departingEvs: Seq[UUID],
      replyTo: ActorRef[DepartingEvsResponse],
  ) extends ParticipantRequest

}
