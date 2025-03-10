/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant2.ParticipantAgent.ParticipantRequest
import edu.ie3.simona.model.participant2.evcs.EvModelWrapper
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{DataResponseMessage, ServiceRegistrationMessage}
import org.apache.pekko.actor.ActorRef

import java.util.UUID

sealed trait EvMessage

object EvMessage {

  /** Indicate the [[edu.ie3.simona.service.ev.ExtEvDataService]] that the
    * requesting agent wants to receive EV movements
    *
    * @param requestingActor
    *   The actor requesting registration for weather data
    * @param evcs
    *   the charging station
    */
  final case class RegisterForEvDataMessage(
      requestingActor: ActorRef,
      evcs: UUID,
  ) extends EvMessage
      with ServiceRegistrationMessage

  trait EvData extends SecondaryData

  /** Requests number of free lots from evcs
    *
    * @param tick
    *   The latest tick that the data is requested for
    * @param replyTo
    *   The actor to receive the response
    */
  final case class EvFreeLotsRequest(override val tick: Long, replyTo: ActorRef)
      extends ParticipantRequest

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
      replyTo: ActorRef,
  ) extends ParticipantRequest

  /** Holds arrivals for one charging station
    *
    * @param arrivals
    *   EVs arriving at the charging station
    */
  final case class ArrivingEvs(
      arrivals: Seq[EvModelWrapper]
  ) extends EvData

  trait EvResponseMessage extends EvMessage with DataResponseMessage

  final case class FreeLotsResponse(
      evcs: UUID,
      freeLots: Int,
  ) extends EvResponseMessage

  final case class DepartingEvsResponse(
      evcs: UUID,
      evModels: Seq[EvModelWrapper],
  ) extends EvResponseMessage

}
