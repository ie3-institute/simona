/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import edu.ie3.simona.agent.participant.ParticipantAgent.ParticipantRequest
import edu.ie3.simona.model.participant.evcs.EvModelWrapper
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ServiceResponseMessage
import edu.ie3.simona.service.Data.SecondaryData
import org.apache.pekko.actor.typed.ActorRef

import java.util.UUID

sealed trait EvMessage

object EvMessage {

  private[services] trait EvInternal extends EvMessage

  trait EvData extends SecondaryData

  /** Requests number of free lots from evcs
    *
    * @param tick
    *   The latest tick that the data is requested for
    * @param replyTo
    *   The actor to receive the response
    */
  final case class EvFreeLotsRequest(
      override val tick: Long,
      replyTo: ActorRef[EvMessage],
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
      replyTo: ActorRef[EvMessage],
  ) extends ParticipantRequest

  /** Holds arrivals for one charging station
    *
    * @param arrivals
    *   EVs arriving at the charging station
    */
  final case class ArrivingEvs(
      arrivals: Seq[EvModelWrapper]
  ) extends EvData

  final case class FreeLotsResponse(
      evcs: UUID,
      freeLots: Int,
  ) extends ServiceResponseMessage

  final case class DepartingEvsResponse(
      evcs: UUID,
      evModels: Seq[EvModelWrapper],
  ) extends ServiceResponseMessage

}
