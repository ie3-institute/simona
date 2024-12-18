/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.model.participant.evcs.EvModelWrapper
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  ProvisionMessage,
  ServiceInternal,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessageUniversal.ServiceRegistrationMessage
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.{ActorRef => ClassicRef}

import java.util.UUID

sealed trait EvMessage extends ServiceInternal

object EvMessage {

  private[services] trait EvInternal extends EvMessage

  /** Indicate the [[edu.ie3.simona.service.ev.ExtEvDataService]] that the
    * requesting agent wants to receive EV movements
    *
    * @param actorRef
    *   actor ref for the agent to be registered
    * @param evcs
    *   the charging station
    */
  final case class RegisterForEvDataMessage(
      actorRef: ClassicRef,
      evcs: UUID,
  ) extends EvMessage
      with ServiceRegistrationMessage

  trait EvData extends SecondaryData

  /** Provide EV movements for the requested tick
    *
    * @param tick
    *   The tick, for which the data is requested for
    * @param data
    *   Actual information
    * @param nextDataTick
    *   Foreseen next tick, where data is available. Usually, no ticks can be
    *   foreseen within evs
    */
  final case class ProvideEvDataMessage(
      override val tick: Long,
      override val serviceRef: ActorRef[EvMessage],
      override val data: EvData,
      override val nextDataTick: Option[Long],
  ) extends EvMessage
      with ProvisionMessage[EvData]

  /** Requests number of free lots from evcs
    *
    * @param tick
    *   The latest tick that the data is requested for
    */
  final case class EvFreeLotsRequest(tick: Long) extends EvMessage

  /** Requests EV models of departing EVs with given UUIDs
    *
    * @param tick
    *   The latest tick that the data is requested for
    * @param departingEvs
    *   The UUIDs of EVs that are requested
    */
  final case class DepartingEvsRequest(tick: Long, departingEvs: Seq[UUID])
      extends EvMessage

  /** Holds arrivals for one charging station
    *
    * @param arrivals
    *   EVs arriving at the charging station
    */
  final case class ArrivingEvs(
      arrivals: Seq[EvModelWrapper]
  ) extends EvData {}

  trait EvResponseMessage extends EvMessage

  final case class FreeLotsResponse(
      evcs: UUID,
      freeLots: Int,
  ) extends EvResponseMessage

  final case class DepartingEvsResponse(
      evcs: UUID,
      evModels: Seq[EvModelWrapper],
  ) extends EvResponseMessage

}
