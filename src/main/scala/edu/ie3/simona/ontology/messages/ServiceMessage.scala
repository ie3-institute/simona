/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages

import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.api.ontology.DataMessageFromExt
import edu.ie3.simona.model.participant.ParticipantModel.AdditionalFactoryData
import edu.ie3.simona.model.participant.evcs.EvModelWrapper
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexRequest,
  FlexResponse,
}
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.service.Data.PrimaryDataExtra
import edu.ie3.simona.service.{Data, DataTimeType}
import org.apache.pekko.actor.typed.ActorRef

import java.util.UUID

/** Collections of all messages, that are send to and from the different
  * services
  */
sealed trait ServiceMessage

object ServiceMessage {

  /** Messages sent by a service to an agent, in part as a response to a
    * [[ServiceMessage]].
    */
  trait Response

  /** Message used to register for a service.
    */
  trait ServiceRegistrationMessage extends ServiceMessage

  /** Indicate a [[edu.ie3.simona.service.SimonaService]] that the requesting
    * agent wants to be registered for the specific service.
    *
    * @param requestingActor
    *   The actor requesting registration for the data service.
    * @param dataTimeType
    *   The data type specifying the temporal dimension of the requested data.
    * @param data
    *   The data, that is used during the registration.
    */
  final case class SecondaryServiceRegistrationMessage(
      requestingActor: ActorRef[Response],
      dataTimeType: DataTimeType,
      data: Any,
  ) extends ServiceRegistrationMessage

  /** Message to register an energy management agent with an energy management
    * service.
    * @param requestingActor
    *   The actor to register.
    * @param inputUuid
    *   The uuid of the actor.
    * @param parentEm
    *   An option for the parent actor of the requesting actor.
    * @param parentUuid
    *   An option for the uuid of the parent actor.
    */
  final case class EmServiceRegistration(
      requestingActor: ActorRef[EmAgent.Message],
      inputUuid: UUID,
      parentEm: Option[ActorRef[FlexResponse]] = None,
      parentUuid: Option[UUID] = None,
  ) extends ServiceRegistrationMessage

  /** Message to register with a primary data service.
    *
    * @param requestingActor
    *   The actor requesting registration for primary data
    * @param inputModelUuid
    *   Identifier of the input model
    */
  final case class PrimaryServiceRegistrationMessage(
      requestingActor: ActorRef[Response],
      inputModelUuid: UUID,
  ) extends ServiceRegistrationMessage

  /** This message can be sent from a proxy to a subordinate worker in order to
    * forward the original registration request. This message may only be used,
    * if no further information are needed.
    *
    * @param requestingActor
    *   Reference to the requesting actor
    */
  final case class WorkerRegistrationMessage(
      requestingActor: ActorRef[Response]
  ) extends ServiceRegistrationMessage

  /** Message that is sent by an [[edu.ie3.simona.api.ExtSimAdapter]] to
    * schedule a service.
    * @param tick
    *   For which the service should be scheduled.
    * @param unlockKey
    *   For unlocking.
    */
  final case class ScheduleServiceActivation(
      tick: Long,
      unlockKey: ScheduleKey,
  ) extends DataMessageFromExt

  /** A message to the agent outside of regular service data messages.
    */
  trait DirectAgentRequest extends Response {

    /** The tick for which the request is valid, which is the current tick.
      */
    val tick: Long
  }

  /** Requests number of free lots from evcs. The evcs agent will answer with an
    * [[FreeLotsResponse]].
    *
    * @param tick
    *   The latest tick that the data is requested for.
    * @param replyTo
    *   The actor to receive the response.
    */
  final case class EvFreeLotsRequest(
      override val tick: Long,
      replyTo: ActorRef[FreeLotsResponse],
  ) extends DirectAgentRequest

  /** Requests EV models of departing EVs with given UUIDs. The evcs agent will
    * answer with a [[DepartingEvsResponse]].
    *
    * @param tick
    *   The latest tick that the data is requested for.
    * @param departingEvs
    *   The UUIDs of EVs that are requested.
    * @param replyTo
    *   The actor to receive the response.
    */
  final case class DepartingEvsRequest(
      override val tick: Long,
      departingEvs: Seq[UUID],
      replyTo: ActorRef[DepartingEvsResponse],
  ) extends DirectAgentRequest

  /** Message sent to the service by an agent, e.g. in response to a
    * [[DirectAgentRequest]]. To receive these message, the service needs to
    * extend [[edu.ie3.simona.service.ExtDataSupport]].
    */
  sealed trait ServiceResponseMessage

  /** Response of an evcs agent to an [[EvFreeLotsRequest]].
    * @param evcs
    *   The uuid of the agent.
    * @param freeLots
    *   The number of free lots.
    */
  final case class FreeLotsResponse(
      evcs: UUID,
      freeLots: Int,
  ) extends ServiceResponseMessage

  /** Response of an evcs agent to a [[DepartingEvsRequest]].
    * @param evcs
    *   The uuid of the agent.
    * @param evModels
    *   The departing evs.
    */
  final case class DepartingEvsResponse(
      evcs: UUID,
      evModels: Seq[EvModelWrapper],
  ) extends ServiceResponseMessage

  /** A message that is sent to an energy management service by an energy
    * management agent.
    * @param message
    *   The actual flex message that is sent by the agent.
    * @param receiver
    *   The receiver of the message.
    */
  final case class EmFlexMessage(
      message: FlexRequest | FlexResponse,
      receiver: UUID | ActorRef[FlexResponse] | ActorRef[EmAgent.Message],
  ) extends ServiceResponseMessage

  /** Messages that are sent by services as responses to registration requests.
    */
  sealed trait RegistrationResponseMessage extends Response {
    val serviceRef: ActorRef[ServiceMessage]
  }

  /** Message confirming a successful registration with a secondary service.
    */
  final case class RegistrationSuccessfulMessage(
      override val serviceRef: ActorRef[ServiceMessage],
      firstDataTick: Long,
      additionalData: Option[AdditionalFactoryData] = None,
  ) extends RegistrationResponseMessage

  /** Message confirming a successful registration with the primary service.
    *
    * @param firstDataTick
    *   The first tick at which data will be sent.
    * @param primaryDataExtra
    *   Extra functionality specific to the primary data class.
    */
  final case class PrimaryRegistrationSuccessfulMessage(
      override val serviceRef: ActorRef[ServiceMessage],
      firstDataTick: Long,
      primaryDataExtra: PrimaryDataExtra[?],
  ) extends RegistrationResponseMessage

  /** Message announcing a failed registration.
    */
  final case class RegistrationFailedMessage(
      override val serviceRef: ActorRef[ServiceMessage]
  ) extends RegistrationResponseMessage

  /** Data provision messages sent by data services.
    */
  sealed trait DataMessage extends Response {

    /** The current tick.
      */
    val tick: Long

    /** The sending service actor ref.
      */
    val serviceRef: ActorRef[ServiceMessage]

    /** Next tick at which data could arrive. If None, no data is expected for
      * the rest of the simulation.
      */
    val nextDataTick: Option[Long]
  }

  /** Providing primary or secondary data to an agent.
    *
    * @param data
    *   The data.
    */
  final case class DataProvision(
      override val tick: Long,
      override val serviceRef: ActorRef[ServiceMessage],
      data: Data,
      override val nextDataTick: Option[Long],
  ) extends DataMessage

  /** Providing the information that no data will be provided by the sending
    * service for the current tick. The participant could thus potentially skip
    * calculations for the current tick and reschedule calculation for the next
    * data tick.
    */
  final case class NoDataProvision(
      override val tick: Long,
      override val serviceRef: ActorRef[ServiceMessage],
      override val nextDataTick: Option[Long],
  ) extends DataMessage

}
