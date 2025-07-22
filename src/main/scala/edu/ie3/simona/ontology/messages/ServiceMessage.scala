/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages

import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.ParticipantAgent.ParticipantRequest
import edu.ie3.simona.api.ontology.DataMessageFromExt
import edu.ie3.simona.model.participant.evcs.EvModelWrapper
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexRequest,
  FlexResponse,
}
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.service.ServiceStateData.InitializeServiceStateData
import edu.ie3.util.TimeUtil
import org.apache.pekko.actor.typed.ActorRef

import java.time.ZonedDateTime
import java.util.UUID

/** Collections of all messages, that are send to and from the different
  * services
  */
sealed trait ServiceMessage

object ServiceMessage {

  /** Service initialization data can sometimes only be constructed once the
    * service actor is created (e.g.
    * [[edu.ie3.simona.service.ev.ExtEvDataService]]). Thus, we need an extra
    * initialization message.
    */
  final case class Create(
      initializeStateData: InitializeServiceStateData,
      unlockKey: ScheduleKey,
  ) extends ServiceMessage

  /** Message used to register for a service.
    */
  trait ServiceRegistrationMessage extends ServiceMessage

  /** Indicate a [[edu.ie3.simona.service.SimonaService]] that the requesting
    * agent wants to be registered for the specific service.
    *
    * @param requestingActor
    *   The actor requesting registration for the data service.
    * @param data
    *   The data, that is used during the registration.
    */
  final case class SecondaryServiceRegistrationMessage[D](
      requestingActor: ActorRef[ParticipantAgent.Request],
      data: D,
  ) extends ServiceRegistrationMessage

  final case class EmServiceRegistration(
      requestingActor: ActorRef[EmAgent.Message],
      inputUuid: UUID,
      parentEm: Option[ActorRef[FlexResponse]],
      parentUuid: Option[UUID],
  ) extends ServiceRegistrationMessage

  /** Message to register with a primary data service.
    *
    * @param requestingActor
    *   The actor requesting registration for primary data
    * @param inputModelUuid
    *   Identifier of the input model
    */
  final case class PrimaryServiceRegistrationMessage(
      requestingActor: ActorRef[ParticipantAgent.Request],
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
      requestingActor: ActorRef[ParticipantAgent.Request]
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
  ) extends ParticipantRequest

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
  ) extends ParticipantRequest

  /** Message used in response to a service request. To receive these message,
    * the service needs to extend [[edu.ie3.simona.service.ExtDataSupport]].
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

  final case class EmFlexMessage(
      message: FlexRequest | FlexResponse,
      receiver: UUID | ActorRef[FlexResponse] | ActorRef[EmAgent.Message],
  ) extends ServiceResponseMessage

  final case class ResultResponseMessage(results: Iterable[ResultEntity])
      extends ServiceMessage
      with ServiceResponseMessage {
    def tick(using startTime: ZonedDateTime): Long = {
      val time = results match {
        case res :: el => res.getTime
      }

      TimeUtil.withDefaults.zonedDateTimeDifferenceInSeconds(
        startTime,
        time,
      )
    }
  }
}
