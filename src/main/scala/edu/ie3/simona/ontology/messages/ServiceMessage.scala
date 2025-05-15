/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages

import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.api.data.ontology.DataMessageFromExt
import edu.ie3.simona.model.participant.evcs.EvModelWrapper
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.service.ServiceStateData.InitializeServiceStateData
import org.apache.pekko.actor.typed.ActorRef

import java.util.UUID

/** Collections of all messages, that are send to and from the different
  * services
  */
sealed trait ServiceMessage

object ServiceMessage {

  type ServiceMessages = ServiceMessage | Activation

  // actor ref
  type ServiceRef = ActorRef[ServiceMessage]

  /** Service initialization data can sometimes only be constructed once the
    * service actor is created (e.g.
    * [[edu.ie3.simona.service.ev.ExtEvDataService]]). Thus, we need an extra
    * initialization message.
    */
  final case class Create(
      initializeStateData: InitializeServiceStateData,
      unlockKey: ScheduleKey,
  ) extends ServiceMessage

  final case class ScheduleServiceActivation(
      tick: Long,
      unlockKey: ScheduleKey,
  ) extends ServiceMessage
      with DataMessageFromExt // TODO: Check if this can be removed

  /** Message used in response to a service request.
    */
  trait ServiceResponseMessage extends ServiceMessage

  final case class FreeLotsResponse(
      evcs: UUID,
      freeLots: Int,
  ) extends ServiceResponseMessage

  final case class DepartingEvsResponse(
      evcs: UUID,
      evModels: Seq[EvModelWrapper],
  ) extends ServiceResponseMessage

  /** Message used to register for a service
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
  final case class RegisterForService[D](
      requestingActor: ActorRef[ParticipantAgent.Request],
      data: D,
  ) extends ServiceRegistrationMessage

  /** Message to register with a primary data service.
    *
    * @param requestingActor
    *   The actor requesting registration for primary data
    * @param inputModelUuid
    *   Identifier of the input model
    */
  // TODO: Use RegisterForService instead?
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

}
