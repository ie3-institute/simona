/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import edu.ie3.simona.agent.participant2.ParticipantAgent

import java.util.UUID
import edu.ie3.simona.api.data.ontology.DataMessageFromExt
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.services.EvMessage.EvInternal
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.service.ServiceStateData.InitializeServiceStateData
import org.apache.pekko.actor.typed.ActorRef

/** Collections of all messages, that are send to and from the different
  * services
  */
sealed trait ServiceMessage extends EvInternal

object ServiceMessage {

  final case class WrappedActivation(activation: Activation)
      extends ServiceMessage

  final case class WrappedExternalMessage(
      extMsg: DataMessageFromExt
  ) extends ServiceMessage

  /** Service initialization data can sometimes only be constructed once the
    * service actor is created (e.g.
    * [[edu.ie3.simona.service.ev.ExtEvDataService]]). Thus, we need an extra
    * initialization message.
    */
  final case class Create[+I <: InitializeServiceStateData](
      initializeStateData: I,
      unlockKey: ScheduleKey,
  ) extends ServiceMessage

  final case class ScheduleServiceActivation(
      tick: Long,
      unlockKey: ScheduleKey,
  ) extends ServiceMessage

  /** Message used to register for a service
    */
  trait ServiceRegistrationMessage extends ServiceMessage

  /** Message to register with a primary data service.
    *
    * @param agentToBeRegistered
    *   Reference to the agent, that should be registered
    * @param inputModelUuid
    *   Identifier of the input model
    */
  final case class PrimaryServiceRegistrationMessage(
      agentToBeRegistered: ActorRef[ParticipantAgent.Request],
      inputModelUuid: UUID,
  ) extends ServiceRegistrationMessage

  /** This message can be sent from a proxy to a subordinate worker in order to
    * forward the original registration request. This message may only be used,
    * if no further information are needed.
    *
    * @param agentToBeRegistered
    *   Reference to the agent, that should be registered
    */
  final case class WorkerRegistrationMessage(
      agentToBeRegistered: ActorRef[ParticipantAgent.Request]
  ) extends ServiceRegistrationMessage
}
