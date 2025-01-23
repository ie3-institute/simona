/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import edu.ie3.simona.agent.em.EmAgent

import org.apache.pekko.actor.{ActorRef => ClassicRef}
import org.apache.pekko.actor.typed.ActorRef

import java.util.UUID
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.FlexRequest
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey

/** Collections of all messages, that are send to and from the different
  * services
  */
sealed trait ServiceMessage

object ServiceMessage {

  final case class RequestExtPrimaryDataAssets() extends ServiceMessage {}

  /** Message used to register for a service
    */
  trait ServiceRegistrationMessage extends ServiceMessage

  /** Message to register with a primary data service.
    *
    * @param inputModelUuid
    *   Identifier of the input model
    */
  final case class PrimaryServiceRegistrationMessage(inputModelUuid: UUID)
      extends ServiceRegistrationMessage

  /** This message can be sent from a proxy to a subordinate worker in order to
    * forward the original registration request. This message may only be used,
    * if no further information are needed.
    *
    * @param requestingActor
    *   Reference to the requesting actor
    */
  final case class WorkerRegistrationMessage(requestingActor: ClassicRef)
      extends ServiceRegistrationMessage

  final case class ExtPrimaryDataServiceRegistrationMessage(
      modelUuid: UUID,
      requestingActor: ClassicRef,
  ) extends ServiceRegistrationMessage

  final case class ExtEmDataServiceRegistrationMessage(
      modelUuid: UUID,
      requestingActor: ActorRef[EmAgent.Request],
      flexAdapter: ActorRef[FlexRequest],
  ) extends ServiceRegistrationMessage

  sealed trait RegistrationResponseMessage extends ServiceMessage {
    val serviceRef: ClassicRef
  }

  object RegistrationResponseMessage {

    /** Message, that is used to confirm a successful registration
      */
    final case class RegistrationSuccessfulMessage(
        override val serviceRef: ClassicRef,
        nextDataTick: Option[Long],
    ) extends RegistrationResponseMessage

    final case class WrappedRegistrationSuccessfulMessage(
        registrationSuccessfulMessage: RegistrationSuccessfulMessage
    ) extends EmAgent.Request

    /** Message, that is used to announce a failed registration
      */
    final case class RegistrationFailedMessage(
        override val serviceRef: ClassicRef
    ) extends RegistrationResponseMessage

    final case class ScheduleServiceActivation(
        tick: Long,
        unlockKey: ScheduleKey,
    )
  }

  /** Actual provision of data
    *
    * @tparam D
    *   type of data that is delivered
    */
  trait ProvisionMessage[D <: Data] extends ServiceMessage {
    val tick: Long
    val serviceRef: ClassicRef
    val data: D

    /** Next tick at which data could arrive. If None, no data is expected for
      * the rest of the simulation
      */
    val nextDataTick: Option[Long]
  }
}
