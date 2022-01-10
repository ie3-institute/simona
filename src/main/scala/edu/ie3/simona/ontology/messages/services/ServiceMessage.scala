/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import java.util.UUID
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.akka.SimonaActorRef

/** Collections of all messages, that are send to and from the different
  * services
  */
sealed trait ServiceMessage

case object ServiceMessage {

  /** Message used to register for a service
    */
  trait ServiceRegistrationMessage extends ServiceMessage

  /** Message to register with a primary data service.
    *
    * @param inputModelUuid
    *   Identifier of the input model
    * @param requestingActor
    *   Reference to the requesting actor
    */
  final case class PrimaryServiceRegistrationMessage(
      inputModelUuid: UUID,
      requestingActor: SimonaActorRef
  ) extends ServiceRegistrationMessage

  /** This message can be sent from a proxy to a subordinate worker in order to
    * forward the original registration request. This message may only be used,
    * if no further information are needed.
    *
    * @param requestingActor
    *   Reference to the requesting actor
    */
  final case class WorkerRegistrationMessage(requestingActor: SimonaActorRef)
      extends ServiceRegistrationMessage

  sealed trait RegistrationResponseMessage extends ServiceMessage {
    val serviceRef: SimonaActorRef
  }

  case object RegistrationResponseMessage {

    /** Message, that is used to confirm a successful registration
      */
    final case class RegistrationSuccessfulMessage(
        nextDataTick: Option[Long],
        override val serviceRef: SimonaActorRef
    ) extends RegistrationResponseMessage

    /** Message, that is used to announce a failed registration
      */
    final case class RegistrationFailedMessage(
        override val serviceRef: SimonaActorRef
    ) extends RegistrationResponseMessage
  }

  /** Actual provision of data
    *
    * @tparam D
    *   type of data that is delivered
    */
  trait ProvisionMessage[D <: Data] extends ServiceMessage {
    val tick: Long
    val data: D
    val nextDataTick: Option[Long]
    val serviceRef: SimonaActorRef
  }
}
