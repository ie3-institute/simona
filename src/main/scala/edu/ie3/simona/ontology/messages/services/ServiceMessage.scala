/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import akka.actor.ActorRef

import java.util.UUID
import edu.ie3.simona.agent.participant.data.Data

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
  final case class WorkerRegistrationMessage(
      requestingActor: ActorRef,
      requestingUUID: UUID
  ) extends ServiceRegistrationMessage

  /** This message can be sent from the ExtOpfDataService to the
    * PrimaryServiceProxy in order to register als PrimaryServiceWorker for all
    * fixed feed-in generators
    *
    * @param generators
    */
  final case class ExtOpfRegistrationMessage(generators: List[UUID])
      extends ServiceRegistrationMessage

  sealed trait RegistrationResponseMessage extends ServiceMessage

  case object RegistrationResponseMessage {

    /** Message, that is used to confirm a successful registration
      */
    final case class RegistrationSuccessfulMessage(nextDataTick: Option[Long])
        extends RegistrationResponseMessage

    /** Message, that is used to announce a failed registration
      */
    case object RegistrationFailedMessage extends RegistrationResponseMessage
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
  }
}
