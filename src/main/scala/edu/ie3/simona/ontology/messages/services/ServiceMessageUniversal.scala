/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import edu.ie3.simona.api.data.ontology.{
  DataMessageFromExt,
  ScheduleDataServiceMessage,
}
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.services.EvMessage.EvInternal
import edu.ie3.simona.ontology.messages.services.PrimaryDataMessage.PrimaryInternal
import edu.ie3.simona.ontology.messages.services.WeatherMessage.WeatherInternal
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.service.ServiceStateData.InitializeServiceStateData
import org.apache.pekko.actor.typed.ActorRef

sealed trait ServiceMessageUniversal
    extends PrimaryInternal
    with EvInternal
    with WeatherInternal

object ServiceMessageUniversal {

  final case class WrappedActivation(activation: Activation)
      extends ServiceMessageUniversal

  final case class WrappedExternalMessage(
      extMsg: DataMessageFromExt
  ) extends ServiceMessageUniversal

  /** Service initialization data can sometimes only be constructed once the
    * service actor is created (e.g.
    * [[edu.ie3.simona.service.ev.ExtEvDataService]]). Thus, we need an extra
    * initialization message.
    */
  final case class Create[+I <: InitializeServiceStateData](
      initializeStateData: I,
      unlockKey: ScheduleKey,
  ) extends ServiceMessageUniversal

  /** Message used to register for a service
    */
  trait ServiceRegistrationMessage extends ServiceMessageUniversal

  sealed trait RegistrationResponseMessage extends ServiceMessageUniversal {
    val serviceRef: ActorRef[_ <: ServiceMessage]
  }

  object RegistrationResponseMessage {

    /** Message, that is used to confirm a successful registration
      */
    final case class RegistrationSuccessfulMessage(
        override val serviceRef: ActorRef[_ <: ServiceMessage],
        nextDataTick: Option[Long],
    ) extends RegistrationResponseMessage

    /** Message, that is used to announce a failed registration
      */
    final case class RegistrationFailedMessage(
        override val serviceRef: ActorRef[_ <: ServiceMessage]
    ) extends RegistrationResponseMessage

    final case class ScheduleServiceActivation(
        tick: Long,
        unlockKey: ScheduleKey,
    ) extends ServiceMessageUniversal
  }
}
