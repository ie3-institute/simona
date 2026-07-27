/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.datamodel.models.input.system.{LoadInput, SystemParticipantInput}
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.ParticipantModel.AdditionalFactoryData
import edu.ie3.simona.ontology.messages.ServiceMessage
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
  SecondaryServiceRegistrationMessage,
}
import edu.ie3.simona.service.weather.WeatherService.WeatherRegistrationData
import edu.ie3.simona.service.{
  DataTimeType,
  ServiceRegistrationData,
  ServiceType,
}
import edu.ie3.simona.util.Coordinate
import edu.ie3.simona.util.InputUtils.identifier
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

/** Trait offering secondary service registration functionality to agents.
  *
  * @tparam Msg
  *   The type of message that the resulting behavior can receive. Needs to
  *   allow service responses for obvious reasons.
  * @tparam CR
  *   The consumer result type, if applicable. Can be [[Unit]] otherwise.
  */
trait SecondaryServiceRegistration[Msg >: ServiceMessage.Response, CR] {

  /** Consumer for additional data that might come with a
    * [[RegistrationSuccessfulMessage]].
    */
  trait AdditionalDataConsumer {
    def update(data: AdditionalFactoryData): CR
    def unchanged: CR
  }

  private type CompletionBehavior =
    (CR, Map[ActorRef[ServiceMessage], Long]) => Behavior[Msg]

  /** Starts registration with required services, if any are present. Otherwise,
    * we skip to the completion behavior right away.
    *
    * @param modelAsset
    *   The agent model.
    * @param additionalDataConsumer
    *   A consumer for additional data received by the service.
    * @param registrationCompleteBehavior
    *   The behavior to transition to after the registration process completed.
    * @param registrationData
    *   The registration data to use for service registration, including types
    *   of services and data time type.
    * @param services
    *   A map of available services by service type.
    * @return
    *   A following behavior.
    */
  def startRegistration(
      modelAsset: AssetInput,
      additionalDataConsumer: AdditionalDataConsumer,
      registrationCompleteBehavior: CompletionBehavior,
      registrationData: ServiceRegistrationData,
      services: Map[ServiceType, ActorRef[ServiceMessage]],
  ): Behavior[Msg] = {
    Behaviors.setup { ctx =>
      if registrationData.serviceTypes.isEmpty then {
        // not requiring any secondary services, thus we're ready to go
        registrationCompleteBehavior(
          additionalDataConsumer.unchanged,
          Map.empty,
        )
      } else {
        // requiring at least one secondary service, thus send out registrations and wait for replies
        val requiredServices = registrationData.serviceTypes
          .map(serviceType =>
            serviceType -> services
              .getOrElse(
                serviceType,
                throw new CriticalFailureException(
                  s"${modelAsset.identifier}: Service of type $serviceType is not available."
                ),
              )
          )
          .toMap

        requiredServices.foreach { case (serviceType, serviceRef) =>
          registerForService(
            modelAsset,
            ctx.self,
            serviceType,
            registrationData.dataTimeType,
            serviceRef,
          )
        }

        waitingForServices(
          modelAsset,
          additionalDataConsumer,
          registrationCompleteBehavior,
          requiredServices.values.toSet,
        )
      }
    }
  }

  /** Waiting for replies from secondary services. If all replies have been
    * received, we complete the initialization.
    */
  private def waitingForServices(
      modelAsset: AssetInput,
      additionalDataConsumer: AdditionalDataConsumer,
      registrationCompleteBehavior: CompletionBehavior,
      expectedRegistrations: Set[ActorRef[ServiceMessage]],
      expectedFirstData: Map[ActorRef[ServiceMessage], Long] = Map.empty,
  ): Behavior[Msg] =
    Behaviors.withStash(100) { buffer =>
      Behaviors.receiveMessagePartial {
        case RegistrationSuccessfulMessage(
              serviceRef,
              nextDataTick,
              additionalData,
            ) =>
          // received registration success message from secondary service
          if !expectedRegistrations.contains(serviceRef) then
            throw new CriticalFailureException(
              s"${modelAsset.identifier}: Registration response from $serviceRef was not expected!"
            )

          val newExpectedRegistrations = expectedRegistrations.excl(serviceRef)
          val newExpectedFirstData =
            expectedFirstData.updated(serviceRef, nextDataTick)

          val consumerResult = additionalData match {
            case Some(data: AdditionalFactoryData) =>
              additionalDataConsumer.update(data)
            case None => additionalDataConsumer.unchanged
          }

          if newExpectedRegistrations.isEmpty then {
            // all secondary services set up, ready to go
            buffer.unstashAll(
              registrationCompleteBehavior(
                consumerResult,
                newExpectedFirstData,
              )
            )
          } else
            // there's at least one more service to go, let's wait for confirmation
            waitingForServices(
              modelAsset,
              additionalDataConsumer,
              registrationCompleteBehavior,
              newExpectedRegistrations,
              newExpectedFirstData,
            )

        case RegistrationFailedMessage(serviceRef) =>
          throw new CriticalFailureException(
            s"${modelAsset.identifier}: Registration for service $serviceRef failed!"
          )

        case msg =>
          // stash away other messages until service registration has completed
          buffer.stash(msg)
          Behaviors.same
      }
    }

  private def registerForService(
      assetInput: AssetInput,
      registrantRef: ActorRef[Msg],
      serviceType: ServiceType,
      dataTimeType: DataTimeType,
      serviceRef: ActorRef[ServiceMessage],
  ): Unit =
    serviceType match {
      case ServiceType.WeatherService =>
        val participantInput = assetInput match {
          case spInput: SystemParticipantInput =>
            spInput
          case _ =>
            throw new CriticalFailureException(
              s"${assetInput.identifier}: Only SystemParticipantInputs can register for weather data, since we need a geolocation for weather registration."
            )
        }

        val geoPosition = participantInput.getNode.getGeoPosition

        Option(geoPosition.getY).zip(Option(geoPosition.getX)) match {
          case Some((lat, lon)) =>
            serviceRef ! SecondaryServiceRegistrationMessage(
              registrantRef,
              dataTimeType,
              WeatherRegistrationData(
                Coordinate(lat, lon)
              ),
            )
          case _ =>
            throw new CriticalFailureException(
              s"${participantInput.identifier} cannot register for weather information at " +
                s"node ${participantInput.getNode.getId} (${participantInput.getNode.getUuid}), " +
                s"because the geo position (${geoPosition.getY}, ${geoPosition.getX}) is invalid."
            )
        }

      case ServiceType.PriceService =>
        serviceRef ! SecondaryServiceRegistrationMessage(
          registrantRef,
          dataTimeType,
          (),
        )

      case ServiceType.EvMovementService =>
        serviceRef ! SecondaryServiceRegistrationMessage(
          registrantRef,
          // only data for current tick possible
          DataTimeType.Current,
          assetInput.getUuid,
        )

      case ServiceType.LoadProfileService =>
        assetInput match {
          case load: LoadInput =>
            serviceRef ! SecondaryServiceRegistrationMessage(
              registrantRef,
              dataTimeType,
              load.getLoadProfile,
            )

          case _ =>
            throw new CriticalFailureException(
              s"${assetInput.identifier} cannot register for load profile service!"
            )
        }
    }

}
