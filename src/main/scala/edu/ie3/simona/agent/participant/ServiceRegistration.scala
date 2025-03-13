/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import org.apache.pekko.actor.ActorRef
import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.PrimaryDataWithComplexPower
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.{
  ActorPriceService,
  ActorWeatherService,
}
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
import edu.ie3.simona.config.RuntimeConfig.BaseRuntimeConfig
import edu.ie3.simona.exceptions.agent.ServiceRegistrationException
import edu.ie3.simona.model.participant.{
  CalcRelevantData,
  ModelState,
  SystemParticipant,
}
import edu.ie3.simona.ontology.messages.services.WeatherMessage.RegisterForWeatherMessage

trait ServiceRegistration[
    PD <: PrimaryDataWithComplexPower[PD],
    CD <: CalcRelevantData,
    MS <: ModelState,
    D <: ParticipantStateData[PD],
    I <: SystemParticipantInput,
    MC <: BaseRuntimeConfig,
    M <: SystemParticipant[CD, PD, MS],
] {
  this: ParticipantAgent[PD, CD, MS, D, I, MC, M] =>

  /** Registers the agent for the needed services and collects all actor
    * references, with which the actor has been registered
    *
    * @param inputModel
    *   Input model definition
    * @param services
    *   Definition of where to get what
    * @param participantRef
    *   Actor reference of the participant
    * @return
    *   an iterable of actor references to wait for responses
    */
  def registerForServices(
      inputModel: I,
      services: Iterable[SecondaryDataService[_ <: SecondaryData]],
      participantRef: ActorRef,
  ): Iterable[ActorRef] =
    services.flatMap(service =>
      registerForSecondaryService(service, inputModel, participantRef)
    )

  /** Register for the distinct secondary service
    *
    * @param serviceDefinition
    *   Definition of the service
    * @param inputModel
    *   Input model that is interested in the information
    * @param participantRef
    *   Actor reference of the participant
    * @tparam S
    *   Type of the secondary data, that is awaited
    * @return
    *   An [[Option]] to the service's [[ActorRef]], if registration is
    *   supported at the moment
    */
  private def registerForSecondaryService[
      S <: SecondaryData
  ](
      serviceDefinition: SecondaryDataService[S],
      inputModel: I,
      participantRef: ActorRef,
  ): Option[ActorRef] = serviceDefinition match {
    case SecondaryDataService.ActorPriceService(_) =>
      log.debug(
        s"Attempt to register for {}. This is currently not supported.",
        ActorPriceService,
      )
      None
    case ActorWeatherService(serviceRef) =>
      registerForWeather(serviceRef, participantRef, inputModel)
      Some(serviceRef)
  }

  /** Register for the weather service
    *
    * @param serviceRef
    *   Actor reference of the weather service
    * @param participantRef
    *   Actor reference of the participant
    * @param inputModel
    *   Input model of the simulation mode
    * @return
    */
  private def registerForWeather(
      serviceRef: ActorRef,
      participantRef: ActorRef,
      inputModel: I,
  ): Unit = {
    /* If we are asked to register for weather, determine the proper geo position */
    val geoPosition = inputModel.getNode.getGeoPosition
    val (lat, lon) =
      (Option(geoPosition.getY), Option(geoPosition.getX)) match {
        case (Some(lat), Some(lon)) => (lat, lon)
        case _ =>
          throw new ServiceRegistrationException(
            s"Cannot register for weather information at node ${inputModel.getNode.getId} " +
              s"(${inputModel.getNode.getUuid}), because the geo position " +
              s"(${inputModel.getNode.getGeoPosition.getY}, ${inputModel.getNode.getGeoPosition.getX}) " +
              s"is invalid."
          )
      }
    serviceRef ! RegisterForWeatherMessage(participantRef.toTyped, lat, lon)
  }

}
