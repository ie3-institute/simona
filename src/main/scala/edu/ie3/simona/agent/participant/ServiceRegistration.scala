/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import org.apache.pekko.actor.ActorRef
import edu.ie3.datamodel.models.input.system.{EvcsInput, SystemParticipantInput}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.PrimaryDataWithApparentPower
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.{
  ActorExtEvDataService,
  ActorPriceService,
  ActorWeatherService,
}
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.exceptions.agent.ServiceRegistrationException
import edu.ie3.simona.model.participant.{
  CalcRelevantData,
  ModelState,
  SystemParticipant,
}
import edu.ie3.simona.ontology.messages.services.EvMessage.RegisterForEvDataMessage
import edu.ie3.simona.ontology.messages.services.WeatherMessage.RegisterForWeatherMessage

trait ServiceRegistration[
    PD <: PrimaryDataWithApparentPower[PD],
    CD <: CalcRelevantData,
    MS <: ModelState,
    D <: ParticipantStateData[PD],
    I <: SystemParticipantInput,
    MC <: SimonaConfig.BaseRuntimeConfig,
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
    * @return
    *   an iterable of actor references to wait for responses
    */
  def registerForServices(
      inputModel: I,
      services: Iterable[SecondaryDataService[_ <: SecondaryData]],
  ): Iterable[ActorRef] =
    services.flatMap(service =>
      registerForSecondaryService(service, inputModel)
    )

  /** Register for the distinct secondary service
    *
    * @param serviceDefinition
    *   Definition of the service
    * @param inputModel
    *   Input model that is interested in the information
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
  ): Option[ActorRef] = serviceDefinition match {
    case SecondaryDataService.ActorPriceService(_) =>
      log.debug(
        s"Attempt to register for {}. This is currently not supported.",
        ActorPriceService,
      )
      None
    case ActorWeatherService(serviceRef) =>
      registerForWeather(serviceRef, inputModel)
      Some(serviceRef)
    case ActorExtEvDataService(serviceRef) =>
      registerForEvData(serviceRef, inputModel)
      Some(serviceRef)
  }

  /** Register for the weather service
    *
    * @param actorRef
    *   Actor reference of the weather service
    * @param inputModel
    *   Input model of the simulation mode
    * @return
    */
  private def registerForWeather(
      actorRef: ActorRef,
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
    actorRef ! RegisterForWeatherMessage(lat, lon)
  }

  /** Register for the EV movement service
    *
    * @param serviceRef
    *   Actor reference of the EV movements service
    * @param inputModel
    *   Input model of the simulation mode
    * @return
    */
  private def registerForEvData(
      serviceRef: ActorRef,
      inputModel: I,
  ): Unit = {
    inputModel match {
      case evcsInput: EvcsInput =>
        serviceRef ! RegisterForEvDataMessage(evcsInput.getUuid)
      case _ =>
        throw new ServiceRegistrationException(
          s"Cannot register for EV movements information at node ${inputModel.getNode.getId} " +
            s"(${inputModel.getNode.getUuid}) of type ${inputModel.getClass.getName}, because only Evcs can register for this."
        )
    }
  }

}
