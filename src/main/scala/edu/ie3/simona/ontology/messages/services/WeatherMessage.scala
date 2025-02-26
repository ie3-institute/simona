/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant2.ParticipantAgent
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ServiceRegistrationMessage
import edu.ie3.util.scala.quantities.Irradiance
import org.apache.pekko.actor.typed.ActorRef
import squants.{Temperature, Velocity}

sealed trait WeatherMessage

/** Declares all messages sent and received by the weather service and weather
  * data provided through these messages
  *
  * @version 0.1
  * @since 2019-07-28
  */
object WeatherMessage {

  private[services] trait WeatherInternal extends WeatherMessage

  /** Indicate the [[edu.ie3.simona.service.weather.WeatherService]] that the
    * requesting agent wants to receive weather for the provided coordinates
    *
    * @param requestingActor
    *   The actor requesting registration for weather data
    * @param latitude
    *   Latitude of the requested location
    * @param longitude
    *   Longitude of the requested location
    */
  final case class RegisterForWeatherMessage(
      requestingActor: ActorRef[ParticipantAgent.Request],
      latitude: Double,
      longitude: Double,
  ) extends WeatherMessage
      with ServiceRegistrationMessage

  /** Container class for the entirety of weather information at a certain point
    * in time and at a certain coordinate
    *
    * @param diffIrr
    *   Diffuse irradiance on the horizontal pane
    * @param dirIrr
    *   Direct irradiance on the horizontal pane
    * @param temp
    *   Temperature
    * @param windVel
    *   Wind velocity
    */
  final case class WeatherData(
      diffIrr: Irradiance,
      dirIrr: Irradiance,
      temp: Temperature,
      windVel: Velocity,
  ) extends SecondaryData

}
