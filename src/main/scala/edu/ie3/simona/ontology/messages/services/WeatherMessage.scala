/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  ProvisionMessage,
  ServiceInternal,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessageUniversal.ServiceRegistrationMessage
import edu.ie3.util.scala.quantities.Irradiance
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.{ActorRef => ClassicRef}
import squants.{Temperature, Velocity}

sealed trait WeatherMessage extends ServiceInternal

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
    * @param actorRef
    *   actor ref for the agent to be registered
    * @param latitude
    *   Latitude of the requested location
    * @param longitude
    *   Longitude of the requested location
    */
  final case class RegisterForWeatherMessage(
      actorRef: ClassicRef,
      latitude: Double,
      longitude: Double,
  ) extends WeatherMessage
      with ServiceRegistrationMessage

  /** Provide weather for the requested tick
    *
    * @param tick
    *   The tick, for which the data is requested for
    * @param data
    *   Actual information
    * @param nextDataTick
    *   Foreseen next tick, where data is available
    */
  final case class ProvideWeatherMessage(
      override val tick: Long,
      override val serviceRef: ActorRef[WeatherMessage],
      override val data: WeatherData,
      override val nextDataTick: Option[Long],
  ) extends WeatherMessage
      with ProvisionMessage[WeatherData]

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
