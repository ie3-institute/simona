/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.akka.SimonaActorRef
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  ProvisionMessage,
  ServiceRegistrationMessage
}
import edu.ie3.util.quantities.interfaces.Irradiance
import tech.units.indriya.ComparableQuantity

import javax.measure.quantity.{Speed, Temperature}

sealed trait WeatherMessage

/** Declares all messages sent and received by the weather service and weather
  * data provided through these messages
  *
  * @version 0.1
  * @since 2019-07-28
  */
object WeatherMessage {

  /** Indicate the [[edu.ie3.simona.service.weather.WeatherService]] that the
    * requesting agent wants to receive weather for the provided coordinates
    *
    * @param latitude
    *   Latitude of the requested location
    * @param longitude
    *   Longitude of the requested location
    */
  final case class RegisterForWeatherMessage(
      latitude: Double,
      longitude: Double
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
      override val data: WeatherData,
      override val nextDataTick: Option[Long],
      override val serviceRef: SimonaActorRef
  ) extends WeatherMessage
      with ProvisionMessage[WeatherData]

  /** Hold entire weather result together
    */
  final case class WeatherData(
      diffRad: ComparableQuantity[Irradiance],
      dirRad: ComparableQuantity[Irradiance],
      temp: ComparableQuantity[Temperature],
      windVel: ComparableQuantity[Speed]
  ) extends SecondaryData

}
