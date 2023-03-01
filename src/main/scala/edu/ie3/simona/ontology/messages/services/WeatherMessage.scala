/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  ProvisionMessage,
  ServiceRegistrationMessage
}
import edu.ie3.util.quantities.interfaces.Irradiance
import tech.units.indriya.ComparableQuantity

import javax.measure.Quantity
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
      override val nextDataTick: Option[Long]
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
      diffIrr: ComparableQuantity[Irradiance],
      dirIrr: ComparableQuantity[Irradiance],
      temp: ComparableQuantity[Temperature],
      windVel: ComparableQuantity[Speed]
  ) extends SecondaryData

  /** Container class for the weather data option. It is similar to the normal
    * weather data but instead of concrete quantities, this container holds
    * options for quantities with their weight.
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
  final case class WeatherDataOption(
      diffIrr: Option[QuantityWithWeight[Irradiance]],
      dirIrr: Option[QuantityWithWeight[Irradiance]],
      temp: Option[QuantityWithWeight[Temperature]],
      windVel: Option[QuantityWithWeight[Speed]]
  )

  /** Container class for a weather quantity with a weight. It is primarily used
    * for interpolation.
    * @param quantity
    *   value
    * @param weight
    *   of the value
    * @tparam V
    *   unit of the quantity
    */
  final case class QuantityWithWeight[V <: Quantity[V]](
      quantity: ComparableQuantity[V],
      weight: Long
  )
}
