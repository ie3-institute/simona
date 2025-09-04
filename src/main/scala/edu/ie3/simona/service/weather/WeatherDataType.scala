/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.weather

import squants.Time
import squants.time.Hours

trait WeatherDataType

object WeatherDataType {

  /** Weather data at the current point in simulation time
    */
  case object Current extends WeatherDataType

  /** Weather data at the current point in simulation time and a weather
    * forecast for a specific length of time into the future.
    *
    * @param forecastLength
    *   The length of the forecast, i.e. the amount of time into future to
    *   forecast. Should be a multiple of [[forecastInterval]].
    * @param forecastInterval
    *   The interval of forecasts.
    */
  final case class CurrentAndForecast(
      forecastLength: Time,
      forecastInterval: Time = Hours(1),
  ) extends WeatherDataType

}
