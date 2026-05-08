/*
 * © 2025-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service

import squants.Time
import squants.time.Hours

/** Specifies the temporal dimension of [[Data]], e.g. if data for the current
  * tick or beyond that is requested.
  */
trait DataTimeType

object DataTimeType {

  /** Data at the current point in simulation time.
    */
  case object Current extends DataTimeType

  /** Data at the current point in simulation time and a forecast for a specific
    * length of time into the future.
    *
    * @param forecastLength
    *   The length of the forecast, i.e. the amount of time into future to
    *   forecast. Should be a multiple of [[forecastResolution]].
    * @param forecastResolution
    *   The resolution of forecast time steps.
    */
  final case class CurrentAndForecast(
      forecastLength: Time,
      forecastResolution: Time = Hours(1),
  ) extends DataTimeType

}
