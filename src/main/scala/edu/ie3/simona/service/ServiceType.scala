/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service

sealed trait ServiceType

object ServiceType {

  case object WeatherService extends ServiceType

  case object PriceService extends ServiceType

  case object EvMovementService extends ServiceType

  case object LoadProfileService extends ServiceType
}
