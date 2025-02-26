/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.evcs

import edu.ie3.simona.exceptions.CriticalFailureException
import squants.Power

import java.util.UUID

trait EvcsChargingStrategy {

  /** Determine scheduling for charging the EVs currently parked at the charging
    * station until their departure.
    *
    * @param evs
    *   currently parked evs at the charging station
    * @param currentTick
    *   current tick
    * @param chargingProps
    *   interface that provides information on charging station
    * @return
    *   scheduling for charging the EVs
    */
  def determineChargingPowers(
      evs: Iterable[EvModelWrapper],
      currentTick: Long,
      chargingProps: EvcsChargingProperties,
  ): Map[UUID, Power]

}

object EvcsChargingStrategy {

  def apply(token: String): EvcsChargingStrategy =
    "[-_]".r.replaceAllIn(token.trim.toLowerCase, "") match {
      case "maxpower"      => MaximumPowerCharging
      case "constantpower" => ConstantPowerCharging
      case unknown =>
        throw new CriticalFailureException(
          s"The token '$unknown' cannot be parsed to charging strategy."
        )
    }

}
