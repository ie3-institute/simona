/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.evcs

import squants.Power

import java.util.UUID

/** Determine scheduling for charging the EVs currently parked at the charging
  * station by charging with maximum power from current time until it reaches
  * either 100% SoC or its departure time.
  */
object MaximumPowerCharging extends EvcsChargingStrategy {

  def determineChargingPowers(
      evs: Iterable[EvModelWrapper],
      currentTick: Long,
      chargingProps: EvcsChargingProperties,
  ): Map[UUID, Power] = evs
    .filter(ev => ev.storedEnergy < ev.eStorage)
    .map { ev =>
      ev.uuid -> chargingProps.getMaxAvailableChargingPower(ev)
    }
    .toMap
}
