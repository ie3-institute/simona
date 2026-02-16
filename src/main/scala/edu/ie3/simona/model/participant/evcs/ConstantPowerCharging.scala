/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import squants.Power

import java.util.UUID

/** Determine scheduling for charging the EVs currently parked at the charging
  * station by charging with constant power from current time until departure.
  * If less than the maximum power is required to reach the target SoC, the
  * power is reduced accordingly.
  */
object ConstantPowerCharging extends EvcsChargingStrategy {

  override def determineChargingPowers(
      evs: Iterable[EvModelWrapper],
      currentTick: Long,
      chargingProps: EvcsChargingProperties,
  ): Map[UUID, Power] = evs
    .filter(ev =>
      ev.storedEnergy < ev.eStorage * chargingProps.departureTargetSoc
    )
    .map { ev =>
      val maxChargingPower = chargingProps.getMaxAvailableChargingPower(ev)
      val timeToDeparture = ev.timeToDeparture(currentTick)

      val requiredEnergyUntilTarget =
        ev.eStorage * chargingProps.departureTargetSoc - ev.storedEnergy

      val chargingPower =
        maxChargingPower.min(requiredEnergyUntilTarget / timeToDeparture)

      ev.uuid -> chargingPower
    }
    .toMap

}
