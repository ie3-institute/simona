/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import squants.{Power, Seconds}

import java.util.UUID

/** Determine scheduling for charging the EVs currently parked at the charging
  * station by charging with constant power from current time until departure.
  * If less than the maximum power is required to reach 100% SoC, the power is
  * reduced accordingly.
  */
object ConstantPowerCharging extends EvcsChargingStrategy {

  override def determineChargingPowers(
      evs: Iterable[EvModelWrapper],
      currentTick: Long,
      chargingProps: EvcsChargingProperties,
  ): Map[UUID, Power] = evs
    .filter(ev => ev.storedEnergy < ev.eStorage)
    .map { ev =>
      val maxChargingPower = chargingProps.getMaxAvailableChargingPower(ev)
      val remainingParkingTime = Seconds(ev.departureTick - currentTick)

      val requiredEnergyUntilFull = ev.eStorage - ev.storedEnergy

      val chargingPower =
        maxChargingPower.min(requiredEnergyUntilFull / remainingParkingTime)

      ev.uuid -> chargingPower
    }
    .toMap

}
