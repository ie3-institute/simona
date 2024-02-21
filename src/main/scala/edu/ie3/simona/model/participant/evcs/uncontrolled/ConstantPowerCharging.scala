/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs.uncontrolled

import edu.ie3.simona.model.participant.evcs.EvcsModel.{
  ScheduleMap,
  ScheduleEntry,
}
import edu.ie3.simona.model.participant.evcs.{EvModelWrapper, EvcsModel}
import squants.Seconds

import scala.collection.immutable.SortedSet

trait ConstantPowerCharging {
  this: EvcsModel =>

  /** Determine scheduling for charging the EVs currently parked at the charging
    * station until their departure. In this case, each EV is charged with
    * constant power from current time until departure. If less than the maximum
    * power is required to reach 100% SoC, the power is reduced accordingly.
    *
    * @param currentTick
    *   current tick
    * @param evs
    *   currently parked evs at the charging station
    * @return
    *   scheduling for charging the EVs
    */
  def chargeWithConstantPower(
      currentTick: Long,
      evs: Seq[EvModelWrapper],
  ): ScheduleMap = evs
    .filter(ev => ev.storedEnergy < ev.eStorage)
    .map { ev =>
      val maxChargingPower = getMaxAvailableChargingPower(ev)
      val remainingParkingTime = Seconds(ev.departureTick - currentTick)

      val requiredEnergyUntilFull = ev.eStorage - ev.storedEnergy
      val maxChargedEnergyUntilDeparture =
        maxChargingPower * remainingParkingTime
      val actualChargedEnergy =
        requiredEnergyUntilFull.min(maxChargedEnergyUntilDeparture)

      val chargingPower = actualChargedEnergy / remainingParkingTime

      ev.uuid -> SortedSet(
        ScheduleEntry(currentTick, ev.departureTick, chargingPower)
      )
    }
    .toMap
}
