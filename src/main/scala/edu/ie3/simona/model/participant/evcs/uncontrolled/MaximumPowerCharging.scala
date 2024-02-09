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

trait MaximumPowerCharging {
  this: EvcsModel =>

  /** Determine scheduling for charging the EVs currently parked at the charging
    * station until their departure. In this case, each EV is charged with
    * maximum power from current time until it reaches either 100% SoC or its
    * departure time.
    *
    * @param currentTick
    *   current tick
    * @param evs
    *   currently parked evs at the charging station
    * @return
    *   scheduling for charging the EVs
    */
  def chargeWithMaximumPower(
      currentTick: Long,
      evs: Seq[EvModelWrapper],
  ): ScheduleMap = evs
    .filter(ev => ev.storedEnergy < ev.eStorage)
    .map { ev =>
      val chargingPower = getMaxAvailableChargingPower(ev)
      val remainingParkingTime =
        Seconds(ev.departureTick - currentTick)

      val possibleChargeableEnergyUntilDeparture =
        chargingPower * remainingParkingTime

      val endTick: Long =
        if (
          ev.storedEnergy + possibleChargeableEnergyUntilDeparture <= ev.eStorage
        ) {
          /* Charge with full power, if battery can accommodate the energy */
          ev.departureTick
        } else {
          /* Charge only until the car is full */
          ((ev.eStorage - ev.storedEnergy) / chargingPower).toSeconds.toLong + currentTick
        }

      ev.uuid ->
        SortedSet(ScheduleEntry(currentTick, endTick, chargingPower))
    }
    .toMap
}
