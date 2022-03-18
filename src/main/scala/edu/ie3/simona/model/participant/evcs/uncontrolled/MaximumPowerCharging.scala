/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs.uncontrolled

import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.model.participant.evcs
import edu.ie3.simona.model.participant.evcs.ChargingSchedule.Entry
import edu.ie3.simona.model.participant.evcs.{
  ChargingSchedule,
  EvcsChargingScheduleEntry,
  EvcsModel
}
import edu.ie3.util.quantities.PowerSystemUnits.{KILOWATT, KILOWATTHOUR}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.SECOND

import javax.measure.quantity.{Energy, Power}

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
      evs: Set[EvModel]
  ): Map[EvModel, Option[ChargingSchedule]] = evs.map { ev =>
    ev -> Option.when(ev.getStoredEnergy.isLessThan(ev.getEStorage)) {
      val chargingPower = getMaxAvailableChargingPower(ev)
      val remainingParkingTime = ev.getDepartureTick - currentTick

      val possibleChargeableEnergyUntilDeparture = chargingPower
        .multiply(Quantities.getQuantity(remainingParkingTime.toDouble, SECOND))
        .asType(classOf[Energy])
        .to(KILOWATTHOUR)

      val endTick: Long =
        if (
          ev.getStoredEnergy
            .add(possibleChargeableEnergyUntilDeparture)
            .isLessThanOrEqualTo(ev.getEStorage)
        ) {
          /* Charge with full power, if battery can accommodate the energy */
          ev.getDepartureTick
        } else {
          /* Charge only until the car is full */
          (ev.getEStorage
            .subtract(ev.getStoredEnergy)
            .to(KILOWATTHOUR)
            .divide(chargingPower.to(KILOWATT))
            .getValue
            .doubleValue() * 3600).toLong
        }

      ChargingSchedule(ev, Seq(Entry(currentTick, endTick, chargingPower)))
    }
  }.toMap
}
