/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs.uncontrolled

import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.model.participant.evcs
import edu.ie3.simona.model.participant.evcs.{
  EvcsChargingScheduleEntry,
  EvcsModel
}
import edu.ie3.util.quantities.PowerSystemUnits.{KILOWATT, KILOWATTHOUR}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.SECOND

import javax.measure.quantity.{Energy, Power}

object SchedulingWithMaximumPower {

  /** Determine scheduling for charging the EVs currently parked at the charging
    * station until their departure. In this case, each EV is charged with
    * maximum power from current time until it reaches either 100% SoC or its
    * departure time.
    *
    * @param evcsModel
    *   evcs model to calculate for / with
    * @param currentTick
    *   current tick
    * @param evs
    *   currently parked evs at the charging station
    * @return
    *   scheduling for charging the EVs
    */
  def calculateNewSchedulingWithMaximumChargingPower(
      evcsModel: EvcsModel,
      currentTick: Long,
      evs: Set[EvModel]
  ): Set[EvcsChargingScheduleEntry] = {

    evs.foldLeft(Set.empty: Set[EvcsChargingScheduleEntry])((schedule, ev) => {

      val newSchedule =
        if (ev.getStoredEnergy.isLessThan(ev.getEStorage)) {

          val chargingPower: ComparableQuantity[Power] =
            evcsModel.getMaxAvailableChargingPower(ev)
          val remainingParkingTime: Long = ev.getDepartureTick - currentTick

          val possibleChargeableEnergyUntilDeparture
              : ComparableQuantity[Energy] =
            chargingPower
              .multiply(
                Quantities.getQuantity(remainingParkingTime.toDouble, SECOND)
              )
              .asType(classOf[Energy])
              .to(KILOWATTHOUR)

          if (
            ev.getStoredEnergy
              .add(possibleChargeableEnergyUntilDeparture)
              .isLessThanOrEqualTo(ev.getEStorage)
          ) {
            schedule + evcs.EvcsChargingScheduleEntry(
              currentTick,
              ev.getDepartureTick,
              ev,
              chargingPower
            )
          } else {
            val chargingTimeUntilFullSoC: Long = (ev.getEStorage
              .subtract(ev.getStoredEnergy)
              .to(KILOWATTHOUR)
              .divide(chargingPower.to(KILOWATT))
              .getValue
              .doubleValue() * 3600).toLong
            schedule + evcs.EvcsChargingScheduleEntry(
              currentTick,
              currentTick + chargingTimeUntilFullSoC,
              ev,
              chargingPower
            )
          }

        } else {
          // if SoC is 100%, no schedule is required
          schedule
        }
      newSchedule

    })

  }

}
