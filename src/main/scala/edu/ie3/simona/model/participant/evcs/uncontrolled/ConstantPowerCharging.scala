/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs.uncontrolled

import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.model.participant.evcs.ChargingSchedule.Entry
import edu.ie3.simona.model.participant.evcs.{ChargingSchedule, EvcsModel}
import edu.ie3.util.quantities.PowerSystemUnits.{KILOWATT, KILOWATTHOUR}
import edu.ie3.util.scala.quantities.QuantityUtil.RichQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.SECOND

import javax.measure.quantity.{Energy, Power}

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
      evs: Set[EvModel]
  ): Map[EvModel, Option[ChargingSchedule]] = evs.map { ev =>
    ev -> Option.when(ev.getStoredEnergy.isLessThan(ev.getEStorage)) {
      val maxChargingPower = getMaxAvailableChargingPower(ev)
      val remainingParkingTime = ev.getDepartureTick - currentTick

      val requiredEnergyUntilFull =
        ev.getEStorage.subtract(ev.getStoredEnergy)
      val maxChargedEnergyUntilDeparture = maxChargingPower
        .multiply(Quantities.getQuantity(remainingParkingTime, SECOND))
        .asType(classOf[Energy])
        .to(KILOWATTHOUR)
      val actualChargedEnergy =
        requiredEnergyUntilFull.min(maxChargedEnergyUntilDeparture)

      val chargingPower = actualChargedEnergy
        .divide(Quantities.getQuantity(remainingParkingTime, SECOND))
        .asType(classOf[Power])
        .to(KILOWATT)

      ChargingSchedule(
        ev,
        Seq(Entry(currentTick, ev.getDepartureTick, chargingPower))
      )
    }
  }.toMap
}
