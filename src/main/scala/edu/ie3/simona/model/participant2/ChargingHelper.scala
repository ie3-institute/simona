/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWh}
import squants.{Dimensionless, Each, Energy, Power, Seconds}

object ChargingHelper {

  def calcEnergy(
      storedEnergy: Energy,
      power: Power,
      startTick: Long,
      endTick: Long,
      maxEnergy: Energy,
      minEnergy: Energy = zeroKWh,
      eta: Dimensionless = Each(1),
  ): Energy = {
    val timespan = Seconds(endTick - startTick)
    val energyChange = calcNetPower(power, eta) * timespan

    // don't allow under- or overcharge e.g. due to tick rounding error
    minEnergy.max(maxEnergy.min(storedEnergy + energyChange))
  }

  def calcNextEventTick(
      storedEnergy: Energy,
      power: Power,
      currentTick: Long,
      chargingEnergyTarget: () => Energy,
      dischargingEnergyTarget: () => Energy,
      eta: Dimensionless = Each(1),
  )(implicit tolerance: Power): Option[Long] = {
    val netPower = calcNetPower(power, eta)

    val maybeTimeSpan =
      if (netPower ~= zeroKW) {
        // we're at 0 kW, do nothing
        None
      } else if (netPower > zeroKW) {
        val energyToFull = chargingEnergyTarget() - storedEnergy
        Some(energyToFull / netPower)
      } else {
        val energyToEmpty = storedEnergy - dischargingEnergyTarget()
        Some(energyToEmpty / (netPower * -1))
      }

    // calculate the tick from time span
    maybeTimeSpan.map { timeSpan =>
      val timeSpanTicks = Math.round(timeSpan.toSeconds)
      currentTick + timeSpanTicks
    }
  }

  /** Calculate net power (after considering efficiency eta).
    *
    * @param setPower
    *   The gross power
    * @param eta
    *   The efficiency
    * @return
    *   The net power
    */
  private def calcNetPower(setPower: Power, eta: Dimensionless): Power =
    if (setPower > zeroKW) {
      // multiply eta if we're charging
      setPower * eta.toEach
    } else {
      // divide by eta if we're discharging
      // (draining the battery more than we get as output)
      setPower / eta.toEach
    }
}
