/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWh}
import squants.{Dimensionless, Each, Energy, Power, Seconds}

/** Provides some basic functionality that can be used wherever energy is stored
  * within some kind of storage system.
  */
object ChargingHelper {

  /** Calculates a new total amount of stored energy given an old amount of
    * energy, a charging or discharging power and a time span. Minimum and
    * maximum charging levels are adhered to.
    *
    * @param storedEnergy
    *   The former amount of stored energy.
    * @param power
    *   The gross (dis-)charging power that has been valid from startTick to
    *   endTick.
    * @param startTick
    *   The tick (in seconds) at which charging started.
    * @param endTick
    *   The tick (in seconds) at which charging ended.
    * @param maxEnergy
    *   The maximum allowed stored energy.
    * @param minEnergy
    *   The minimum allowed stored energy, default to 0 Wh.
    * @param eta
    *   The efficiency applied to charging and discharging.
    * @return
    *   The new amount of stored energy.
    */
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

  /** Calculates the next tick (if applicable) at which some target energy level
    * has been reached.
    *
    * @param storedEnergy
    *   The current amount of stored energy.
    * @param power
    *   The gross (dis-)charging power.
    * @param currentTick
    *   The current tick.
    * @param chargingEnergyTarget
    *   The target energy when charging.
    * @param dischargingEnergyTarget
    *   The target energy when discharging.
    * @param eta
    *   The efficiency applied to charging and discharging.
    * @param tolerance
    *   Tolerance for zero charging power.
    * @return
    *   The tick at which the target is reached, if applicable.
    */
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
      val timeSpanTicks = Math.floor(timeSpan.toSeconds).toLong
      currentTick + timeSpanTicks
    }
  }

  /** Calculate net power (after considering efficiency eta).
    *
    * @param setPower
    *   The gross charging or discharging power.
    * @param eta
    *   The efficiency.
    * @return
    *   The net power.
    */
  def calcNetPower(setPower: Power, eta: Dimensionless): Power =
    if (setPower > zeroKW) {
      // multiply eta if we're charging
      setPower * eta.toEach
    } else {
      // divide by eta if we're discharging
      // (draining the battery more than we get as output)
      setPower / eta.toEach
    }
}
