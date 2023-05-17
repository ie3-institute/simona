/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.energy.Energy
import squants.space.{CubicMeters, Volume}

object SquantsUtils {
  implicit class RichEnergy(energy: Energy) {
    def divideByEnergyDensity(that: EnergyDensity): Volume = CubicMeters(
      energy.toWattHours / that.toWattHoursPerCubicMeter
    )
  }
}
