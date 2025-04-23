/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.electro.{ElectricPotential, Volts}
import squants.energy.Energy
import squants.space.{CubicMeters, Volume}
import squants.thermal.ThermalCapacity
import squants.{Dimensionless, Each}

object SquantsUtils {
  implicit class RichEnergy(energy: Energy) {
    def calcVolume(that: EnergyDensity): Volume = CubicMeters(
      energy.toKilowattHours / that.toKilowattHoursPerCubicMeter
    )
  }
  implicit class RichPower(power: squants.Power) {
    def /(that: ReactivePower): Dimensionless = Each(
      power.toWatts / that.toVars
    )
  }

  implicit class RichElectricPotential(
      electricPotential: ElectricPotential
  ) {
    def multiplyWithDimensionless(
        that: squants.Dimensionless
    ): ElectricPotential = Volts(
      electricPotential.toVolts * that.toEach
    )

  }

}
