/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.Each
import squants.electro.Volts
import squants.energy.Energy
import squants.space.{CubicMeters, Volume}

object SquantsUtils {
  implicit class RichEnergy(energy: Energy) {
    def calcVolume(that: EnergyDensity): Volume = CubicMeters(
      energy.toKilowattHours / that.toKilowattHoursPerCubicMeter
    )
  }
  implicit class RichPower(power: squants.Power) {
    def /(that: ReactivePower): squants.Dimensionless = Each(
      power.toWatts / that.toVars
    )
  }

  implicit class RichElectricPotential(
      electricPotential: squants.electro.ElectricPotential
  ) {
    def multiplyWithDimensionles(
        that: squants.Dimensionless
    ): squants.electro.ElectricPotential = Volts(
      electricPotential.toVolts * that.toEach
    )

  }

}
