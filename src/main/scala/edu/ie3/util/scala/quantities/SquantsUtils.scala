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

  extension (energy: Energy)
    def calcVolume(that: EnergyDensity): Volume = CubicMeters(
      energy.toKilowattHours / that.toKilowattHoursPerCubicMeter
    )

  extension (power: squants.Power)
    def /(that: ReactivePower): squants.Dimensionless = Each(
      power.toWatts / that.toVars
    )

  extension (electricPotential: squants.electro.ElectricPotential)
    def multiplyWithDimensionles(
        that: squants.Dimensionless
    ): squants.electro.ElectricPotential = Volts(
      electricPotential.toVolts * that.toEach
    )

}
