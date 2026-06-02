/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.electro.{Capacitance, ElectricPotential, Resistivity, Volts}
import squants.energy.{Energy, Power, Watts}
import squants.radio.Irradiance
import squants.space.{Area, CubicMeters, Volume}
import squants.thermal.ThermalCapacity
import squants.time.Hours
import squants.{Dimensionless, Each, Time}
import squants.time.Frequency

import scala.math.Pi

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
        that: Dimensionless
    ): ElectricPotential = Volts(
      electricPotential.toVolts * that.toEach
    )
  }

  implicit class RichThermalCapacity(
      thermalCapacity: ThermalCapacity
  ) {
    def toWattHoursPerKelvin: Double =
      this.thermalCapacity.toJoulesPerKelvin / 3600
    def toWattSecondsPerKelvin: Double =
      this.thermalCapacity.toJoulesPerKelvin // Joule == Ws
  }

  implicit class RichIrradiance(
      irradiance: Irradiance
  ) {
    def *(that: Time): Irradiation = WattHoursPerSquareMeter(
      irradiance.toWattsPerSquareMeter * that.toSeconds / Hours(1).toSeconds
    )
  }

  implicit class RichCapacitance(
      electricCapacity: Capacitance
  ) {

    def calculateDielectricLosses(
        voltage: ElectricPotential,
        frequency: Frequency,
        tanDelta: Double,
    ): Power = Watts(
      electricCapacity.toFarads * voltage.toVolts * voltage.toVolts * 2 * Pi * frequency.toHertz * tanDelta
    )
  }

  implicit class RichResistivity(
      electricResistivity: Resistivity
  ) {
    def /(area: Area): ElectricalResistancePerLength = OhmsPerMeter(
      electricResistivity.toOhmMeters / area.toSquareMeters
    )
  }
}
