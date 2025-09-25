/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.quantities

import edu.ie3.simona.test.matchers.SquantsMatchers
import edu.ie3.util.scala.quantities.{
  KilowattHoursPerCubicMeter,
  KilowattHoursPerKelvinCubicMeters,
}
import edu.ie3.util.scala.quantities.SquantsUtils.{
  RichElectricPotential,
  RichEnergy,
  RichSpecificHeatCapacity,
  RichThermalCapacity,
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import squants.electro.Kilovolts
import squants.energy.{Energy, KilowattHours}
import squants.space.CubicMeters
import squants.thermal.{Celsius, JoulesPerKelvin}
import squants.{Each, Kelvin}

class SquantsUtilsSpec extends AnyFlatSpec with Matchers with SquantsMatchers {
  given energyTolerance: Energy = KilowattHours(1e-12)

  behavior of "SquantsUtils and its Units of Measure"

  it should "return Volume when Energy is multiplied by Energy Density" in {
    KilowattHours(1000).calcVolume(KilowattHoursPerCubicMeter(4)) should be(
      CubicMeters(250.0)
    )
  }

  it should "return Voltage when an ElectricPotential is multiplied by Dimensionless" in {
    Kilovolts(10).multiplyWithDimensionless(Each(2)) should be(Kilovolts(20.0))
  }

  it should "return Energy when a SpecificHeatCapacity is multiplied with Volume and temperature delta in Celsius" in {
    KilowattHoursPerKelvinCubicMeters(1.16).calcEnergyToHeat(
      CubicMeters(4),
      Celsius(12),
    ) should approximate(
      KilowattHours(55.68)
    )
  }

  it should "return Energy when a SpecificHeatCapacity is multiplied with Volume and temperature delta in Kelvin" in {
    KilowattHoursPerKelvinCubicMeters(1.16).calcEnergyToHeat(
      CubicMeters(4),
      Kelvin(12),
    ) should approximate(
      KilowattHours(55.68)
    )
  }

  it should "convert a ThermalCapacity to correct value in WattHoursPerKelvin" in {
    JoulesPerKelvin(36000).toWattHoursPerKelvin should be(10)
  }

  it should "convert a ThermalCapacity to correct value in WattSecondsPerKelvin" in {
    JoulesPerKelvin(3600).toWattSecondsPerKelvin should be(3600.0)
  }
}
