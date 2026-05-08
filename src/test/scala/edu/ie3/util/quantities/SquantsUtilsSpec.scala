/*
 * © 2025-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.quantities

import edu.ie3.util.scala.quantities.KilowattHoursPerCubicMeter
import edu.ie3.util.scala.quantities.SquantsUtils.{
  RichElectricPotential,
  RichEnergy,
  RichThermalCapacity,
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import squants.Each
import squants.electro.Kilovolts
import squants.energy.KilowattHours
import squants.space.CubicMeters
import squants.thermal.JoulesPerKelvin

class SquantsUtilsSpec extends AnyFlatSpec with Matchers {

  behavior of "SquantsUtils and its Units of Measure"

  it should "return Volume when Energy is multiplied by Energy Density" in {
    KilowattHours(1000).calcVolume(KilowattHoursPerCubicMeter(4)) should be(
      CubicMeters(250.0)
    )
  }

  it should "return Voltage when an ElectricPotential is multiplied by Dimensionless" in {
    Kilovolts(10).multiplyWithDimensionless(Each(2)) should be(Kilovolts(20.0))
  }

  it should "convert a ThermalCapacity to correct value in WattHoursPerKelvin" in {
    JoulesPerKelvin(36000).toWattHoursPerKelvin should be(10)
  }

  it should "convert a ThermalCapacity to correct value in WattSecondsPerKelvin" in {
    JoulesPerKelvin(3600).toWattSecondsPerKelvin should be(3600.0)
  }
}
