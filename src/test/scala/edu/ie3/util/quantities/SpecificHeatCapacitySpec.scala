/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.quantities

import edu.ie3.util.scala.quantities.KilowattHoursPerCubicMeterKelvin
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import squants.energy.KilowattHours
import squants.space.CubicMeters
import squants.thermal.{Celsius, Kelvin}

class SpecificHeatCapacitySpec extends AnyFlatSpec with Matchers {

  behavior of "ThermalCapacitance and its Units of Measure"

  it should "create values using UOM factories" in {
    KilowattHoursPerCubicMeterKelvin(
      1
    ).toKilowattHoursPerKelvinCubicMeters should be(1)
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = KilowattHoursPerCubicMeterKelvin(1)

    x.toKilowattHoursPerKelvinCubicMeters should be(1)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    KilowattHoursPerCubicMeterKelvin(1).toString(
      KilowattHoursPerCubicMeterKelvin
    ) should be("1.0 kWh/m³K")
  }

  it should "return Energy when multiplied by Temperature delta of 1 Kelvin and Volume" in {
    KilowattHoursPerCubicMeterKelvin(1000).calcEnergy(
      Kelvin(10),
      Kelvin(20),
      CubicMeters(5),
    ) should be(KilowattHours(50000.0))
  }

  it should "return Energy when multiplied by Temperature delta of 1 degree Celsius and Volume" in {
    KilowattHoursPerCubicMeterKelvin(1000).calcEnergy(
      Celsius(100),
      Celsius(101),
      CubicMeters(5),
    ) should be(KilowattHours(5000))
  }
}
