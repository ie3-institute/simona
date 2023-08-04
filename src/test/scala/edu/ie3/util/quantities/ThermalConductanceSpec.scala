/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.quantities

import edu.ie3.util.scala.quantities.WattsPerKelvin
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import squants.Kelvin
import squants.energy.{KilowattHours, Watts}
import squants.thermal.Celsius
import squants.time.Hours

class ThermalConductanceSpec extends AnyFlatSpec with Matchers {

  behavior of "ThermalConductance and its Units of Measure"

  it should "create values using UOM factories" in {
    WattsPerKelvin(1).toWattsPerKelvin should be(1)
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = WattsPerKelvin(1)

    x.toWattsPerKelvin should be(1)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    WattsPerKelvin(1).toString(WattsPerKelvin) should be("1.0 W/K")
  }

  it should "return Power when multiplied by Temperature in Kelvin" in {
    WattsPerKelvin(1) * Kelvin(10) should be(Watts(10))
  }

  it should "return Power when multiplied by Temperature in Celsius" in {
    WattsPerKelvin(1) * Celsius(0) should be(Watts(273.15))
  }

  it should "return Energy when multiplied by Temperature in Kelvin and Time" in {
    WattsPerKelvin(1000).thermalConductanceToEnergy(
      Kelvin(10),
      Kelvin(0),
      Hours(5)
    ) should be(KilowattHours(50.0))
  }

  it should "return Energy when multiplied by Temperature in Celsius and Time" in {
    WattsPerKelvin(1000).thermalConductanceToEnergy(
      Celsius(0),
      Celsius(10),
      Hours(5)
    ) should be(KilowattHours(1365.75))
  }
}
