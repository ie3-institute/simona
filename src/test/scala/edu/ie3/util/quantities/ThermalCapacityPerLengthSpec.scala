/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.quantities

import edu.ie3.util.scala.quantities.{
  JoulesPerMeterKelvin,
  KilowattHoursPerMeterKelvin,
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import squants.energy.KilowattHours
import squants.space.Meters
import squants.thermal.{Celsius, JoulesPerKelvin, Kelvin}

class ThermalCapacityPerLengthSpec extends AnyFlatSpec with Matchers {

  behavior of "ThermalCapacityPerLength and its Units of Measure"

  it should "create values using UOM factories" in {
    KilowattHoursPerMeterKelvin(1).toJoulesPerMeterKelvin should be(3600000.0)
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = JoulesPerMeterKelvin(3600000.0)

    x.toJoulesPerMeterKelvin should be(3600000.0)
    x.to(KilowattHoursPerMeterKelvin) should be(1.0)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    JoulesPerMeterKelvin(1).toString(JoulesPerMeterKelvin) should be(
      "1.0 J/(mK)"
    )
  }

  it should "return Energy when multiplied by Temperature delta and Length" in {
    JoulesPerMeterKelvin(3600000.0).calcEnergy(
      Kelvin(10),
      Kelvin(20),
      Meters(5),
    ) should be(KilowattHours(50.0))
  }

  it should "return ThermalCapacity when multiplied by Length" in {
    JoulesPerMeterKelvin(1000) * Meters(3) should be(JoulesPerKelvin(3000.0))
  }
}
