/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.quantities

import edu.ie3.util.scala.quantities.KilowattHoursPerCubicMeter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import squants.energy.KilowattHours
import squants.space.CubicMeters

class EnergyDensitySpec extends AnyFlatSpec with Matchers {

  behavior of "EnergyDensityCapacity and its Units of Measure"

  it should "create values using UOM factories" in {
    KilowattHoursPerCubicMeter(
      1
    ).toKilowattHoursPerCubicMeter should be(1)
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = KilowattHoursPerCubicMeter(1)

    x.toKilowattHoursPerCubicMeter should be(1)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    KilowattHoursPerCubicMeter(1).toString(
      KilowattHoursPerCubicMeter
    ) should be("1.0 kWh/m³")
  }

  it should "return Energy when multiplied by Volume" in {
    KilowattHoursPerCubicMeter(1) * CubicMeters(10) should be(
      KilowattHours(10)
    )
  }
}
