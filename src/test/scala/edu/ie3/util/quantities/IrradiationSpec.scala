/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.quantities

import edu.ie3.util.scala.quantities.{
  WattHoursPerSquareMeter,
  WattsPerSquareMeter,
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import squants.energy.WattHours
import squants.space.SquareMeters
import squants.time.Hours

class IrradiationSpec extends AnyFlatSpec with Matchers {

  behavior of "Irradiation and its Units of Measure"

  it should "create values using UOM factories" in {
    WattHoursPerSquareMeter(
      1
    ).toWattHoursPerSquareMeter should be(1)
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = WattHoursPerSquareMeter(1)

    x.toWattHoursPerSquareMeter should be(1)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    WattHoursPerSquareMeter(1).toString(
      WattHoursPerSquareMeter
    ) should be("1.0 Wh/m²")
  }

  it should "return Energy when multiplied by Area" in {
    WattHoursPerSquareMeter(1) * SquareMeters(10) should be(
      WattHours(10)
    )
  }

  it should "return Irradiance when divided by Time" in {
    WattHoursPerSquareMeter(100) / Hours(10) should be(
      WattsPerSquareMeter(10)
    )
  }
}
