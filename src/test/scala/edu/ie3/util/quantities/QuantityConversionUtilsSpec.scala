/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.quantities

import edu.ie3.util.scala.quantities.QuantityConversionUtils.TemperatureConversionSimona
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import squants.thermal.Celsius
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.CELSIUS

class QuantityConversionUtilsSpec extends AnyFlatSpec with Matchers {

  behavior of "QuantityConversionUtils"

  it should "properly convert from ComparableQuantity[Celsius] to Celsius" in {
    val temperatureQuantity = Quantities.getQuantity(10, CELSIUS)

    temperatureQuantity.toSquants shouldBe Celsius(10)
  }
}
