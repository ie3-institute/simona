/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.quantities

import edu.ie3.simona.test.matchers.SquantsMatchers
import edu.ie3.util.DoubleUtils.~=
import edu.ie3.util.scala.quantities.QuantityConversionUtils.TemperatureConversionSimona
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import squants.thermal.Celsius
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.{CELSIUS, KELVIN}

class QuantityConversionUtilsSpec
    extends AnyFlatSpec
    with Matchers
    with SquantsMatchers {
  implicit val doubleTolerance: Double = 1e-10

  behavior of "QuantityConversionUtils"

  it should "properly convert from ComparableQuantity[Celsius] to squants temperatures and its double values" in {
    val temperatureQuantityCelsius = Quantities.getQuantity(10, CELSIUS)

    temperatureQuantityCelsius.toSquants shouldBe Celsius(10)
    temperatureQuantityCelsius.toSquants.value shouldBe 10.0
    temperatureQuantityCelsius.toSquants.toCelsiusDegrees shouldBe 10.0
    temperatureQuantityCelsius.toSquants.toCelsiusScale shouldBe 10.0
    temperatureQuantityCelsius.toSquants.toKelvinDegrees shouldBe 10.0
    temperatureQuantityCelsius.toSquants.toKelvinScale shouldBe 283.15

    val temperatureQuantityKelvin = Quantities.getQuantity(100, KELVIN)

    temperatureQuantityKelvin.toSquants shouldBe Celsius(-173.15)
    temperatureQuantityKelvin.toSquants.value shouldBe -173.15
    temperatureQuantityKelvin.toSquants.toCelsiusDegrees shouldBe -173.15
    temperatureQuantityKelvin.toSquants.toCelsiusScale shouldBe -173.15
    temperatureQuantityKelvin.toSquants.toKelvinDegrees shouldBe -173.15
    (temperatureQuantityKelvin.toSquants.toKelvinScale ~= 100d) shouldBe true
  }
}
