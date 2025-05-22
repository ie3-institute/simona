/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.quantities

import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.DoubleUtils.~=
import edu.ie3.util.scala.quantities.QuantityConversionUtils.TemperatureConversionSimona
import squants.thermal.{Celsius, Temperature}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.{CELSIUS, KELVIN}

class QuantityConversionUtilsSpec extends UnitSpec {
  implicit val doubleTolerance: Double = 1e-10
  implicit val temperatureTolerance: Temperature = Celsius(1e-10)

  "QuantityConversionUtils" should {
    "properly convert from ComparableQuantity[Celsius] to squants temperatures and its double values" in {
      val temperatureQuantityCelsius = Quantities.getQuantity(10, CELSIUS)

      temperatureQuantityCelsius.toSquants shouldBe Celsius(10)
      temperatureQuantityCelsius.toSquants.value shouldBe 10.0
      temperatureQuantityCelsius.toSquants.toCelsiusDegrees shouldBe 10.0
      temperatureQuantityCelsius.toSquants.toCelsiusScale shouldBe 10.0
      temperatureQuantityCelsius.toSquants.toKelvinDegrees shouldBe 10.0
      temperatureQuantityCelsius.toSquants.toKelvinScale shouldBe 283.15

      val temperatureQuantityKelvin = Quantities.getQuantity(100, KELVIN)

      temperatureQuantityKelvin.toSquants should approximate(Celsius(-173.15))
      (temperatureQuantityKelvin.toSquants.value ~= -173.15) shouldBe true
      (temperatureQuantityKelvin.toSquants.toCelsiusDegrees ~= -173.15) shouldBe true
      (temperatureQuantityKelvin.toSquants.toCelsiusScale ~= -173.15) shouldBe true
      (temperatureQuantityKelvin.toSquants.toKelvinDegrees ~= -173.15) shouldBe true
      (temperatureQuantityKelvin.toSquants.toKelvinScale ~= 100d) shouldBe true
    }
  }
}
