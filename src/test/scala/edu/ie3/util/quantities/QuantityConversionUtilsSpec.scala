/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.quantities

import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.DoubleUtils.~=
import edu.ie3.util.quantities.PowerSystemUnits.*
import edu.ie3.util.scala.quantities.QuantityConversionUtils.*
import edu.ie3.util.scala.quantities.{
  ApparentPower,
  EuroPerKilowatthour,
  Kilovoltamperes,
  KilowattHoursPerKelvinCubicMeters,
}
import squants.electro.*
import squants.energy.{KilowattHours, Kilowatts}
import squants.space.{CubicMeters, SquareMeters}
import squants.thermal.Celsius
import squants.{Amperes, Each, Radians, Temperature}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.*
import javax.measure.quantity.*

class QuantityConversionUtilsSpec extends UnitSpec {
  implicit val doubleTolerance: Double = 1e-10
  implicit val temperatureTolerance: Temperature = Celsius(1e-10)

  "QuantityConversionUtils" should {
    "properly convert from ComparableQuantity[Temperature] to squants temperatures and its double values" in {
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

    "properly convert dimensionless quantities" in {
      val dimensionless = Quantities.getQuantity(0.95, PU)
      dimensionless.toSquants shouldBe Each(0.95)
    }

    "properly convert voltage quantities" in {
      val voltage = Quantities.getQuantity(110.0, KILOVOLT)
      voltage.toSquants shouldBe Kilovolts(110.0)
    }

    "properly convert current quantities" in {
      val current = Quantities.getQuantity(42.5, AMPERE)
      current.toSquants shouldBe Amperes(42.5)
    }

    "properly convert power quantities" in {
      val activePower = Quantities.getQuantity(50.0, KILOWATT)
      activePower.toSquants shouldBe Kilowatts(50.0)

      val apparentPower = Quantities.getQuantity(60.0, KILOVOLTAMPERE)
      apparentPower.toApparent shouldBe Kilovoltamperes(60.0)
    }

    "properly convert energy quantities" in {
      val energy = Quantities.getQuantity(123.45, KILOVARHOUR)
      energy.toSquants shouldBe KilowattHours(123.45)
    }

    "properly convert energy price quantities" in {
      val price = Quantities.getQuantity(0.25, EURO_PER_KILOWATTHOUR)
      price.toSquants shouldBe EuroPerKilowatthour(0.25)
    }

    "properly convert electrical resistance quantities" in {
      val resistance = Quantities.getQuantity(15.0, OHM)
      resistance.toSquants shouldBe Ohms(15.0)
    }

    "properly convert specific resistance quantities" in {
      implicit val length: ComparableQuantity[Length] =
        Quantities.getQuantity(2.5, KILOMETRE)
      val specResistance = Quantities.getQuantity(0.2, OHM_PER_KILOMETRE)
      specResistance.toSquants shouldBe Ohms(0.5)
    }

    "properly convert electrical conductance quantities" in {
      val conductance = Quantities.getQuantity(0.1, SIEMENS)
      conductance.toSquants shouldBe Siemens(0.1)
    }

    "properly convert specific conductance quantities" in {
      implicit val length: ComparableQuantity[Length] =
        Quantities.getQuantity(10.0, KILOMETRE)
      val specConductance =
        Quantities.getQuantity(0.05, SIEMENS_PER_KILOMETRE)
      specConductance.toSquants shouldBe Siemens(0.5)
    }

    "properly convert area quantities" in {
      val area = Quantities.getQuantity(500.0, SQUARE_METRE)
      area.toSquants shouldBe SquareMeters(500.0)
    }

    "properly convert angle quantities" in {
      val angle = Quantities.getQuantity(Math.PI / 2, RADIAN)
      angle.toSquants shouldBe Radians(Math.PI / 2)
    }

    "properly convert volume quantities" in {
      val volume = Quantities.getQuantity(7.5, CUBIC_METRE)
      volume.toSquants shouldBe CubicMeters(7.5)
    }

    "properly convert specific heat capacity quantities" in {
      val specHeatCapacity = Quantities.getQuantity(
        1.15,
        KILOWATTHOUR_PER_KELVIN_TIMES_CUBICMETRE,
      )
      specHeatCapacity.toSquants shouldBe KilowattHoursPerKelvinCubicMeters(
        1.15
      )
    }
  }
}
