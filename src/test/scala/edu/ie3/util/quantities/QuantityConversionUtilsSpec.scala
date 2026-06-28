/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.quantities

import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.quantities.PowerSystemUnits.*
import edu.ie3.util.scala.quantities.*
import edu.ie3.util.scala.quantities.QuantityConversionUtils.*
import squants.electro.*
import squants.energy.{Energy, KilowattHours, Kilowatts}
import squants.space.{CubicMeters, Meters, Millimeters, SquareMeters}
import squants.thermal.Celsius
import squants.{Amperes, Each, Radians, Temperature}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.*

import java.util.Optional
import javax.measure.quantity.*

class QuantityConversionUtilsSpec extends UnitSpec {
  implicit val doubleTolerance: Double = 1e-10
  implicit val temperatureTolerance: Temperature = Celsius(1e-10)
  implicit val energyTolerance: Energy = KilowattHours(1e-9)

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
      temperatureQuantityKelvin.toSquants.value should approximate(-173.15)
      temperatureQuantityKelvin.toSquants.toCelsiusDegrees should
        approximate(-173.15)
      temperatureQuantityKelvin.toSquants.toCelsiusScale should
        approximate(-173.15)
      temperatureQuantityKelvin.toSquants.toKelvinDegrees should
        approximate(-173.15)
      temperatureQuantityKelvin.toSquants.toKelvinScale should approximate(100d)
    }

    "properly convert dimensionless quantities" in {
      val dimensionless = Quantities.getQuantity(0.95, PU)
      dimensionless.toSquants shouldBe Each(0.95)
    }

    "properly convert voltage quantities from different units" in {
      val voltage = Quantities.getQuantity(110.0, KILOVOLT)
      voltage.toSquants shouldBe Kilovolts(110.0)

      val voltageV = Quantities.getQuantity(10000, VOLT)
      voltageV.toSquants shouldBe Kilovolts(10.0)
    }

    "properly convert current quantities" in {
      val current = Quantities.getQuantity(42.5, AMPERE)
      current.toSquants shouldBe Amperes(42.5)
    }

    "properly convert power quantities from different units" in {
      val activePower = Quantities.getQuantity(50.0, KILOWATT)
      activePower.toSquants shouldBe Kilowatts(50.0)

      val apparentPower = Quantities.getQuantity(60.0, KILOVOLTAMPERE)
      apparentPower.toApparent shouldBe Kilovoltamperes(60.0)

      val activePowerMW = Quantities.getQuantity(1.0, MEGAWATT)
      activePowerMW.toSquants shouldBe Kilowatts(1000.0)

      val apparentPowerVA = Quantities.getQuantity(15500, VOLTAMPERE)
      apparentPowerVA.toApparent shouldBe Kilovoltamperes(15.5)
    }

    "properly convert energy quantities from different units" in {
      val energy = Quantities.getQuantity(123.45, KILOWATTHOUR)
      energy.toSquants shouldBe KilowattHours(123.45)

      val energyMwh = Quantities.getQuantity(1.5, MEGAVARHOUR)
      energyMwh.toSquants should approximate(KilowattHours(1500))
    }

    "properly convert energy price quantities" in {
      val price = Quantities.getQuantity(0.25, EURO_PER_KILOWATTHOUR)
      price.toSquants shouldBe EuroPerKilowattHour(0.25)
    }

    "properly convert electrical resistance quantities from different units" in {
      val resistance = Quantities.getQuantity(15.0, OHM)
      resistance.toSquants shouldBe Ohms(15.0)

      val resistanceMilliOhm = Quantities.getQuantity(1000, MILLIOHM)
      resistanceMilliOhm.toSquants shouldBe Ohms(1)
    }

    "properly convert specific resistance quantities" in {
      given length: ComparableQuantity[Length] =
        Quantities.getQuantity(2.5, KILOMETRE)
      val specResistance = Quantities.getQuantity(0.2, OHM_PER_KILOMETRE)
      specResistance.toResistance shouldBe Ohms(0.5)
      specResistance.toResistancePerLength shouldBe OhmsPerKilometer(0.2)

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

    "properly convert length quantities from different units" in {
      val length = Quantities.getQuantity(500.0, MILLIMETRE)
      length.toSquants shouldBe Millimeters(500.0)

      val length2 = Quantities.getQuantity(0.1, METRE)
      length2.toSquants shouldBe Meters(0.1)
    }

    "properly convert option of length quantities from different units" in {
      val length = Optional.of(Quantities.getQuantity(500.0, MILLIMETRE))
      length.toSquants shouldBe Some(Millimeters(500.0))
    }

    "properly convert area quantities from different units" in {
      val area = Quantities.getQuantity(500.0, SQUARE_METRE)
      area.toSquants shouldBe SquareMeters(500.0)

      val areaKm2 =
        Quantities.getQuantity(0.01, KILOMETRE.pow(2).asType(classOf[Area]))
      areaKm2.toSquants shouldBe SquareMeters(10000.0)
    }

    "properly convert option of an area quantities from different units" in {
      val area = Optional.of(Quantities.getQuantity(500.0, SQUARE_METRE))
      area.toSquants shouldBe Some(SquareMeters(500.0))
    }

    "properly convert energy price quantities from different units" in {
      val price = Quantities.getQuantity(50.0, EURO_PER_KILOWATTHOUR)
      price.toSquants shouldBe EuroPerKilowattHour(50.0)
    }

    "properly convert electric capacitance quantities from different units" in {
      val capacitance = Quantities.getQuantity(0.05, FARAD)
      capacitance.toSquants shouldBe Farads(0.05)
    }

    "properly convert thermal resistivity quantities from different units" in {
      val thermalRes = Quantities.getQuantity(0.05, KELVIN_METRE_PER_WATT)
      thermalRes.toSquants shouldBe KelvinMetersPerWatt(0.05)
    }

    "properly convert thermal capacitance quantities from different units" in {
      val thermalCapa =
        Quantities.getQuantity(2.0, JOULE_PER_CUBIC_METRE_KELVIN)
      thermalCapa.toSquants shouldBe JoulesPerMeterKelvin(
        2.0
      ) // FIXME JoulesPerMeter or CubicMeter
    }

    "properly convert angle quantities from different units" in {
      val angle = Quantities.getQuantity(Math.PI / 2, RADIAN)
      angle.toSquants shouldBe Radians(Math.PI / 2)

      val angleDeg = Quantities.getQuantity(90.0, DEGREE_GEOM)
      angleDeg.toSquants shouldBe Radians(Math.PI / 2)
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
