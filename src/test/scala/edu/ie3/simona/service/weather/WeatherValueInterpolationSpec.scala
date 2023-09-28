/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.weather

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.timeseries.individual.{
  IndividualTimeSeries,
  TimeBasedValue
}
import edu.ie3.datamodel.models.value.{
  SolarIrradianceValue,
  TemperatureValue,
  WeatherValue,
  WindValue
}
import edu.ie3.simona.ontology.messages.services.WeatherMessage.ValueWithWeight
import edu.ie3.simona.service.weather.WeatherValueInterpolationSpec._
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.geo.GeoUtils
import edu.ie3.util.scala.quantities.WattsPerSquareMeter
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.Kelvin
import squants.motion.MetersPerSecond
import squants.thermal.{Celsius, Temperature}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.time.ZonedDateTime
import java.util.UUID
import scala.jdk.CollectionConverters._

class WeatherValueInterpolationSpec
    extends UnitSpec
    with TableDrivenPropertyChecks {
  "The WeatherValueInterpolation" should {
    val getValue = PrivateMethod[Option[ValueWithWeight[_]]](Symbol("getValue"))

    "find correct previous values" in {
      val cases = Table(
        (
          "timeBasedValues",
          "expectedDiffIrr",
          "expectedDirIrr",
          "expectedTemp",
          "expectedWindVel"
        ),
        (
          Set(
            timeBasedValue0,
            timeBasedValue1,
            timeBasedValue2,
            timeBasedValue3,
            timeBasedValue4
          ),
          Some(ValueWithWeight(WattsPerSquareMeter(50d), 1800)),
          Some(ValueWithWeight(WattsPerSquareMeter(50d), 1800)),
          Some(ValueWithWeight(Temperature(238.15d, Kelvin), 6000)),
          Some(ValueWithWeight(MetersPerSecond(10d), 6000))
        ),
        (Set(timeBasedValue0), None, None, None, None)
      )

      forAll(cases) {
        (
            timeBasedValues,
            expectedDiffIrr,
            expectedDirIrr,
            expectedTemp,
            expectedWindVel
        ) =>
          val timeSeries: IndividualTimeSeries[WeatherValue] =
            buildTimeSeries(timeBasedValues)
          val intervalStart: ZonedDateTime = time.minusHours(2)

          WeatherValueInterpolation invokePrivate getValue(
            timeSeries,
            time,
            intervalStart,
            time,
            "diffIrr"
          ) shouldBe expectedDiffIrr

          WeatherValueInterpolation invokePrivate getValue(
            timeSeries,
            time,
            intervalStart,
            time,
            "diffIrr"
          ) shouldBe expectedDirIrr

          WeatherValueInterpolation invokePrivate getValue(
            timeSeries,
            time,
            intervalStart,
            time,
            "temp"
          ) shouldBe expectedTemp

          WeatherValueInterpolation invokePrivate getValue(
            timeSeries,
            time,
            intervalStart,
            time,
            "windVel"
          ) shouldBe expectedWindVel
      }
    }

    "find correct next values" in {
      val cases = Table(
        (
          "timeBasedValues",
          "expectedDiffIrr",
          "expectedDirIrr",
          "expectedTemp",
          "expectedWindVel"
        ),
        (
          Set(
            timeBasedValue0,
            timeBasedValue1,
            timeBasedValue2,
            timeBasedValue3,
            timeBasedValue4,
            timeBasedValue5
          ),
          Some(ValueWithWeight(WattsPerSquareMeter(60d), 1800)),
          Some(ValueWithWeight(WattsPerSquareMeter(60d), 1800)),
          Some(ValueWithWeight(Temperature(15d, Celsius), 1800)),
          Some(ValueWithWeight(MetersPerSecond(20d), 1800))
        ),
        (
          Set(timeBasedValue0, timeBasedValue4, timeBasedValue5),
          Some(ValueWithWeight(WattsPerSquareMeter(40d), 6000)),
          Some(ValueWithWeight(WattsPerSquareMeter(40d), 6000)),
          None,
          None
        )
      )

      forAll(cases) {
        (
            timeBasedValues,
            expectedDiffIrr,
            expectedDirIrr,
            expectedTemp,
            expectedWindVel
        ) =>
          val timeSeries: IndividualTimeSeries[WeatherValue] =
            buildTimeSeries(timeBasedValues)
          val intervalEnd: ZonedDateTime = time.plusHours(2)

          WeatherValueInterpolation invokePrivate getValue(
            timeSeries,
            time,
            time,
            intervalEnd,
            "diffIrr"
          ) shouldBe expectedDiffIrr

          WeatherValueInterpolation invokePrivate getValue(
            timeSeries,
            time,
            time,
            intervalEnd,
            "diffIrr"
          ) shouldBe expectedDirIrr

          WeatherValueInterpolation invokePrivate getValue(
            timeSeries,
            time,
            time,
            intervalEnd,
            "temp"
          ) shouldBe expectedTemp

          WeatherValueInterpolation invokePrivate getValue(
            timeSeries,
            time,
            time,
            intervalEnd,
            "windVel"
          ) shouldBe expectedWindVel
      }
    }

    "get a value correctly" in {
      val getValue = PrivateMethod[Option[_]](Symbol("getValue"))

      val weatherValue = new WeatherValue(
        coordinate67775,
        new SolarIrradianceValue(
          Quantities.getQuantity(50d, StandardUnits.SOLAR_IRRADIANCE),
          Quantities.getQuantity(50d, StandardUnits.SOLAR_IRRADIANCE)
        ),
        new TemperatureValue(Quantities.getQuantity(238.15d, Units.KELVIN)),
        new WindValue(
          missingValue,
          Quantities.getQuantity(10d, Units.METRE_PER_SECOND)
        )
      )

      val cases = Table(
        ("typeString", "expectedValue"),
        ("diffIrr", Some(WattsPerSquareMeter(50d))),
        ("dirIrr", Some(WattsPerSquareMeter(50d))),
        ("temp", Some(Temperature(238.15d, Kelvin))),
        ("windVel", Some(MetersPerSecond(10d))),
        ("other", None)
      )

      forAll(cases) { (typeString, expectedValue) =>
        WeatherValueInterpolation invokePrivate getValue(
          weatherValue,
          typeString
        ) shouldBe expectedValue
      }
    }
  }
}

case object WeatherValueInterpolationSpec {
  private val coordinate67775 = GeoUtils.buildPoint(51.5, 7.438)
  private val time: ZonedDateTime = ZonedDateTime.now()
  private val missingValue: Null = null

  private val timeBasedValue0: TimeBasedValue[WeatherValue] =
    buildTimeBasedValue(
      time.minusMinutes(130),
      new WeatherValue(
        coordinate67775,
        new SolarIrradianceValue(missingValue, missingValue),
        new TemperatureValue(
          missingValue
        ),
        new WindValue(missingValue, missingValue)
      )
    )

  private val timeBasedValue1: TimeBasedValue[WeatherValue] =
    buildTimeBasedValue(
      time.minusMinutes(100),
      new WeatherValue(
        coordinate67775,
        new SolarIrradianceValue(
          missingValue,
          Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE)
        ),
        new TemperatureValue(Quantities.getQuantity(238.15d, Units.KELVIN)),
        new WindValue(
          missingValue,
          Quantities.getQuantity(10, Units.METRE_PER_SECOND)
        )
      )
    )

  private val timeBasedValue2: TimeBasedValue[WeatherValue] =
    buildTimeBasedValue(
      time.minusMinutes(30),
      new WeatherValue(
        coordinate67775,
        new SolarIrradianceValue(
          Quantities.getQuantity(50d, StandardUnits.SOLAR_IRRADIANCE),
          Quantities.getQuantity(50d, StandardUnits.SOLAR_IRRADIANCE)
        ),
        new TemperatureValue(missingValue),
        new WindValue(missingValue, missingValue)
      )
    )

  private val timeBasedValue3: TimeBasedValue[WeatherValue] =
    buildTimeBasedValue(
      time.plusMinutes(30),
      new WeatherValue(
        coordinate67775,
        new SolarIrradianceValue(
          missingValue,
          Quantities.getQuantity(60d, StandardUnits.SOLAR_IRRADIANCE)
        ),
        new TemperatureValue(Quantities.getQuantity(288.15d, Units.KELVIN)),
        new WindValue(
          missingValue,
          Quantities.getQuantity(20, Units.METRE_PER_SECOND)
        )
      )
    )

  private val timeBasedValue4: TimeBasedValue[WeatherValue] =
    buildTimeBasedValue(
      time.plusMinutes(100),
      new WeatherValue(
        coordinate67775,
        new SolarIrradianceValue(
          Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE),
          Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE)
        ),
        new TemperatureValue(missingValue),
        new WindValue(missingValue, missingValue)
      )
    )

  private val timeBasedValue5: TimeBasedValue[WeatherValue] =
    buildTimeBasedValue(
      time.plusMinutes(130),
      new WeatherValue(
        coordinate67775,
        new SolarIrradianceValue(
          missingValue,
          Quantities.getQuantity(50d, StandardUnits.SOLAR_IRRADIANCE)
        ),
        new TemperatureValue(Quantities.getQuantity(288.15d, Units.KELVIN)),
        new WindValue(missingValue, missingValue)
      )
    )

  def buildTimeBasedValue(
      time: ZonedDateTime,
      value: WeatherValue
  ): TimeBasedValue[WeatherValue] = {
    new TimeBasedValue[WeatherValue](UUID.randomUUID(), time, value)
  }

  def buildTimeSeries(
      set: Set[TimeBasedValue[WeatherValue]]
  ): IndividualTimeSeries[WeatherValue] = {
    new IndividualTimeSeries[WeatherValue](UUID.randomUUID(), set.asJava)
  }
}
