/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.weather

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.NodeInput
import edu.ie3.simona.ontology.messages.services.WeatherMessage.WeatherData
import edu.ie3.simona.service.weather.WeatherSource.{
  AgentCoordinates,
  WeightedCoordinates
}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.util.TickUtil._
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.mockito.MockitoSugar
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import scala.util.{Failure, Success}

class SampleWeatherSourceSpec
    extends UnitSpec
    with MockitoSugar
    with TableDrivenPropertyChecks {
  implicit val simulationStart: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2011-01-01 00:00:00")
  val source: SampleWeatherSource = new SampleWeatherSource()

  "The sample weather source" should {
    "always return the queried coordinate itself as nearest coordinate" in {
      val queryCoordinate = AgentCoordinates(
        NodeInput.DEFAULT_GEO_POSITION.getY,
        NodeInput.DEFAULT_GEO_POSITION.getX
      )

      source.getWeightedCoordinates(
        queryCoordinate,
        4,
        Quantities.getQuantity(28, PowerSystemUnits.KILOMETRE)
      ) match {
        case Success(WeightedCoordinates(weighting)) =>
          weighting.corresponds(
            Map(NodeInput.DEFAULT_GEO_POSITION -> 1d)
          ) { case ((pointA, weightA), (pointB, weightB)) =>
            pointA.equalsExact(pointB, 1e-6) && Math.abs(
              weightA - weightB
            ) < 1e-6
          }
        case Failure(exception) =>
          fail(
            "Querying the nearest coordinates was supposed to pass.",
            exception
          )
      }
    }

    "return the correct data ticks" in {
      val testData = Table(
        ("start", "end", "expected"),
        (0L, 86400L, (0L to 86400L by 3600L).toArray),
        (1L, 86400L, (3600L to 86400L by 3600L).toArray),
        (0L, 86399L, (0L to 82800L by 3600L).toArray),
        (1L, 86399L, (3600L to 82800L by 3600L).toArray)
      )

      testData.forEvery {
        case (start: Long, end: Long, expected: Array[Long]) =>
          source.getDataTicks(start, end) shouldBe expected
      }
    }

    val getWeatherPrivate = PrivateMethod[WeatherData](Symbol("getWeather"))
    val tick =
      TimeUtil.withDefaults.toZonedDateTime("2011-02-01 15:00:00").toTick

    "return correct weather data in value and unit" in {
      val actual = source invokePrivate getWeatherPrivate(tick)

      /* Units meet expectation */
      actual.diffIrr.getUnit shouldBe StandardUnits.SOLAR_IRRADIANCE
      actual.dirIrr.getUnit shouldBe StandardUnits.SOLAR_IRRADIANCE
      actual.temp.getUnit shouldBe StandardUnits.TEMPERATURE
      actual.windVel.getUnit shouldBe StandardUnits.WIND_VELOCITY

      /* Values meet expectations */
      actual.diffIrr should equalWithTolerance(
        Quantities.getQuantity(72.7656, StandardUnits.SOLAR_IRRADIANCE)
      )
      actual.dirIrr should equalWithTolerance(
        Quantities.getQuantity(80.1172, StandardUnits.SOLAR_IRRADIANCE)
      )
      actual.windVel should equalWithTolerance(
        Quantities.getQuantity(11.11602, StandardUnits.WIND_VELOCITY)
      )
      actual.temp should equalWithTolerance(
        Quantities.getQuantity(6.459, StandardUnits.TEMPERATURE)
      )
    }

    "return correct weather data neglecting the given coordinate" in {
      val weightedCoordinates =
        WeightedCoordinates(Map(NodeInput.DEFAULT_GEO_POSITION -> 1d))

      source.getWeather(tick, weightedCoordinates) match {
        case WeatherData(diffRad, dirRad, temp, windVel) =>
          diffRad.getUnit shouldBe StandardUnits.SOLAR_IRRADIANCE
          diffRad should equalWithTolerance(
            Quantities.getQuantity(72.7656, StandardUnits.SOLAR_IRRADIANCE)
          )

          dirRad.getUnit shouldBe StandardUnits.SOLAR_IRRADIANCE
          dirRad should equalWithTolerance(
            Quantities.getQuantity(80.1172, StandardUnits.SOLAR_IRRADIANCE)
          )

          temp.getUnit shouldBe StandardUnits.TEMPERATURE
          temp should equalWithTolerance(
            Quantities.getQuantity(6.459, StandardUnits.TEMPERATURE)
          )

          windVel.getUnit shouldBe StandardUnits.WIND_VELOCITY
          windVel should equalWithTolerance(
            Quantities.getQuantity(11.11602, StandardUnits.WIND_VELOCITY)
          )
      }
    }
  }
}
