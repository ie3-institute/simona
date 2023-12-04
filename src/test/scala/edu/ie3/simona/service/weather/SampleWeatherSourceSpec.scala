/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.weather

import edu.ie3.datamodel.models.input.NodeInput
import edu.ie3.simona.ontology.messages.services.WeatherMessage.WeatherData
import edu.ie3.simona.service.weather.WeatherSource.{
  AgentCoordinates,
  WeightedCoordinates
}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.util.TickUtil.*
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.quantities.{Irradiance, WattsPerSquareMeter}
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.mockito.MockitoSugar
import squants.{Temperature, Velocity}
import squants.motion.MetersPerSecond
import squants.thermal.Celsius

import java.time.ZonedDateTime
import scala.util.{Failure, Success}

class SampleWeatherSourceSpec
    extends UnitSpec
    with MockitoSugar
    with TableDrivenPropertyChecks {
  given simulationStart: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2011-01-01 00:00:00")
  given toleranceIrradiance: Irradiance = WattsPerSquareMeter(0.1)
  given toleranceVelocity: Velocity = MetersPerSecond(0.01)
  given toleranceTemperature: Temperature = Celsius(0.01)
  val source: SampleWeatherSource = new SampleWeatherSource()

  "The sample weather source" should {
    "always return the queried coordinate itself as nearest coordinate" in {
      val queryCoordinate = AgentCoordinates(
        NodeInput.DEFAULT_GEO_POSITION.getY,
        NodeInput.DEFAULT_GEO_POSITION.getX
      )

      source.getWeightedCoordinates(
        queryCoordinate,
        4
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
      val actual = source.invokePrivate(getWeatherPrivate(tick))

      /* Units meet expectation */
      actual.diffIrr.unit shouldBe WattsPerSquareMeter
      actual.dirIrr.unit shouldBe WattsPerSquareMeter
      actual.temp.unit shouldBe Celsius
      actual.windVel.unit shouldBe MetersPerSecond

      /* Values meet expectations */
      (actual.diffIrr ~= WattsPerSquareMeter(72.7656)) shouldBe true

      (actual.dirIrr ~= WattsPerSquareMeter(80.1172)) shouldBe true

      (actual.windVel ~= MetersPerSecond(11.11602)) shouldBe true

      (actual.temp ~= Celsius(6.459)) shouldBe true

    }

    "return correct weather data neglecting the given coordinate" in {
      val weightedCoordinates =
        WeightedCoordinates(Map(NodeInput.DEFAULT_GEO_POSITION -> 1d))

      source.getWeather(tick, weightedCoordinates) match {
        case WeatherData(diffIrr, dirIrr, temp, windVel) =>
          diffIrr.unit shouldBe WattsPerSquareMeter
          (diffIrr ~= WattsPerSquareMeter(72.7656)) shouldBe true

          dirIrr.unit shouldBe WattsPerSquareMeter
          (dirIrr ~= WattsPerSquareMeter(80.1172)) shouldBe true

          temp.unit shouldBe Celsius
          (temp ~= Celsius(6.459d)) shouldBe true

          windVel.unit shouldBe MetersPerSecond
          (windVel ~= MetersPerSecond(11.11602d)) shouldBe true

      }
    }
  }
}
