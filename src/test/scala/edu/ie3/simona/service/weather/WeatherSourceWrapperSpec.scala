/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.weather

import edu.ie3.datamodel.io.factory.timeseries.IconTimeBasedWeatherValueFactory
import edu.ie3.datamodel.io.source.{
  IdCoordinateSource,
  WeatherSource => PsdmWeatherSource,
}
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.timeseries.individual.{
  IndividualTimeSeries,
  TimeBasedValue,
}
import edu.ie3.datamodel.models.value.WeatherValue
import edu.ie3.simona.ontology.messages.services.WeatherMessage.WeatherData
import edu.ie3.simona.service.weather.WeatherSource.{
  EMPTY_WEATHER_DATA,
  WeightedCoordinates,
}
import edu.ie3.simona.service.weather.WeatherSourceSpec.DummyIdCoordinateSource
import edu.ie3.simona.service.weather.WeatherSourceWrapper.WeightSum
import edu.ie3.simona.service.weather.WeatherSourceWrapperSpec._
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.geo.GeoUtils
import edu.ie3.util.interval.ClosedInterval
import edu.ie3.util.scala.quantities.{Irradiance, WattsPerSquareMeter}
import org.locationtech.jts.geom.Point
import tech.units.indriya.ComparableQuantity
import squants.{Temperature, Velocity}
import squants.motion.MetersPerSecond
import squants.thermal.{Celsius, Kelvin}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.time.{ZoneId, ZonedDateTime}
import java.util
import java.util.{Optional, UUID}
import javax.measure.quantity.Length
import scala.jdk.CollectionConverters._

class WeatherSourceWrapperSpec extends UnitSpec {

  implicit val toleranceIrradiance: Irradiance = WattsPerSquareMeter(0.1d)
  implicit val toleranceVelocity: Velocity = MetersPerSecond(0.01d)
  implicit val tolerance: Temperature = Celsius(0.01d)

  "A weather source wrapper" should {
    val actor = classOf[WeatherSourceWrapper].getDeclaredConstructor(
      classOf[PsdmWeatherSource],
      classOf[IdCoordinateSource],
      classOf[Long],
      classOf[ComparableQuantity[Length]],
      classOf[ZonedDateTime],
    )
    actor.setAccessible(true)

    val date = ZonedDateTime.of(2021, 1, 15, 18, 0, 0, 0, ZoneId.of("UTC"))

    val source = actor.newInstance(
      WeatherSourceWrapperSpec.DummyPsdmWeatherSource,
      DummyIdCoordinateSource,
      360L,
      Quantities.getQuantity(10000, Units.METRE),
      date,
    )

    "calculate the correct weighted value for 4 coordinates with 0.25 weight each" in {
      val weightedCoordinates = WeightedCoordinates(
        Map(
          coordinate1a -> 0.25,
          coordinate1b -> 0.25,
          coordinate1c -> 0.25,
          coordinate13 -> 0.25,
        )
      )
      val result = source.getWeather(date.toEpochSecond, weightedCoordinates)
      val sumOfAll = 1 + 1 + 1 + 13
      result.dirIrr should approximate(WattsPerSquareMeter(sumOfAll / 4))
      result.diffIrr should approximate(WattsPerSquareMeter(sumOfAll / 4))
      result.temp should approximate(Celsius(sumOfAll / 4))
      result.windVel should approximate(MetersPerSecond(sumOfAll / 4))

    }

    "Calculate the correct weighted value for 4 coordinates with 0.25 weight each, where a singular parameter value is missing" in {
      val weightedCoordinates = WeightedCoordinates(
        Map(
          coordinate1a -> 0.25,
          coordinate1b -> 0.25,
          coordinate1c -> 0.25,
          coordinate13NoTemp -> 0.25,
        )
      )
      val result = source.getWeather(date.toEpochSecond, weightedCoordinates)
      val sumOfAll = 1 + 1 + 1 + 13
      result.dirIrr should approximate(WattsPerSquareMeter(sumOfAll / 4))
      result.diffIrr should approximate(WattsPerSquareMeter(sumOfAll / 4))
      result.temp should approximate(Celsius((1 + 1 + 1) / 3))
      result.windVel should approximate(MetersPerSecond(sumOfAll / 4))
    }

    "Calculate the correct weighted value for 4 coordinates with 0.25 weight each, where one is empty" in {
      val weightedCoordinates = WeightedCoordinates(
        Map(
          coordinate1a -> 0.25,
          coordinate1b -> 0.25,
          coordinate1c -> 0.25,
          coordinateEmpty -> 0.25,
        )
      )
      val result = source.getWeather(date.toEpochSecond, weightedCoordinates)
      val sumOfAll = 1 + 1 + 1
      result.dirIrr should approximate(WattsPerSquareMeter(sumOfAll / 3))
      result.diffIrr should approximate(WattsPerSquareMeter(sumOfAll / 3))
      result.temp should approximate(Celsius(sumOfAll / 3))
      result.windVel should approximate(MetersPerSecond(sumOfAll / 3))

    }

    "calculate the correct weighted value for 1 coordinate with a weight of 1" in {
      val weightedCoordinates = WeightedCoordinates(Map(coordinate13 -> 1d))
      val result = source.getWeather(date.toEpochSecond, weightedCoordinates)
      result.dirIrr should approximate(WattsPerSquareMeter(13d))
      result.diffIrr should approximate(WattsPerSquareMeter(13d))
      result.temp should approximate(Celsius(13d))
      result.windVel should approximate(MetersPerSecond(13d))
    }

    "return temperature quantity on absolute scale" in {
      val weightedCoordinates = WeightedCoordinates(Map(coordinate1a -> 1))
      val result = source.getWeather(date.toEpochSecond, weightedCoordinates)
      result.temp.unit shouldBe Kelvin
    }

    "return the correct data ticks" in {
      val testData = Table(
        ("start", "end", "expected"),
        (0L, 10800L, (0L to 10800L by 3600L).toArray),
        (1L, 10800L, (3600L to 10800L by 3600L).toArray),
        (0L, 10799L, (0L to 7200L by 3600L).toArray),
        (1L, 10799L, (3600L to 7200L by 3600L).toArray),
      )

      testData.forEvery {
        case (start: Long, end: Long, expected: Array[Long]) =>
          source.getDataTicks(start, end) shouldBe expected
      }
    }
  }

  "Handling the weighted weather" when {
    "adding to the weight sum" should {
      "produce correct results" in {
        val weightSum = WeightSum(0.1d, 0.2d, 0.3d, 0.4d)
        val weightSumAdded = weightSum.add(0.2d, 0.3d, 0.4d, 0.5d)

        weightSumAdded.diffIrr should ===(0.3d +- 1e-10)
        weightSumAdded.dirIrr should ===(0.5d +- 1e-10)
        weightSumAdded.temp should ===(0.7d +- 1e-10)
        weightSumAdded.windVel should ===(0.9d +- 1e-10)
      }
    }

    "scaling the weighted attributes with the sum of weights" should {
      "calculate proper information on proper input" in {
        val weatherSeq = Seq(
          (0.5, 0.75, 291d, 10d),
          (12.3, 1.2, 293d, 12d),
          (25.0, 5.7, 290d, 9d),
          (26.3, 1.7, 289d, 11d),
        )
        val weights = Seq(
          (0.1, 0.2, 0.3, 0.4),
          (0.25, 0.2, 0.25, 0.1),
          (0.3, 0.4, 0.15, 0.05),
          (0.35, 0.2, 0.3, 0.45),
        )

        val (weightedWeather, weightSum) =
          prepareWeightTestData(weatherSeq, weights)

        weightSum.scale(weightedWeather) match {
          case WeatherData(diffIrr, dirIrr, temp, windVel) =>
            diffIrr should approximate(WattsPerSquareMeter(19.83))
            dirIrr should approximate(WattsPerSquareMeter(3.01))
            temp should approximate(Kelvin(290.75))
            windVel should approximate(MetersPerSecond(10.6))
        }
      }
    }

    "calculate proper input, if data is missing in one coordinate" in {
      val weatherSeq = Seq(
        (0.5, 0.75, 291d, 10d),
        (12.3, 1.2, 293d, 12d),
        (25.0, 5.7, 290d, 9d),
        (26.3, 1.7, 289d, 11d),
      )
      val weights = Seq(
        (0.1, 0.2, 0d, 0.4),
        (0.25, 0.2, 0d, 0.1),
        (0.3, 0.4, 0d, 0.05),
        (0.35, 0.2, 0d, 0.45),
      )

      val (weightedWeather, weightSum) =
        prepareWeightTestData(weatherSeq, weights)

      weightSum.scale(weightedWeather) match {
        case WeatherData(_, _, temp, _) =>
          temp shouldBe EMPTY_WEATHER_DATA.temp
      }
    }

    "return empty value for an attribute, if weight sum is zero" in {
      val weatherSeq = Seq(
        (0.5, 0.75, 291d, 10d),
        (12.3, 1.2, 0d, 12d),
        (25.0, 5.7, 290d, 9d),
        (26.3, 1.7, 289d, 11d),
      )
      val weights = Seq(
        (0.1, 0.2, 0.3, 0.4),
        (0.25, 0.2, 0d, 0.1),
        (0.3, 0.4, 0.15, 0.05),
        (0.35, 0.2, 0.3, 0.45),
      )

      val (weightedWeather, weightSum) =
        prepareWeightTestData(weatherSeq, weights)

      weightSum.scale(weightedWeather) match {
        case WeatherData(_, _, temp, _) =>
          temp should approximate(Kelvin(290d))
      }
    }

    "correctly calculate scaled properties if provided with varying weight components" in {
      val weatherData = WeatherData(
        WattsPerSquareMeter(1.0),
        WattsPerSquareMeter(1.0),
        Kelvin(1.0d),
        MetersPerSecond(1.0d),
      )
      val weightSum = WeightSum(0.25, 0.5, 0.8, 1.0)

      weightSum.scale(weatherData) match {
        case WeatherData(diffIrr, dirIrr, temp, windVel) =>
          diffIrr should approximate(WattsPerSquareMeter(4.0))
          dirIrr should approximate(WattsPerSquareMeter(2.0))
          temp should approximate(Kelvin(1.25d))
          windVel should approximate(MetersPerSecond(1.0d))
      }
    }
  }
}

object WeatherSourceWrapperSpec {
  // lat/lon are irrelevant, we will manually create weights later on
  private val coordinate1a = GeoUtils.buildPoint(51, 6)
  private val coordinate1b = GeoUtils.buildPoint(51, 7)
  private val coordinate1c = GeoUtils.buildPoint(51, 8)
  private val coordinate1d = GeoUtils.buildPoint(51, 9)
  private val coordinate13 = GeoUtils.buildPoint(51, 10)
  private val coordinate13NoTemp = GeoUtils.buildPoint(52, 10)
  private val coordinateEmpty = GeoUtils.buildPoint(53, 10)

  case object DummyPsdmWeatherSource
      extends PsdmWeatherSource(
        DummyIdCoordinateSource,
        new IconTimeBasedWeatherValueFactory(),
      ) {

    override def getTimeKeysAfter(
        zonedDateTime: ZonedDateTime
    ): util.Map[Point, util.List[ZonedDateTime]] = {
      val startTime =
        ZonedDateTime.of(2021, 1, 15, 18, 0, 0, 0, ZoneId.of("UTC"))

      val time = Range
        .inclusive(0, 3, 1)
        .map(startTime.plusHours(_))
        .filter(_.isAfter(zonedDateTime))
        .asJava

      Map(coordinate1a -> time).asJava
    }

    private val dummyValues = Map(
      coordinate1a -> new WeatherValue(
        coordinate1a,
        Quantities.getQuantity(1d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(1d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(1d, StandardUnits.TEMPERATURE),
        Quantities.getQuantity(1d, StandardUnits.WIND_DIRECTION),
        Quantities.getQuantity(1d, StandardUnits.WIND_VELOCITY),
      ),
      coordinate1b -> new WeatherValue(
        coordinate1b,
        Quantities.getQuantity(1d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(1d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(1d, StandardUnits.TEMPERATURE),
        Quantities.getQuantity(1d, StandardUnits.WIND_DIRECTION),
        Quantities.getQuantity(1d, StandardUnits.WIND_VELOCITY),
      ),
      coordinate1c -> new WeatherValue(
        coordinate1c,
        Quantities.getQuantity(1d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(1d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(1d, StandardUnits.TEMPERATURE),
        Quantities.getQuantity(1d, StandardUnits.WIND_DIRECTION),
        Quantities.getQuantity(1d, StandardUnits.WIND_VELOCITY),
      ),
      coordinate1d -> new WeatherValue(
        coordinate1d,
        Quantities.getQuantity(1d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(1d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(1d, StandardUnits.TEMPERATURE),
        Quantities.getQuantity(1d, StandardUnits.WIND_DIRECTION),
        Quantities.getQuantity(1d, StandardUnits.WIND_VELOCITY),
      ),
      coordinate13 -> new WeatherValue(
        coordinate13,
        Quantities.getQuantity(13d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(13d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(13d, StandardUnits.TEMPERATURE),
        Quantities.getQuantity(13d, StandardUnits.WIND_DIRECTION),
        Quantities.getQuantity(13d, StandardUnits.WIND_VELOCITY),
      ),
      coordinate13NoTemp -> new WeatherValue(
        coordinate13NoTemp,
        Quantities.getQuantity(13d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(13d, StandardUnits.SOLAR_IRRADIANCE),
        null,
        Quantities.getQuantity(13d, StandardUnits.WIND_DIRECTION),
        Quantities.getQuantity(13d, StandardUnits.WIND_VELOCITY),
      ),
      coordinateEmpty -> new WeatherValue(
        coordinateEmpty,
        null,
        null,
        null,
        null,
        null,
      ),
    )

    override def getSourceFields: Optional[util.Set[String]] =
      // only required for validation
      Optional.empty()

    override def getWeather(
        timeInterval: ClosedInterval[ZonedDateTime]
    ): util.Map[Point, IndividualTimeSeries[WeatherValue]] = {
      val ticks = LazyList
        .iterate(timeInterval.getLower)(_.plusHours(1))
        .takeWhile(_.isBefore(timeInterval.getUpper.plusHours(1)))
        .toList
      dummyValues.map { case (point, data) =>
        (
          point,
          new IndividualTimeSeries[WeatherValue](
            UUID.randomUUID(),
            ticks.map(tick => new TimeBasedValue(tick, data)).toSet.asJava,
          ),
        )
      }.asJava
    }

    override def getWeather(
        timeInterval: ClosedInterval[ZonedDateTime],
        coordinates: util.Collection[Point],
    ): util.Map[Point, IndividualTimeSeries[WeatherValue]] = {
      val ticks = LazyList
        .iterate(timeInterval.getLower)(_.plusHours(1))
        .takeWhile(_.isBefore(timeInterval.getUpper.plusHours(1)))
        .toList
      dummyValues
        .filter { case (point, _) => coordinates.contains(point) }
        .map { case (point, data) =>
          (
            point,
            new IndividualTimeSeries[WeatherValue](
              UUID.randomUUID(),
              ticks.map(tick => new TimeBasedValue(tick, data)).toSet.asJava,
            ),
          )
        }
        .asJava
    }

    override def getWeather(
        date: ZonedDateTime,
        coordinate: Point,
    ): Optional[TimeBasedValue[WeatherValue]] = {
      dummyValues.get(coordinate) match {
        case Some(value) => Optional.of(new TimeBasedValue(date, value))
        case None        => Optional.empty()
      }
    }
  }

  /** Prepare test data for WeightSum-related tests
    *
    * @param weatherSeq
    *   sequence of raw weather data
    * @param weights
    *   the weights to use for averaging the weather data, with rows equivalent
    *   to the rows in weatherSeq
    * @return
    *   A tuple of 1. the weighted average weather data and 2. the weight sum
    */
  private def prepareWeightTestData(
      weatherSeq: Seq[(Double, Double, Double, Double)],
      weights: Seq[(Double, Double, Double, Double)],
  ): (WeatherData, WeightSum) = {
    val weatherData = weatherSeq.map { case (diff, dir, temp, wVel) =>
      WeatherData(
        WattsPerSquareMeter(diff),
        WattsPerSquareMeter(dir),
        Kelvin(temp),
        MetersPerSecond(wVel),
      )
    }

    val weightedWeather =
      weatherData.zip(weights).foldLeft(EMPTY_WEATHER_DATA) {
        case (
              currentSum,
              (
                WeatherData(diffIrr, dirIrr, temp, windVel),
                (diffWeight, dirWeight, tempWeight, wVelWeight),
              ),
            ) =>
          currentSum.copy(
            diffIrr = currentSum.diffIrr + (diffIrr * diffWeight),
            dirIrr = currentSum.dirIrr + (dirIrr * dirWeight),
            temp = currentSum.temp + temp * tempWeight,
            windVel = currentSum.windVel + windVel * wVelWeight,
          )
      }
    val weightSum = weights.foldLeft(WeightSum.EMPTY_WEIGHT_SUM) {
      case (currentSum, currentWeight) =>
        currentSum.add(
          currentWeight._1,
          currentWeight._2,
          currentWeight._3,
          currentWeight._4,
        )
    }

    (weightedWeather, weightSum)
  }

}
