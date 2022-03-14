/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.weather

import edu.ie3.datamodel.io.source.{
  IdCoordinateSource,
  WeatherSource => PsdmWeatherSource
}
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.timeseries.individual.{
  IndividualTimeSeries,
  TimeBasedValue
}
import edu.ie3.datamodel.models.value.WeatherValue
import edu.ie3.simona.ontology.messages.services.WeatherMessage.WeatherData
import edu.ie3.simona.service.weather.WeatherSource.{
  EMPTY_WEATHER_DATA,
  WeightedCoordinates
}
import edu.ie3.simona.service.weather.WeatherSourceSpec.DummyIdCoordinateSource
import edu.ie3.simona.service.weather.WeatherSourceWrapper.WeightSum
import edu.ie3.simona.service.weather.WeatherSourceWrapperSpec._
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.geo.GeoUtils
import edu.ie3.util.interval.ClosedInterval
import org.locationtech.jts.geom.Point
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.time.{ZoneId, ZonedDateTime}
import java.util
import java.util.{Optional, UUID}
import javax.measure.Quantity.Scale
import scala.jdk.CollectionConverters.{MapHasAsJava, SetHasAsJava}

class WeatherSourceWrapperSpec extends UnitSpec {

  "A weather source wrapper" should {
    val ctor = classOf[WeatherSourceWrapper].getDeclaredConstructor(
      classOf[PsdmWeatherSource],
      classOf[IdCoordinateSource],
      classOf[Long],
      classOf[ZonedDateTime]
    )
    ctor.setAccessible(true)
    val source = ctor.newInstance(
      WeatherSourceWrapperSpec.DummyPsdmWeatherSource,
      DummyIdCoordinateSource,
      360L,
      ZonedDateTime.now()
    )
    val date = ZonedDateTime.of(2021, 1, 15, 18, 0, 0, 0, ZoneId.of("UTC"))

    "calculate the correct weighted value for 4 coordinates with 0.25 weight each" in {
      val weightedCoordinates = WeightedCoordinates(
        Map(
          coordinate1a -> 0.25,
          coordinate1b -> 0.25,
          coordinate1c -> 0.25,
          coordinate13 -> 0.25
        )
      )
      val result = source.getWeather(date.toEpochSecond, weightedCoordinates)
      val sumOfAll = 1 + 1 + 1 + 13
      result.dirRad should equalWithTolerance(
        Quantities.getQuantity(sumOfAll / 4, StandardUnits.SOLAR_IRRADIANCE)
      )
      result.diffRad should equalWithTolerance(
        Quantities.getQuantity(sumOfAll / 4, StandardUnits.SOLAR_IRRADIANCE)
      )
      result.temp should equalWithTolerance(
        Quantities.getQuantity(sumOfAll / 4, StandardUnits.TEMPERATURE)
      )
      result.windVel should equalWithTolerance(
        Quantities.getQuantity(sumOfAll / 4, StandardUnits.WIND_VELOCITY)
      )
    }

    "Calculate the correct weighted value for 4 coordinates with 0.25 weight each, where a singular parameter value is missing" in {
      val weightedCoordinates = WeightedCoordinates(
        Map(
          coordinate1a -> 0.25,
          coordinate1b -> 0.25,
          coordinate1c -> 0.25,
          coordinate13NoTemp -> 0.25
        )
      )
      val result = source.getWeather(date.toEpochSecond, weightedCoordinates)
      val sumOfAll = 1 + 1 + 1 + 13
      result.dirRad should equalWithTolerance(
        Quantities.getQuantity(sumOfAll / 4, StandardUnits.SOLAR_IRRADIANCE)
      )
      result.diffRad should equalWithTolerance(
        Quantities.getQuantity(sumOfAll / 4, StandardUnits.SOLAR_IRRADIANCE)
      )
      result.temp should equalWithTolerance(
        Quantities.getQuantity((1 + 1 + 1) / 3, StandardUnits.TEMPERATURE)
      )
      result.windVel should equalWithTolerance(
        Quantities.getQuantity(sumOfAll / 4, StandardUnits.WIND_VELOCITY)
      )
    }

    "Calculate the correct weighted value for 4 coordinates with 0.25 weight each, where one is empty" in {
      val weightedCoordinates = WeightedCoordinates(
        Map(
          coordinate1a -> 0.25,
          coordinate1b -> 0.25,
          coordinate1c -> 0.25,
          coordinateEmpty -> 0.25
        )
      )
      val result = source.getWeather(date.toEpochSecond, weightedCoordinates)
      val sumOfAll = 1 + 1 + 1
      result.dirRad should equalWithTolerance(
        Quantities.getQuantity(sumOfAll / 3, StandardUnits.SOLAR_IRRADIANCE)
      )
      result.diffRad should equalWithTolerance(
        Quantities.getQuantity(sumOfAll / 3, StandardUnits.SOLAR_IRRADIANCE)
      )
      result.temp should equalWithTolerance(
        Quantities.getQuantity(sumOfAll / 3, StandardUnits.TEMPERATURE)
      )
      result.windVel should equalWithTolerance(
        Quantities.getQuantity(sumOfAll / 3, StandardUnits.WIND_VELOCITY)
      )
    }

    "calculate the correct weighted value for 1 coordinate with a weight of 1" in {
      val weightedCoordinates = WeightedCoordinates(Map(coordinate13 -> 1d))
      val result = source.getWeather(date.toEpochSecond, weightedCoordinates)
      result.dirRad should equalWithTolerance(
        Quantities.getQuantity(13, StandardUnits.SOLAR_IRRADIANCE)
      )
      result.diffRad should equalWithTolerance(
        Quantities.getQuantity(13, StandardUnits.SOLAR_IRRADIANCE)
      )
      result.temp should equalWithTolerance(
        Quantities.getQuantity(13, StandardUnits.TEMPERATURE)
      )
      result.windVel should equalWithTolerance(
        Quantities.getQuantity(13, StandardUnits.WIND_VELOCITY)
      )
    }

    "return temperature quantity on absolute scale" in {
      val weightedCoordinates = WeightedCoordinates(Map(coordinate1a -> 1))
      val result = source.getWeather(date.toEpochSecond, weightedCoordinates)
      result.temp.getScale shouldBe Scale.ABSOLUTE
    }
  }

  "Handling the weighted weather" when {
    "scaling the weighted attributes with the sum of weights" should {
      "calculate proper information on proper input" in {
        val weatherSeq = Seq(
          (0.5, 0.75, 291d, 10d),
          (12.3, 1.2, 293d, 12d),
          (25.0, 5.7, 290d, 9d),
          (26.3, 1.7, 289d, 11d)
        )
        val weights = Seq(
          (0.1, 0.2, 0.3, 0.4),
          (0.25, 0.2, 0.25, 0.1),
          (0.3, 0.4, 0.15, 0.05),
          (0.35, 0.2, 0.3, 0.45)
        )

        val (_, weightedWeather, weightSum) =
          prepareWeightTestData(weatherSeq, weights)

        weightSum.scale(weightedWeather) match {
          case WeatherData(diffRad, dirRad, temp, windVel) =>
            diffRad should equalWithTolerance(
              Quantities.getQuantity(19.83, StandardUnits.SOLAR_IRRADIANCE),
              1e-6
            )
            dirRad should equalWithTolerance(
              Quantities.getQuantity(3.01, StandardUnits.SOLAR_IRRADIANCE),
              1e-6
            )
            temp should equalWithTolerance(
              Quantities
                .getQuantity(290.75, Units.KELVIN)
                .to(StandardUnits.TEMPERATURE),
              1e-6
            )
            windVel should equalWithTolerance(
              Quantities.getQuantity(10.6, StandardUnits.WIND_VELOCITY),
              1e-6
            )
        }
      }
    }

    "calculate proper input, if data is missing in one coordinate" in {
      val weatherSeq = Seq(
        (0.5, 0.75, 291d, 10d),
        (12.3, 1.2, 293d, 12d),
        (25.0, 5.7, 290d, 9d),
        (26.3, 1.7, 289d, 11d)
      )
      val weights = Seq(
        (0.1, 0.2, 0d, 0.4),
        (0.25, 0.2, 0d, 0.1),
        (0.3, 0.4, 0d, 0.05),
        (0.35, 0.2, 0d, 0.45)
      )

      val (_, weightedWeather, weightSum) =
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
        (26.3, 1.7, 289d, 11d)
      )
      val weights = Seq(
        (0.1, 0.2, 0.3, 0.4),
        (0.25, 0.2, 0d, 0.1),
        (0.3, 0.4, 0.15, 0.05),
        (0.35, 0.2, 0.3, 0.45)
      )

      val (_, weightedWeather, weightSum) =
        prepareWeightTestData(weatherSeq, weights)

      weightSum.scale(weightedWeather) match {
        case WeatherData(_, _, temp, _) =>
          temp should equalWithTolerance(
            Quantities
              .getQuantity(290d, Units.KELVIN)
              .to(StandardUnits.TEMPERATURE)
          )
      }
    }
  }
}

case object WeatherSourceWrapperSpec {
  // lat/lon are irrelevant, we will manually create weights later on
  private val coordinate1a = GeoUtils.xyToPoint(6, 51)
  private val coordinate1b = GeoUtils.xyToPoint(7, 51)
  private val coordinate1c = GeoUtils.xyToPoint(8, 51)
  private val coordinate1d = GeoUtils.xyToPoint(9, 51)
  private val coordinate13 = GeoUtils.xyToPoint(10, 51)
  private val coordinate13NoTemp = GeoUtils.xyToPoint(10, 52)
  private val coordinateEmpty = GeoUtils.xyToPoint(10, 53)

  case object DummyPsdmWeatherSource extends PsdmWeatherSource {

    private val dummyValues = Map(
      coordinate1a -> new WeatherValue(
        coordinate1a,
        Quantities.getQuantity(1d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(1d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(1d, StandardUnits.TEMPERATURE),
        Quantities.getQuantity(1d, StandardUnits.WIND_DIRECTION),
        Quantities.getQuantity(1d, StandardUnits.WIND_VELOCITY)
      ),
      coordinate1b -> new WeatherValue(
        coordinate1b,
        Quantities.getQuantity(1d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(1d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(1d, StandardUnits.TEMPERATURE),
        Quantities.getQuantity(1d, StandardUnits.WIND_DIRECTION),
        Quantities.getQuantity(1d, StandardUnits.WIND_VELOCITY)
      ),
      coordinate1c -> new WeatherValue(
        coordinate1c,
        Quantities.getQuantity(1d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(1d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(1d, StandardUnits.TEMPERATURE),
        Quantities.getQuantity(1d, StandardUnits.WIND_DIRECTION),
        Quantities.getQuantity(1d, StandardUnits.WIND_VELOCITY)
      ),
      coordinate1d -> new WeatherValue(
        coordinate1d,
        Quantities.getQuantity(1d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(1d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(1d, StandardUnits.TEMPERATURE),
        Quantities.getQuantity(1d, StandardUnits.WIND_DIRECTION),
        Quantities.getQuantity(1d, StandardUnits.WIND_VELOCITY)
      ),
      coordinate13 -> new WeatherValue(
        coordinate13,
        Quantities.getQuantity(13d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(13d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(13d, StandardUnits.TEMPERATURE),
        Quantities.getQuantity(13d, StandardUnits.WIND_DIRECTION),
        Quantities.getQuantity(13d, StandardUnits.WIND_VELOCITY)
      ),
      coordinate13NoTemp -> new WeatherValue(
        coordinate13NoTemp,
        Quantities.getQuantity(13d, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(13d, StandardUnits.SOLAR_IRRADIANCE),
        null,
        Quantities.getQuantity(13d, StandardUnits.WIND_DIRECTION),
        Quantities.getQuantity(13d, StandardUnits.WIND_VELOCITY)
      ),
      coordinateEmpty -> new WeatherValue(
        coordinateEmpty,
        null,
        null,
        null,
        null,
        null
      )
    )

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
            ticks.map(tick => new TimeBasedValue(tick, data)).toSet.asJava
          )
        )
      }.asJava
    }

    override def getWeather(
        timeInterval: ClosedInterval[ZonedDateTime],
        coordinates: util.Collection[Point]
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
              ticks.map(tick => new TimeBasedValue(tick, data)).toSet.asJava
            )
          )
        }
        .asJava
    }

    override def getWeather(
        date: ZonedDateTime,
        coordinate: Point
    ): Optional[TimeBasedValue[WeatherValue]] = {
      dummyValues.get(coordinate) match {
        case Some(value) => Optional.of(new TimeBasedValue(date, value))
        case None        => Optional.empty()
      }
    }
  }

  def prepareWeightTestData(
      weatherSeq: Seq[(Double, Double, Double, Double)],
      weights: Seq[(Double, Double, Double, Double)]
  ): (Seq[WeatherData], WeatherData, WeightSum) = {
    val weatherData = weatherSeq.map { case (diff, dir, temp, wVel) =>
      WeatherData(
        Quantities.getQuantity(diff, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(dir, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(temp, Units.KELVIN),
        Quantities.getQuantity(wVel, StandardUnits.WIND_VELOCITY)
      )
    }

    val weightedWeather =
      weatherData.zip(weights).foldLeft(EMPTY_WEATHER_DATA) {
        case (
              currentSum,
              (
                WeatherData(diffRad, dirRad, temp, windVel),
                (diffWeight, dirWeight, tempWeight, wVelWeight)
              )
            ) =>
          currentSum.copy(
            diffRad = currentSum.diffRad.add(diffRad.multiply(diffWeight)),
            dirRad = currentSum.dirRad.add(dirRad.multiply(dirWeight)),
            temp = currentSum.temp.add(temp.multiply(tempWeight)),
            windVel = currentSum.windVel.add(windVel.multiply(wVelWeight))
          )
      }
    val weightSum = weights.foldLeft(WeightSum.EMPTY_WEIGHT_SUM) {
      case (currentSum, currentWeight) =>
        currentSum.add(
          currentWeight._1,
          currentWeight._2,
          currentWeight._3,
          currentWeight._1
        )
    }

    (weatherData, weightedWeather, weightSum)
  }

}
