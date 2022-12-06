/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.weather

import edu.ie3.datamodel.io.source.IdCoordinateSource
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
import edu.ie3.simona.exceptions.ServiceException
import edu.ie3.simona.ontology.messages.services.WeatherMessage
import edu.ie3.simona.ontology.messages.services.WeatherMessage.{
  WeatherData,
  WeatherDataOption
}
import edu.ie3.simona.service.weather.WeatherSource.{
  AgentCoordinates,
  EMPTY_WEATHER_DATA,
  WeightedCoordinates,
  getNextValue,
  getPreviousValue,
  getWeatherData,
  updateWeatherDataOption
}
import edu.ie3.simona.service.weather.WeatherSourceSpec._
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.geo.{CoordinateDistance, GeoUtils}
import edu.ie3.util.quantities.{PowerSystemUnits, QuantityUtil}
import org.locationtech.jts.geom.Point
import org.scalatest.prop.TableDrivenPropertyChecks
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.time.ZonedDateTime
import java.util
import java.util.{Optional, UUID}
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.util.{Failure, Success}

class WeatherSourceSpec extends UnitSpec with TableDrivenPropertyChecks {
  private val coordinate0 = GeoUtils.buildPoint(51.47, 7.41)

  "A weather source" should {
    "issue a ServiceException, if there are not enough coordinates available" in {
      DummyWeatherSource.getNearestCoordinatesWithDistances(
        AgentCoordinates(coordinate0.getY, coordinate0.getX),
        9,
        Quantities.getQuantity(28, PowerSystemUnits.KILOMETRE)
      ) match {
        case Failure(exception: ServiceException) =>
          exception.getMessage shouldBe "There are not enough coordinates for averaging. Found 8 but need 9."
        case _ => fail("You shall not pass!")
      }
    }
    "issue a ServiceException, if there are not enough coordinates in max distance available" in {
      DummyWeatherSource.getNearestCoordinatesWithDistances(
        AgentCoordinates(coordinate0.getY, coordinate0.getX),
        5,
        Quantities.getQuantity(5, PowerSystemUnits.KILOMETRE)
      ) match {
        case Failure(exception: ServiceException) =>
          exception.getMessage shouldBe "There are not enough coordinates within the max coordinate distance of 5 km. Found 4 but need 5."
        case _ => fail("You shall not pass!")
      }
    }

    "issue a ServiceException, if the queried coordinate is not surrounded by the found weather coordinates" in {
      val agentCoordinates = AgentCoordinates(51.3, 7.3)
      DummyWeatherSource.getNearestCoordinatesWithDistances(
        agentCoordinates,
        4,
        Quantities.getQuantity(28, PowerSystemUnits.KILOMETRE)
      ) match {
        case Failure(exception: ServiceException) =>
          exception.getMessage shouldBe "The queried point shall be surrounded by 4 weather coordinates, which are in each quadrant. This is not the case."
        case _ => fail("You shall not pass!")
      }
    }

    "return one coordinate, if we found an exact hit" in {
      val agentCoordinates = AgentCoordinates(51.4380006, 7.4380005)
      val distance = GeoUtils.calcHaversine(
        agentCoordinates.latitude,
        agentCoordinates.longitude,
        coordinate551525.getY,
        coordinate551525.getX
      )

      DummyWeatherSource.getNearestCoordinatesWithDistances(
        agentCoordinates,
        4,
        Quantities.getQuantity(28, PowerSystemUnits.KILOMETRE)
      ) match {
        case Success(coordinateDistances) =>
          coordinateDistances.size shouldBe 1
          coordinateDistances.headOption match {
            case Some(coordinateDistance) =>
              coordinateDistance.getCoordinateA shouldBe agentCoordinates.toPoint
              coordinateDistance.getCoordinateB shouldBe coordinate551525
              QuantityUtil.isEquivalentAbs(
                coordinateDistance.getDistance,
                distance
              ) shouldBe true
            case None => fail("Somebody stole the first result >:-(")
          }
        case Failure(exception) =>
          fail(
            "Determining the nearest coordinates was meant to succeed.",
            exception
          )
      }
    }

    "determine the nearest 4 coordinates" in {
      val agentCoordinates =
        AgentCoordinates(coordinate0.getY, coordinate0.getX)
      val expectedCoordinateDistances = Vector(
        new CoordinateDistance(
          coordinate0,
          coordinate67775
        ),
        new CoordinateDistance(
          coordinate0,
          coordinate551525
        ),
        new CoordinateDistance(
          coordinate0,
          coordinate531137
        ),
        new CoordinateDistance(
          coordinate0,
          coordinate278150
        )
      )

      DummyWeatherSource.getNearestCoordinatesWithDistances(
        agentCoordinates,
        4,
        Quantities.getQuantity(28, PowerSystemUnits.KILOMETRE)
      ) match {
        case Success(coordinateDistances) =>
          coordinateDistances.size shouldBe 4
          coordinateDistances.corresponds(expectedCoordinateDistances) {
            case (a: CoordinateDistance, b: CoordinateDistance) =>
              a.getCoordinateA.equalsExact(b.getCoordinateA, 1e-6) &&
              a.getCoordinateB.equalsExact(b.getCoordinateB, 1e-6) &&
              QuantityUtil.isEquivalentAbs(a.getDistance, b.getDistance, 1e-4)
          } shouldBe true
        case Failure(exception) =>
          fail(
            "Determining the nearest coordinates was meant to succeed.",
            exception
          )
      }
    }

    "determine coordinate weights correctly, if there is only one coordinate" in {
      val coordinates = Vector(
        new CoordinateDistance(
          coordinate0,
          coordinate67775
        )
      )

      DummyWeatherSource.determineWeights(coordinates) match {
        case Success(weightedCoordinates) =>
          weightedCoordinates.weighting.size shouldBe 1
          weightedCoordinates.weighting.contains(coordinate67775) shouldBe true
          weightedCoordinates.weighting.get(coordinate67775) shouldBe Some(1d)
        case Failure(exception) =>
          fail(
            "Determining the weight of coordinates was meant to succeed.",
            exception
          )
      }
    }

    "refuse to determine weights for coordinates, if the sum of distances is zero" in {
      val coordinates = Vector(
        new CoordinateDistance(
          coordinate0,
          coordinate0
        ),
        new CoordinateDistance(
          coordinate0,
          coordinate0
        )
      )

      DummyWeatherSource.determineWeights(coordinates) match {
        case Failure(exception) =>
          exception match {
            case ServiceException(msg) =>
              msg shouldBe "The total sum of distances to surrounding coordinates is 0 m" +
                " or less. Therefore averaging would lead to numeric errors."
            case _ => fail("Got wrong exception")
          }
        case Success(_) => fail("You shall not pass!")
      }
    }

    "determine weights correctly" in {
      val coordinates = Vector(
        new CoordinateDistance(
          coordinate0,
          coordinate67775
        ),
        new CoordinateDistance(
          coordinate0,
          coordinate531137
        ),
        new CoordinateDistance(
          coordinate0,
          coordinate551525
        ),
        new CoordinateDistance(
          coordinate0,
          coordinate278150
        )
      )
      val expectedWeights = Map(
        coordinate67775 -> 0.254626046882988,
        coordinate531137 -> 0.249222038996929,
        coordinate551525 -> 0.250659514620527,
        coordinate278150 -> 0.245492399499556
      )

      DummyWeatherSource.determineWeights(coordinates) match {
        case Success(WeightedCoordinates(weighting)) =>
          weighting.corresponds(expectedWeights) {
            case ((pointA, weightA), (pointB, weightB)) =>
              pointA == pointB && Math.abs(weightA - weightB) < 1e-6
          } shouldBe true

        case Failure(exception) =>
          fail(
            "Determining the weight of coordinates was meant to succeed.",
            exception
          )
      }
    }

    "refuse to return the nearest weighted coordinates on an arbitrary error in underlying methods" in {
      /* Query more coordinates, than are apparent */
      DummyWeatherSource.getWeightedCoordinates(
        AgentCoordinates(coordinate0.getY, coordinate0.getX),
        9,
        Quantities.getQuantity(28, PowerSystemUnits.KILOMETRE)
      ) match {
        case Failure(exception: ServiceException) =>
          exception.getMessage shouldBe "Determination of coordinate weights failed."
          exception.getCause shouldBe ServiceException(
            "There are not enough coordinates for averaging. Found 8 but need 9."
          )
        case _ => fail("You shall not pass!")
      }
    }

    "return one coordinate with weight one if we found an exact hit" in {
      val agentCoordinates = AgentCoordinates(51.4380006, 7.4380005)

      DummyWeatherSource.getWeightedCoordinates(
        agentCoordinates,
        4,
        Quantities.getQuantity(28, PowerSystemUnits.KILOMETRE)
      ) match {
        case Success(WeightedCoordinates(weighting)) =>
          weighting.size shouldBe 1
          weighting.getOrElse(
            coordinate551525,
            fail("Expected coordinate wasn't found")
          ) shouldBe 1d
        case Failure(exception) =>
          fail(
            "Determining the nearest weighted coordinates was meant to succeed.",
            exception
          )
      }
    }

    "return four coordinates with respective weight" in {
      val agentCoordinates =
        AgentCoordinates(coordinate0.getY, coordinate0.getX)
      val expectedWeighting = Map(
        coordinate67775 -> 0.254626046882988,
        coordinate531137 -> 0.249222038996929,
        coordinate551525 -> 0.250659514620527,
        coordinate278150 -> 0.245492399499556
      )

      DummyWeatherSource.getWeightedCoordinates(
        agentCoordinates,
        4,
        Quantities.getQuantity(28, PowerSystemUnits.KILOMETRE)
      ) match {
        case Success(WeightedCoordinates(weighting)) =>
          weighting.corresponds(expectedWeighting) {
            case ((pointA, weightA), (pointB, weightB)) =>
              pointA.equalsExact(pointB, 1e-6) && Math.abs(
                weightA - weightB
              ) < 1e-6
          }
        case Failure(exception) =>
          fail(
            "Determining the nearest weighted coordinates was meant to succeed.",
            exception
          )
      }
    }

    "interpolate missing weather values correctly" in {
      val cases = Table(
        ("timeBasedValues", "time", "expectedWeatherData"),
        (Set.empty[TimeBasedValue[WeatherValue]], time, EMPTY_WEATHER_DATA),
        (
          Set(
            timeBasedValue0,
            timeBasedValue1,
            timeBasedValue2,
            timeBasedValue3,
            timeBasedValue4,
            timeBasedValue5,
            timeBasedValue6
          ),
          time.minusHours(3),
          EMPTY_WEATHER_DATA
        ),
        (
          Set(
            timeBasedValue0,
            timeBasedValue1,
            timeBasedValue2,
            timeBasedValue3,
            timeBasedValue4,
            timeBasedValue5,
            timeBasedValue6
          ),
          time.plusHours(2),
          EMPTY_WEATHER_DATA
        ),
        (
          Set(
            timeBasedValue0,
            timeBasedValue1,
            timeBasedValue2,
            timeBasedValue3,
            timeBasedValue4,
            timeBasedValue5,
            timeBasedValue6
          ),
          time,
          WeatherData(
            Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(15d, StandardUnits.TEMPERATURE),
            Quantities.getQuantity(0d, StandardUnits.WIND_VELOCITY)
          )
        )
      )

      forAll(cases) { (timeBasedValues, time, expectedWeatherData) =>
        val timeSeries: IndividualTimeSeries[WeatherValue] =
          buildTimeSeries(timeBasedValues)
        val weatherData: WeatherData = getWeatherData(timeSeries, time)

        weatherData.diffIrr shouldBe expectedWeatherData.diffIrr
        weatherData.dirIrr shouldBe expectedWeatherData.dirIrr
        weatherData.temp shouldBe expectedWeatherData.temp
        weatherData.windVel shouldBe expectedWeatherData.windVel
      }

    }

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
          Some(
            Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE),
            3900
          ),
          Some(
            Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE),
            6900
          ),
          Some(Quantities.getQuantity(15d, StandardUnits.TEMPERATURE), 3900),
          Some(Quantities.getQuantity(10d, Units.METRE_PER_SECOND), 3900)
        ),
        (
          Set(timeBasedValue0, timeBasedValue6),
          None,
          None,
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
          val weatherDataOption: WeatherDataOption =
            getPreviousValue(buildTimeSeries(timeBasedValues), time)

          weatherDataOption.diffIrr shouldBe expectedDiffIrr
          weatherDataOption.dirIrr shouldBe expectedDirIrr
          weatherDataOption.temp shouldBe expectedTemp
          weatherDataOption.windVel shouldBe expectedWindVel
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
            timeBasedValue5,
            timeBasedValue6
          ),
          Some(
            Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE),
            3300
          ),
          Some(
            Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE),
            3300
          ),
          Some(Quantities.getQuantity(15d, StandardUnits.TEMPERATURE), 6000),
          None
        ),
        (
          Set(timeBasedValue0, timeBasedValue5),
          Some(
            Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE),
            6000
          ),
          None,
          Some(Quantities.getQuantity(15d, StandardUnits.TEMPERATURE), 6000),
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
          val weatherDataOption: WeatherDataOption =
            getNextValue(buildTimeSeries(timeBasedValues), time)

          weatherDataOption.diffIrr shouldBe expectedDiffIrr
          weatherDataOption.dirIrr shouldBe expectedDirIrr
          weatherDataOption.temp shouldBe expectedTemp
          weatherDataOption.windVel shouldBe expectedWindVel
      }
    }

    "update weather data option correctly" in {
      val weatherDataOption: WeatherDataOption =
        WeatherDataOption(None, None, None, None)

      val cases = Table(
        ("givenValue", "givenWeight", "expectedResult"),
        (timeBasedValue0.getValue, 200L, weatherDataOption),
        (
          timeBasedValue1.getValue,
          300L,
          WeatherDataOption(
            Some(
              Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE),
              300L
            ),
            None,
            Some(
              Quantities.getQuantity(15d, StandardUnits.TEMPERATURE),
              300L
            ),
            Some(Quantities.getQuantity(10, Units.METRE_PER_SECOND), 300L)
          )
        ),
        (
          timeBasedValue2.getValue,
          250L,
          WeatherDataOption(
            Some(
              Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE),
              250L
            ),
            Some(
              Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE),
              250L
            ),
            Some(
              Quantities.getQuantity(15d, StandardUnits.TEMPERATURE),
              250L
            ),
            Some(Quantities.getQuantity(20, Units.METRE_PER_SECOND), 250L)
          )
        ),
        (
          timeBasedValue3.getValue,
          300L,
          WeatherDataOption(
            Some(
              Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE),
              300L
            ),
            None,
            Some(
              Quantities.getQuantity(15d, StandardUnits.TEMPERATURE),
              300L
            ),
            Some(Quantities.getQuantity(20, Units.METRE_PER_SECOND), 300L)
          )
        ),
        (
          timeBasedValue4.getValue,
          250L,
          WeatherDataOption(
            Some(
              Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE),
              250L
            ),
            Some(
              Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE),
              250L
            ),
            None,
            None
          )
        ),
        (
          timeBasedValue5.getValue,
          300L,
          WeatherDataOption(
            Some(
              Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE),
              300L
            ),
            None,
            Some(
              Quantities.getQuantity(15d, StandardUnits.TEMPERATURE),
              300L
            ),
            None
          )
        ),
        (
          timeBasedValue6.getValue,
          250L,
          WeatherDataOption(
            Some(
              Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE),
              250L
            ),
            Some(
              Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE),
              250L
            ),
            Some(
              Quantities.getQuantity(17d, StandardUnits.TEMPERATURE),
              250L
            ),
            Some(Quantities.getQuantity(20, Units.METRE_PER_SECOND), 250L)
          )
        )
      )

      forAll(cases) { (givenValue, givenWeight, expectedResult) =>
        val updatedWeatherDataOption =
          updateWeatherDataOption(weatherDataOption, givenValue, givenWeight)

        updatedWeatherDataOption shouldBe expectedResult
      }

    }
  }
}

case object WeatherSourceSpec {
  private val coordinate67775 = GeoUtils.buildPoint(51.5, 7.438)
  private val coordinate531137 = GeoUtils.buildPoint(51.5, 7.375)
  private val coordinate551525 = GeoUtils.buildPoint(51.438, 7.438)
  private val coordinate278150 = GeoUtils.buildPoint(51.438, 7.375)
  private val coordinate477295 = GeoUtils.buildPoint(52.312, 12.812)
  private val coordinate537947 = GeoUtils.buildPoint(52.25, 12.812)
  private val coordinate144112 = GeoUtils.buildPoint(52.312, 12.875)
  private val coordinate165125 = GeoUtils.buildPoint(52.25, 12.875)

  private val time: ZonedDateTime = ZonedDateTime.now()

  private val solarIrradianceValue0: SolarIrradianceValue =
    new SolarIrradianceValue(null, null)
  private val solarIrradianceValue1: SolarIrradianceValue =
    new SolarIrradianceValue(
      null,
      Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE)
    )
  private val solarIrradianceValue2: SolarIrradianceValue =
    new SolarIrradianceValue(
      Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE),
      Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE)
    )

  private val temperatureValue0: TemperatureValue = new TemperatureValue(null)
  private val temperatureValue1: TemperatureValue = new TemperatureValue(
    Quantities.getQuantity(288.15d, Units.KELVIN)
  )
  private val temperatureValue2: TemperatureValue = new TemperatureValue(
    Quantities.getQuantity(290.15d, Units.KELVIN)
  )

  private val windValue0: WindValue = new WindValue(null, null)
  private val windValue1: WindValue =
    new WindValue(null, Quantities.getQuantity(10, Units.METRE_PER_SECOND))
  private val windValue2: WindValue =
    new WindValue(null, Quantities.getQuantity(20, Units.METRE_PER_SECOND))

  private val timeBasedValue0: TimeBasedValue[WeatherValue] =
    buildTimeBasedValue(
      time,
      new WeatherValue(
        coordinate67775,
        solarIrradianceValue0,
        temperatureValue0,
        windValue0
      )
    )

  private val timeBasedValue1: TimeBasedValue[WeatherValue] =
    buildTimeBasedValue(
      time.minusMinutes(65),
      new WeatherValue(
        coordinate67775,
        solarIrradianceValue1,
        temperatureValue1,
        windValue1
      )
    )

  private val timeBasedValue2: TimeBasedValue[WeatherValue] =
    buildTimeBasedValue(
      time.minusMinutes(115),
      new WeatherValue(
        coordinate67775,
        solarIrradianceValue2,
        temperatureValue1,
        windValue2
      )
    )

  private val timeBasedValue3: TimeBasedValue[WeatherValue] =
    buildTimeBasedValue(
      time.minusMinutes(121),
      new WeatherValue(
        coordinate67775,
        solarIrradianceValue1,
        temperatureValue1,
        windValue2
      )
    )

  private val timeBasedValue4: TimeBasedValue[WeatherValue] =
    buildTimeBasedValue(
      time.plusMinutes(55),
      new WeatherValue(
        coordinate67775,
        solarIrradianceValue2,
        temperatureValue0,
        windValue0
      )
    )

  private val timeBasedValue5: TimeBasedValue[WeatherValue] =
    buildTimeBasedValue(
      time.plusMinutes(100),
      new WeatherValue(
        coordinate67775,
        solarIrradianceValue1,
        temperatureValue1,
        windValue0
      )
    )

  private val timeBasedValue6: TimeBasedValue[WeatherValue] =
    buildTimeBasedValue(
      time.plusMinutes(1),
      new WeatherValue(
        coordinate67775,
        solarIrradianceValue2,
        temperatureValue2,
        windValue2
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

  case object DummyWeatherSource extends WeatherSource {
    override protected val idCoordinateSource: IdCoordinateSource =
      DummyIdCoordinateSource

    /** Get the weather data for the given tick as a weighted average taking
      * into account the given weighting of weather coordinates.
      *
      * @param tick
      *   Simulation date in question
      * @param weightedCoordinates
      *   The coordinate in question
      * @return
      *   Matching weather data
      */
    override def getWeather(
        tick: Long,
        weightedCoordinates: WeightedCoordinates
    ): WeatherMessage.WeatherData =
      throw new UnsupportedOperationException(
        "This is not supported by the dummy source."
      )

    /** Determine an Array with all ticks between the request frame's start and
      * end on which new data is available. Bot the request frame's start and
      * end are INCLUDED.
      *
      * @param requestFrameStart
      *   Beginning of the announced request frame
      * @param requestFrameEnd
      *   End of the announced request frame
      * @return
      *   Array with data ticks
      */
    override def getDataTicks(
        requestFrameStart: Long,
        requestFrameEnd: Long
    ): Array[Long] =
      throw new UnsupportedOperationException(
        "This is not supported by the dummy source."
      )
  }

  case object DummyIdCoordinateSource extends IdCoordinateSource {
    private val idToCoordinate = Map(
      67775 -> coordinate67775,
      531137 -> coordinate531137,
      551525 -> coordinate551525,
      278150 -> coordinate278150,
      477295 -> coordinate477295,
      537947 -> coordinate537947,
      144112 -> coordinate144112,
      165125 -> coordinate165125
    )

    private val coordinateToId = idToCoordinate.map { case (key, value) =>
      value -> key
    }

    override def getCoordinate(id: Int): Optional[Point] =
      idToCoordinate.get(id).toJava

    override def getCoordinates(ids: Int*): util.Collection[Point] =
      ids.flatMap(idToCoordinate.get).toVector.asJava

    override def getId(coordinate: Point): Optional[Integer] =
      coordinateToId.get(coordinate).map(Integer.valueOf).toJava

    override def getAllCoordinates: util.Collection[Point] =
      idToCoordinate.values.toVector.asJava
  }
}
