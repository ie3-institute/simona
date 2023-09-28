/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.weather

import edu.ie3.datamodel.io.factory.timeseries.{
  CosmoIdCoordinateFactory,
  IconIdCoordinateFactory,
  IdCoordinateFactory
}
import edu.ie3.datamodel.io.source.IdCoordinateSource
import edu.ie3.simona.exceptions.{
  InvalidConfigParameterException,
  ServiceException
}
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
import edu.ie3.simona.ontology.messages.services.WeatherMessage.QuantityWithWeight
import edu.ie3.simona.service.weather.WeatherSource.{
  AgentCoordinates,
  EMPTY_WEATHER_DATA,
  WeightedCoordinates,
  getValue,
  interpolate
}
import edu.ie3.simona.service.weather.WeatherSourceSpec._
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.geo.{CoordinateDistance, GeoUtils}
import edu.ie3.util.quantities.QuantityUtil
import org.locationtech.jts.geom.{Envelope, Point}
import tech.units.indriya.ComparableQuantity
import edu.ie3.util.quantities.interfaces.Irradiance
import edu.ie3.util.quantities.{PowerSystemUnits, QuantityUtil}
import org.locationtech.jts.geom.Point
import org.scalatest.prop.TableDrivenPropertyChecks
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.time.ZonedDateTime
import java.util
import java.util.Optional
import javax.measure.quantity.Length
import java.util.{Optional, UUID}
import javax.measure.quantity.{Speed, Temperature}
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.util.{Failure, Success, Try}

class WeatherSourceSpec extends UnitSpec with TableDrivenPropertyChecks {
  private val coordinate0 = GeoUtils.buildPoint(51.47, 7.41)

  "A weather source" should {
    "issue a ServiceException, if there are not enough coordinates available" in {
      DummyWeatherSource.getNearestCoordinatesWithDistances(
        AgentCoordinates(coordinate0.getY, coordinate0.getX),
        9
      ) match {
        case Failure(exception: ServiceException) =>
          exception.getMessage shouldBe "There are not enough coordinates for averaging. Found 8 within the given distance of 400000 m but need 9. Please make sure that there are enough coordinates within the given distance."
        case _ => fail("You shall not pass!")
      }
    }
    "issue a ServiceException, if there are not enough coordinates in max distance available" in {
      DummyWeatherSource.getNearestCoordinatesWithDistances(
        AgentCoordinates(coordinate0.getY, coordinate0.getX),
        9
      ) match {
        case Failure(exception: ServiceException) =>
          exception.getMessage shouldBe "There are not enough coordinates for averaging. Found 8 within the given distance of 400000 m but need 9. Please make sure that there are enough coordinates within the given distance."
        case _ => fail("You shall not pass!")
      }
    }

    "issue a ServiceException, if the queried coordinate is not surrounded by the found weather coordinates" in {
      val agentCoordinates = AgentCoordinates(51.3, 7.3)
      DummyWeatherSource.getNearestCoordinatesWithDistances(
        agentCoordinates,
        4
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
        4
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
        4
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
        9
      ) match {
        case Failure(exception: ServiceException) =>
          exception.getMessage shouldBe "Determination of coordinate weights failed."
          exception.getCause shouldBe ServiceException(
            "There are not enough coordinates for averaging. Found 8 within the given distance of 400000 m but need 9. Please make sure that there are enough coordinates within the given distance."
          )
        case _ => fail("You shall not pass!")
      }
    }

    "return one coordinate with weight one if we found an exact hit" in {
      val agentCoordinates = AgentCoordinates(51.4380006, 7.4380005)

      DummyWeatherSource.getWeightedCoordinates(
        agentCoordinates,
        4
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
        4
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

    "return correct coordinate factory" in {
      val checkCoordinateFactory =
        PrivateMethod[IdCoordinateFactory](Symbol("checkCoordinateFactory"))

      val cases = Table(
        ("gridModel", "expectedClass", "failureMessage"),
        (
          "",
          classOf[InvalidConfigParameterException],
          "No grid model defined!"
        ),
        ("icon", classOf[IconIdCoordinateFactory], ""),
        ("cosmo", classOf[CosmoIdCoordinateFactory], ""),
        (
          "else",
          classOf[InvalidConfigParameterException],
          "Grid model 'else' is not supported!"
        )
      )

      forAll(cases) { (gridModel, expectedClass, failureMessage) =>
        val actual =
          Try(WeatherSource invokePrivate checkCoordinateFactory(gridModel))

        actual match {
          case Success(factory) =>
            factory.getClass shouldBe expectedClass

          case Failure(exception) =>
            exception.getClass shouldBe expectedClass
            exception.getMessage shouldBe failureMessage
        }
      }
    }

    "interpolate missing weather values correctly" in {
      val cases = Table(
        ("previousOption", "nextOption", "emptyValue", "expectedValue"),
        (None, None, EMPTY_WEATHER_DATA.diffIrr, EMPTY_WEATHER_DATA.diffIrr),
        (
          Some(QuantityWithWeight(EMPTY_WEATHER_DATA.diffIrr, 0L)),
          None,
          EMPTY_WEATHER_DATA.diffIrr,
          EMPTY_WEATHER_DATA.diffIrr
        ),
        (
          Some(
            QuantityWithWeight(
              Quantities.getQuantity(10, StandardUnits.SOLAR_IRRADIANCE),
              10
            )
          ),
          Some(
            QuantityWithWeight(
              Quantities.getQuantity(20, StandardUnits.SOLAR_IRRADIANCE),
              10
            )
          ),
          EMPTY_WEATHER_DATA.diffIrr,
          Quantities.getQuantity(15, StandardUnits.SOLAR_IRRADIANCE)
        )
      )

      forAll(cases) { (previousOption, nextOption, emptyValue, expectedValue) =>
        val value: ComparableQuantity[Irradiance] =
          interpolate(time, previousOption, nextOption, emptyValue)
        value shouldBe expectedValue
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
            QuantityWithWeight(
              Quantities.getQuantity(50d, StandardUnits.SOLAR_IRRADIANCE),
              1800
            )
          ),
          Some(
            QuantityWithWeight(
              Quantities.getQuantity(50d, StandardUnits.SOLAR_IRRADIANCE),
              1800
            )
          ),
          Some(
            QuantityWithWeight(
              Quantities.getQuantity(-35d, StandardUnits.TEMPERATURE),
              6000
            )
          ),
          Some(
            QuantityWithWeight(
              Quantities.getQuantity(10d, Units.METRE_PER_SECOND),
              6000
            )
          )
        ),
        (
          Set(timeBasedValue0),
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
          val timeSeries: IndividualTimeSeries[WeatherValue] =
            buildTimeSeries(timeBasedValues)
          val intervalStart: ZonedDateTime = time.minusHours(2)

          val diffIrr: Option[QuantityWithWeight[Irradiance]] = getValue(
            timeSeries,
            time,
            intervalStart,
            intervalEnd = time,
            "diffIrr"
          )
          diffIrr shouldBe expectedDiffIrr

          val dirIrr: Option[QuantityWithWeight[Irradiance]] = getValue(
            timeSeries,
            time,
            intervalStart,
            intervalEnd = time,
            "diffIrr"
          )
          dirIrr shouldBe expectedDirIrr

          val temp: Option[QuantityWithWeight[Temperature]] = getValue(
            timeSeries,
            time,
            intervalStart,
            intervalEnd = time,
            "temp"
          )
          temp shouldBe expectedTemp

          val windVel: Option[QuantityWithWeight[Speed]] = getValue(
            timeSeries,
            time,
            intervalStart,
            intervalEnd = time,
            "windVel"
          )
          windVel shouldBe expectedWindVel
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
          Some(
            QuantityWithWeight(
              Quantities.getQuantity(60d, StandardUnits.SOLAR_IRRADIANCE),
              1800
            )
          ),
          Some(
            QuantityWithWeight(
              Quantities.getQuantity(60d, StandardUnits.SOLAR_IRRADIANCE),
              1800
            )
          ),
          Some(
            QuantityWithWeight(
              Quantities.getQuantity(15d, StandardUnits.TEMPERATURE),
              1800
            )
          ),
          Some(
            QuantityWithWeight(
              Quantities.getQuantity(20d, StandardUnits.WIND_VELOCITY),
              1800
            )
          )
        ),
        (
          Set(timeBasedValue0, timeBasedValue4, timeBasedValue5),
          Some(
            QuantityWithWeight(
              Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE),
              6000
            )
          ),
          Some(
            QuantityWithWeight(
              Quantities.getQuantity(40d, StandardUnits.SOLAR_IRRADIANCE),
              6000
            )
          ),
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

          val diffIrr: Option[QuantityWithWeight[Irradiance]] = getValue(
            timeSeries,
            time,
            intervalStart = time,
            intervalEnd,
            "diffIrr"
          )
          diffIrr shouldBe expectedDiffIrr

          val dirIrr: Option[QuantityWithWeight[Irradiance]] = getValue(
            timeSeries,
            time,
            intervalStart = time,
            intervalEnd,
            "diffIrr"
          )
          dirIrr shouldBe expectedDirIrr

          val temp: Option[QuantityWithWeight[Temperature]] = getValue(
            timeSeries,
            time,
            intervalStart = time,
            intervalEnd,
            "temp"
          )
          temp shouldBe expectedTemp

          val windVel: Option[QuantityWithWeight[Speed]] = getValue(
            timeSeries,
            time,
            intervalStart = time,
            intervalEnd,
            "windVel"
          )
          windVel shouldBe expectedWindVel
      }
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

  case object DummyWeatherSource extends WeatherSource {
    override protected val idCoordinateSource: IdCoordinateSource =
      DummyIdCoordinateSource
    override protected val maxCoordinateDistance: ComparableQuantity[Length] =
      Quantities.getQuantity(400000, Units.METRE)

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

    def getClosestCoordinates(
        coordinate: Point,
        n: Int,
        distance: ComparableQuantity[Length]
    ): util.List[CoordinateDistance] = {
      val points: Set[Point] = coordinateToId.keySet

      val envelope: Envelope =
        GeoUtils.calculateBoundingBox(coordinate, distance)

      val reducedPoints: Set[Point] = points.flatMap { point =>
        if (envelope.contains(point.getCoordinate)) {
          Some(point)
        } else {
          None
        }
      }

      calculateCoordinateDistances(coordinate, n, reducedPoints.asJava)
    }

    override def getNearestCoordinates(
        coordinate: Point,
        n: Int
    ): util.List[CoordinateDistance] = {
      calculateCoordinateDistances(coordinate, n, coordinateToId.keySet.asJava)
    }
  }
}
