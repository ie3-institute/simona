/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.weather

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.config.ConfigParams.SampleParams
import edu.ie3.simona.config.InputConfig.{CoordinateSource, WeatherDatasource}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.*
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.Data.SecondaryData.{
  SecondarySeriesData,
  WeatherData,
}
import edu.ie3.simona.service.DataTimeType
import edu.ie3.simona.service.weather.WeatherService.{
  InitWeatherServiceStateData,
  WeatherRegistrationData,
}
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.util.Coordinate
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.quantities.WattsPerSquareMeter
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.scalatest.PrivateMethodTester
import org.scalatest.wordspec.AnyWordSpecLike
import squants.motion.MetersPerSecond
import squants.thermal.Celsius
import squants.time.Hours

import java.time.ZonedDateTime
import scala.collection.immutable.SortedMap
import scala.language.implicitConversions

class WeatherServiceSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with PrivateMethodTester
    with LazyLogging
    with TestSpawnerTyped {

  private given simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2011-01-01T00:00:00Z")

  private val simulationEndDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2011-01-01T01:00:00Z")

  private val dataSourceConfig = WeatherDatasource(
    coordinateSource = CoordinateSource(
      sampleParams = Some(SampleParams())
    ),
    sampleParams = Some(SampleParams()),
  )

  // setup values
  private val invalidCoordinate: Coordinate = Coordinate(180.5, 90.5)
  private val validCoordinate: Coordinate = Coordinate(52.02083574, 7.40110716)

  private val scheduler = TestProbe[SchedulerMessage]("scheduler")

  private val agent1 = TestProbe[ParticipantAgent.Message]("agent1")
  private val agent2 = TestProbe[ParticipantAgent.Message]("agent2")

  "A weather service" must {
    val serviceKey =
      ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
    // lock activation scheduled
    scheduler.expectMessageType[ScheduleActivation]

    val weatherService = spawn(
      WeatherService(
        scheduler.ref,
        InitWeatherServiceStateData(
          dataSourceConfig,
          simulationStartDate,
          simulationEndDate,
        ),
        serviceKey,
      )
    )

    "send correct completion message after initialisation" in {
      scheduler.expectMessage(
        ScheduleActivation(weatherService, 0L, Some(serviceKey))
      )
    }

    "announce failed weather registration on invalid coordinate" in {
      weatherService ! SecondaryServiceRegistrationMessage(
        agent1.ref,
        DataTimeType.Current,
        WeatherRegistrationData(
          Coordinate(invalidCoordinate.latitude, invalidCoordinate.longitude)
        ),
      )

      agent1.expectMessage(RegistrationFailedMessage(weatherService))
    }

    "announce that a valid coordinate is registered for current weather data" in {
      weatherService ! SecondaryServiceRegistrationMessage(
        agent1.ref,
        DataTimeType.Current,
        WeatherRegistrationData(
          Coordinate(validCoordinate.latitude, validCoordinate.longitude)
        ),
      )

      agent1.expectMessage(
        RegistrationSuccessfulMessage(weatherService, 0L)
      )
    }

    "announce, that a valid coordinate is registered for forecast data" in {
      weatherService ! SecondaryServiceRegistrationMessage(
        agent2.ref,
        DataTimeType.CurrentAndForecast(
          forecastLength = Hours(6),
          forecastResolution = Hours(1),
        ),
        WeatherRegistrationData(
          Coordinate(validCoordinate.latitude, validCoordinate.longitude)
        ),
      )

      agent2.expectMessage(
        RegistrationSuccessfulMessage(weatherService, 0L)
      )

      agent1.expectNoMessage()
    }

    "recognize that agent is already registered" in {
      weatherService ! SecondaryServiceRegistrationMessage(
        agent1.ref,
        DataTimeType.Current,
        WeatherRegistrationData(
          Coordinate(validCoordinate.latitude, validCoordinate.longitude)
        ),
      )

      agent1.expectNoMessage()
      agent2.expectNoMessage()
    }

    "send out correct weather information upon activity start trigger and request the triggering for the next tick" in {
      /* Send out an activity start trigger as the scheduler */
      weatherService ! Activation(0)

      val activationMsg = scheduler.expectMessageType[Completion]
      activationMsg.newTick shouldBe Some(3600)

      agent1.expectMessage(
        DataProvision(
          0,
          weatherService,
          WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(-2.3719999999999573),
            MetersPerSecond(4.16474),
            Some(Celsius(-2.3719999999999573)),
            Some(Celsius(-2.3719999999999573)),
          ),
          Some(3600L),
        )
      )

      agent2.expectMessageType[DataProvision] match {
        case DataProvision(tick, serviceRef, data, nextTick) =>
          tick shouldBe 0L
          serviceRef shouldBe weatherService
          data match {
            case SecondarySeriesData(series) =>
              series.size shouldBe 7
            case unexpected =>
              fail(s"Received unexpected data $unexpected")
          }
          nextTick shouldBe Some(3600L)
      }

    }

    "sends out correct weather information when triggered again and does not as for triggering, if the end is reached" in {
      /* Send out an activity start trigger as the scheduler */
      weatherService ! Activation(3600)

      val completionMsg = scheduler.expectMessageType[Completion]
      completionMsg.newTick shouldBe None

      agent1.expectMessage(
        DataProvision(
          3600,
          weatherService,
          WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(-2.5259999999999536),
            MetersPerSecond(4.918092),
            Some(Celsius(-2.5259999999999536)),
            Some(Celsius(-2.5259999999999536)),
          ),
          None,
        )
      )
    }

    "reduce time series resolution according to a given interval" when {

      val timeSeries = SortedMap(
        0L -> 1,
        3600L -> 2,
        7200L -> 3,
        10800L -> 4,
      )

      "interval is finer than data resolution" in {
        WeatherService.reduceTimeSeriesResolution(
          timeSeries = timeSeries,
          resolution = Hours(0.5),
        ) should equal(timeSeries)

        WeatherService.reduceTimeSeriesResolution(
          timeSeries = timeSeries,
          resolution = Hours(0.75),
        ) should equal(timeSeries)
      }

      "interval matches data resolution" in {
        WeatherService.reduceTimeSeriesResolution(
          timeSeries = timeSeries,
          resolution = Hours(1),
        ) should equal(timeSeries)
      }

      "interval is more coarse than data resolution" in {
        val expectedResult = SortedMap(
          0L -> 1,
          7200L -> 3,
        )

        WeatherService.reduceTimeSeriesResolution(
          timeSeries = timeSeries,
          resolution = Hours(1.5),
        ) should equal(expectedResult)

        WeatherService.reduceTimeSeriesResolution(
          timeSeries = timeSeries,
          resolution = Hours(2),
        ) should equal(expectedResult)
      }

    }
  }
}
