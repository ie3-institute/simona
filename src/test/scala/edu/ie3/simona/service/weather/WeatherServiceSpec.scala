/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.weather

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.ParticipantAgent.{
  DataProvision,
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  Create,
  SecondaryServiceRegistrationMessage,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.Data.SecondaryData.{
  WeatherData,
  WeatherSeriesData,
}
import edu.ie3.simona.service.weather.WeatherService.{
  InitWeatherServiceStateData,
  WeatherRegistrationData,
}
import edu.ie3.simona.test.common.{ConfigTestData, TestSpawnerTyped}
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

import scala.collection.immutable.SortedMap
import scala.language.implicitConversions

class WeatherServiceSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with PrivateMethodTester
    with LazyLogging
    with ConfigTestData
    with TestSpawnerTyped {

  // setup config for scheduler
  private val config = ConfigFactory
    .parseString(s"""
            simona.time.startDateTime = "2011-01-01T00:00:00Z"
            simona.time.endDateTime = "2011-01-01T01:00:00Z"
            simona.time.schedulerReadyCheckWindow = 900
            simona.input.grid.datasource.id = "csv"
            simona.input.grid.datasource.csvParams.folderPath = "netdata"
            simona.input.grid.datasource.csvParams.csvSep =","
            simona.input.weather.datasource.scheme = "icon"
            simona.input.weather.datasource.sampleParams.use = true
            simona.input.weather.datasource.coordinateSource.sampleParams.use = true
            simona.powerflow.maxSweepPowerDeviation = 1E-5 // the maximum allowed deviation in power between two sweeps, before overall convergence is assumed
            simona.powerflow.newtonraphson.epsilon = [1E-12]
            simona.powerflow.newtonraphson.iterations = 50
            simona.simulationName = "ConfigTestDataSimulation"
            simona.gridConfig.refSystems = []
          """)
    .resolve()
    .withFallback(typesafeConfig)
  override protected val simonaConfig: SimonaConfig = SimonaConfig(config)

  // setup values
  private val invalidCoordinate: Coordinate = Coordinate(180.5, 90.5)
  private val validCoordinate: Coordinate = Coordinate(52.02083574, 7.40110716)

  private val scheduler = TestProbe[SchedulerMessage]("scheduler")

  private val agent1 = TestProbe[ParticipantAgent.Request]("agent1")
  private val agent2 = TestProbe[ParticipantAgent.Request]("agent2")

  // build the weather service
  private val weatherService = testKit.spawn(
    WeatherService(scheduler.ref)
  )

  "A weather service" must {
    "receive correct completion message after initialisation" in {
      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      scheduler
        .expectMessageType[ScheduleActivation] // lock activation scheduled

      weatherService ! Create(
        InitWeatherServiceStateData(
          simonaConfig.simona.input.weather.datasource,
          TimeUtil.withDefaults.toZonedDateTime(
            simonaConfig.simona.time.startDateTime
          ),
          TimeUtil.withDefaults.toZonedDateTime(
            simonaConfig.simona.time.endDateTime
          ),
        ),
        key,
      )

      val activationMsg = scheduler.expectMessageType[ScheduleActivation]
      activationMsg.tick shouldBe INIT_SIM_TICK
      activationMsg.unlockKey shouldBe Some(key)

      weatherService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(activationMsg.actor, Some(0)))
    }

    "announce failed weather registration on invalid coordinate" in {
      weatherService ! SecondaryServiceRegistrationMessage(
        agent1.ref,
        WeatherRegistrationData(
          Coordinate(invalidCoordinate.latitude, invalidCoordinate.longitude),
          WeatherDataType.Current,
        ),
      )

      agent1.expectMessage(RegistrationFailedMessage(weatherService))
    }

    "announce, that a valid coordinate is registered for current weather data" in {
      /* The successful registration stems from the test above */
      weatherService ! SecondaryServiceRegistrationMessage(
        agent1.ref,
        WeatherRegistrationData(
          Coordinate(validCoordinate.latitude, validCoordinate.longitude),
          WeatherDataType.Current,
        ),
      )

      agent1.expectMessage(
        RegistrationSuccessfulMessage(weatherService, 0L)
      )
    }

    "announce, that a valid coordinate is registered for forecast data" in {
      /* The successful registration stems from the test above */
      weatherService ! SecondaryServiceRegistrationMessage(
        agent2.ref,
        WeatherRegistrationData(
          Coordinate(validCoordinate.latitude, validCoordinate.longitude),
          WeatherDataType.CurrentAndForecast(
            forecastLength = Hours(6),
            forecastResolution = Hours(1),
          ),
        ),
      )

      agent2.expectMessage(
        RegistrationSuccessfulMessage(weatherService, 0L)
      )

      agent1.expectNoMessage()
    }

    "recognize, that a valid coordinate is already registered" in {
      /* The successful registration stems from the test above */
      weatherService ! SecondaryServiceRegistrationMessage(
        agent1.ref,
        WeatherRegistrationData(
          Coordinate(validCoordinate.latitude, validCoordinate.longitude),
          WeatherDataType.Current,
        ),
      )

      agent1.expectNoMessage()
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
          ),
          Some(3600L),
        )
      )

      agent2.expectMessageType[DataProvision] match {
        case DataProvision(tick, serviceRef, data, nextTick) =>
          tick shouldBe 0
          serviceRef shouldBe weatherService
          data match {
            case WeatherSeriesData(series) =>
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

      val activationMsg = scheduler.expectMessageType[Completion]
      activationMsg.newTick shouldBe None

      agent1.expectMessage(
        DataProvision(
          3600,
          weatherService,
          WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(-2.5259999999999536),
            MetersPerSecond(4.918092),
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
