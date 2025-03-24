/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.weather

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.agent.participant2.ParticipantAgent.{
  DataProvision,
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  Create,
  WrappedActivation,
}
import edu.ie3.simona.ontology.messages.services.WeatherMessage._
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.weather.WeatherService.InitWeatherServiceStateData
import edu.ie3.simona.service.weather.WeatherSource.AgentCoordinates
import edu.ie3.simona.test.common.{ConfigTestData, TestSpawnerTyped}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.quantities.WattsPerSquareMeter
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import org.scalatest.PrivateMethodTester
import org.scalatest.wordspec.AnyWordSpecLike
import squants.motion.MetersPerSecond
import squants.thermal.Celsius

import scala.language.implicitConversions

class WeatherServiceSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with PrivateMethodTester
    with LazyLogging
    with ConfigTestData
    with TestSpawnerTyped {

  implicit def wrap(msg: Activation): ServiceMessage = WrappedActivation(msg)

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
  private val invalidCoordinate: AgentCoordinates =
    AgentCoordinates(180.5, 90.5)
  private val validCoordinate: AgentCoordinates =
    AgentCoordinates(52.02083574, 7.40110716)

  private val scheduler = TestProbe[SchedulerMessage]("scheduler")

  private val agent = TestProbe[Any]("agent")

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
      weatherService ! RegisterForWeatherMessage(
        agent.ref,
        invalidCoordinate.latitude,
        invalidCoordinate.longitude,
      )

      agent.expectMessage(RegistrationFailedMessage(weatherService))
    }

    "announce, that a valid coordinate is registered" in {
      /* The successful registration stems from the test above */
      weatherService ! RegisterForWeatherMessage(
        agent.ref,
        validCoordinate.latitude,
        validCoordinate.longitude,
      )

      agent.expectMessage(
        RegistrationSuccessfulMessage(weatherService, 0L)
      )
    }

    "recognize, that a valid coordinate yet is registered" in {
      /* The successful registration stems from the test above */
      weatherService ! RegisterForWeatherMessage(
        agent.ref,
        validCoordinate.latitude,
        validCoordinate.longitude,
      )

      agent.expectNoMessage()
    }

    "send out correct weather information upon activity start trigger and request the triggering for the next tick" in {
      /* Send out an activity start trigger as the scheduler */
      weatherService ! Activation(0)

      val activationMsg = scheduler.expectMessageType[Completion]
      activationMsg.newTick shouldBe Some(3600)

      agent.expectMessage(
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

    }

    "sends out correct weather information when triggered again and does not as for triggering, if the end is reached" in {
      /* Send out an activity start trigger as the scheduler */
      weatherService ! Activation(3600)

      val activationMsg = scheduler.expectMessageType[Completion]
      activationMsg.newTick shouldBe None

      agent.expectMessage(
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
  }
}
