/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.weather

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.testkit.{EventFilter, ImplicitSender, TestActorRef}
import org.apache.pekko.util.Timeout
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.{
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage
}
import edu.ie3.simona.ontology.messages.services.WeatherMessage._
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeServiceTrigger
}
import edu.ie3.simona.service.weather.WeatherService.InitWeatherServiceStateData
import edu.ie3.simona.service.weather.WeatherSource.AgentCoordinates
import edu.ie3.simona.test.common.{ConfigTestData, TestKitWithShutdown}
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.quantities.WattsPerSquareMeter
import org.scalatest.PrivateMethodTester
import org.scalatest.wordspec.AnyWordSpecLike
import squants.motion.MetersPerSecond
import squants.thermal.Celsius

import java.time.ZonedDateTime
import java.util.concurrent.TimeUnit

class WeatherServiceSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "WeatherServiceSpec",
        ConfigFactory
          .parseString("""
             |pekko.loggers = ["org.apache.pekko.testkit.TestEventListener"]
             |pekko.loglevel = "INFO"
          """.stripMargin)
      )
    )
    with ImplicitSender
    with AnyWordSpecLike
    with PrivateMethodTester
    with LazyLogging
    with ConfigTestData {

  private implicit val timeout: Timeout = Timeout(10, TimeUnit.SECONDS)

  // setup config for scheduler
  private val config = ConfigFactory
    .parseString(s"""
            simona.time.startDateTime = "2011-01-01 00:00:00"
            simona.time.endDateTime = "2011-01-01 01:00:00"
            simona.time.schedulerReadyCheckWindow = 900
            simona.input.grid.datasource.id = "csv"
            simona.input.grid.datasource.csvParams.folderPath = "netdata"
            simona.input.grid.datasource.csvParams.csvSep =","
            simona.input.weather.datasource.scheme = "icon"
            simona.input.weather.datasource.sampleParams.use = true
            simona.input.weather.datasource.coordinateSource.sampleParams.use = true
            simona.input.grid.datatarget.id = "csv"
            simona.powerflow.maxSweepPowerDeviation = 1E-5 // the maximum allowed deviation in power between two sweeps, before overall convergence is assumed
            simona.powerflow.skipOnFailure = true
            simona.powerflow.newtonraphson.epsilon = [1E-12]
            simona.powerflow.newtonraphson.iterations = 50
            simona.powerflow.resolution = "3600s"
            simona.simulationName = "ConfigTestDataSimulation"
            simona.gridConfig.refSystems = []
          """)
    .resolve()
    .withFallback(typesafeConfig)
  override protected val simonaConfig: SimonaConfig = SimonaConfig(config)

  // setup values
  private val triggerId = 0

  private val invalidCoordinate: AgentCoordinates =
    AgentCoordinates(180.5, 90.5)
  private val validCoordinate: AgentCoordinates =
    AgentCoordinates(52.02083574, 7.40110716)

  // convert tick from long into JAVA ZonedDateTime
  private implicit val startDateTime: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime(
      simonaConfig.simona.time.startDateTime
    )

  // build the weather service
  private val weatherActor: TestActorRef[WeatherService] = TestActorRef(
    new WeatherService(
      self,
      TimeUtil.withDefaults.toZonedDateTime(
        simonaConfig.simona.time.startDateTime
      ),
      TimeUtil.withDefaults.toZonedDateTime(
        simonaConfig.simona.time.endDateTime
      ),
      4
    )
  )

  "A weather service" must {
    "receive correct completion message after initialisation" in {
      weatherActor ! TriggerWithIdMessage(
        InitializeServiceTrigger(
          InitWeatherServiceStateData(
            simonaConfig.simona.input.weather.datasource
          )
        ),
        triggerId
      )

      expectMsg(
        CompletionMessage(
          0L,
          Some(
            ScheduleTriggerMessage(
              ActivityStartTrigger(0L),
              weatherActor
            )
          )
        )
      )

    }

    "announce failed weather registration on invalid coordinate" in {
      EventFilter
        .error(
          pattern =
            "\\[.*] Unable to obtain necessary information to register for coordinate AgentCoordinates\\(180\\.5,90\\.5\\)\\.",
          occurrences = 1
        )
        .intercept {
          weatherActor ! RegisterForWeatherMessage(
            invalidCoordinate.latitude,
            invalidCoordinate.longitude
          )
        }

      expectMsg(RegistrationFailedMessage)
    }

    "announce, that a valid coordinate is registered" in {
      /* The successful registration stems from the test above */
      weatherActor ! RegisterForWeatherMessage(
        validCoordinate.latitude,
        validCoordinate.longitude
      )

      expectMsg(RegistrationSuccessfulMessage(Some(0L)))
    }

    "recognize, that a valid coordinate yet is registered" in {
      /* The successful registration stems from the test above */
      EventFilter
        .warning(
          pattern = "Sending actor Actor\\[.*] is already registered",
          occurrences = 1
        )
        .intercept {
          weatherActor ! RegisterForWeatherMessage(
            validCoordinate.latitude,
            validCoordinate.longitude
          )
        }
      expectNoMessage()
    }

    "send out correct weather information upon activity start trigger and request the triggering for the next tick" in {
      /* Send out an activity start trigger as the scheduler */
      weatherActor ! TriggerWithIdMessage(ActivityStartTrigger(0L), 1L)

      /* Expect a weather provision (as this test is registered via the test actor) and a completion message (as the
       * test is also the scheduler) */
      expectMsgAllClassOf(
        classOf[ProvideWeatherMessage],
        classOf[CompletionMessage]
      ).foreach {
        case ProvideWeatherMessage(tick, weatherValue, nextDataTick) =>
          tick shouldBe 0L
          weatherValue shouldBe WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(-2.3719999999999573),
            MetersPerSecond(4.16474)
          )
          nextDataTick shouldBe Some(3600L)
        case CompletionMessage(triggerId, nextTriggers) =>
          triggerId shouldBe 1L
          nextTriggers shouldBe Some(
            ScheduleTriggerMessage(
              ActivityStartTrigger(3600L),
              weatherActor
            )
          )
      }
    }

    "sends out correct weather information when triggered again and does not as for triggering, if the end is reached" in {
      /* Send out an activity start trigger as the scheduler */
      weatherActor ! TriggerWithIdMessage(ActivityStartTrigger(3600L), 2L)

      /* Expect a weather provision (as this test is registered via the test actor) and a completion message (as the
       * test is also the scheduler) */
      expectMsgAllClassOf(
        classOf[ProvideWeatherMessage],
        classOf[CompletionMessage]
      ).foreach {
        case ProvideWeatherMessage(tick, weatherValue, nextDataTick) =>
          tick shouldBe 3600L
          weatherValue shouldBe WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(-2.5259999999999536),
            MetersPerSecond(4.918092)
          )
          nextDataTick shouldBe None
        case CompletionMessage(triggerId, nextTriggers) =>
          triggerId shouldBe 2L
          nextTriggers match {
            case Some(triggers) =>
              fail(s"Did not expect to get new triggers: $triggers")
            case None => succeed
          }
      }
    }
  }
}
