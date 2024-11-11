/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.weather

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.{
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.ontology.messages.services.WeatherMessage._
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.SimonaService
import edu.ie3.simona.service.weather.WeatherService.InitWeatherServiceStateData
import edu.ie3.simona.service.weather.WeatherSource.AgentCoordinates
import edu.ie3.simona.test.common.{
  ConfigTestData,
  TestKitWithShutdown,
  TestSpawnerClassic,
}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.quantities.WattsPerSquareMeter
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.testkit.{
  EventFilter,
  ImplicitSender,
  TestActorRef,
  TestProbe,
}
import org.scalatest.PrivateMethodTester
import org.scalatest.wordspec.AnyWordSpecLike
import squants.motion.MetersPerSecond
import squants.thermal.Celsius

class WeatherServiceSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "WeatherServiceSpec",
        ConfigFactory
          .parseString("""
             |pekko.loggers = ["org.apache.pekko.testkit.TestEventListener"]
             |pekko.loglevel = "INFO"
          """.stripMargin),
      )
    )
    with ImplicitSender
    with AnyWordSpecLike
    with PrivateMethodTester
    with LazyLogging
    with ConfigTestData
    with TestSpawnerClassic {

  // setup config for scheduler
  override protected val simonaConfig: SimonaConfig = read_conf_with_fallback("""
          simona.time.startDateTime = "2011-01-01T00:00:00Z"
          simona.time.endDateTime = "2011-01-01T01:00:00Z"
          simona.time.schedulerReadyCheckWindow = 900
          simona.input.grid.datasource.id = "csv"
          simona.input.grid.datasource.csvParams.folderPath = "netdata"
          simona.input.grid.datasource.csvParams.csvSep =","
          simona.input.weather.datasource.scheme = "icon"
          simona.input.weather.datasource.sampleParams.use = true
          simona.input.weather.datasource.coordinateSource.sampleParams.use = true
          simona.input.grid.datatarget.id = "csv"
          simona.powerflow.maxSweepPowerDeviation = 1E-5 // the maximum allowed deviation in power between two sweeps, before overall convergence is assumed

          simona.powerflow.newtonraphson.epsilon = [1E-12]
          simona.powerflow.newtonraphson.iterations = 50

          simona.simulationName = "ConfigTestDataSimulation"
          simona.gridConfig.refSystems = []
        """)

  // setup values
  private val invalidCoordinate: AgentCoordinates =
    AgentCoordinates(180.5, 90.5)
  private val validCoordinate: AgentCoordinates =
    AgentCoordinates(52.02083574, 7.40110716)

  private val scheduler = TestProbe("scheduler")

  // build the weather service
  private val weatherActor: TestActorRef[WeatherService] = TestActorRef(
    new WeatherService(
      scheduler.ref,
      TimeUtil.withDefaults.toZonedDateTime(
        simonaConfig.time.startDateTime
      ),
      TimeUtil.withDefaults.toZonedDateTime(
        simonaConfig.time.endDateTime
      ),
      4,
    )
  )

  "A weather service" must {
    "receive correct completion message after initialisation" in {
      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      scheduler.send(
        weatherActor,
        SimonaService.Create(
          InitWeatherServiceStateData(
            simonaConfig.input.weather.datasource
          ),
          key,
        ),
      )
      scheduler.expectMsg(
        ScheduleActivation(weatherActor.toTyped, INIT_SIM_TICK, Some(key))
      )

      scheduler.send(weatherActor, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(weatherActor.toTyped, Some(0)))
    }

    "announce failed weather registration on invalid coordinate" in {
      EventFilter
        .error(
          pattern =
            "\\[.*] Unable to obtain necessary information to register for coordinate AgentCoordinates\\(180\\.5,90\\.5\\)\\.",
          occurrences = 1,
        )
        .intercept {
          weatherActor ! RegisterForWeatherMessage(
            invalidCoordinate.latitude,
            invalidCoordinate.longitude,
          )
        }

      expectMsg(RegistrationFailedMessage(weatherActor))
    }

    "announce, that a valid coordinate is registered" in {
      /* The successful registration stems from the test above */
      weatherActor ! RegisterForWeatherMessage(
        validCoordinate.latitude,
        validCoordinate.longitude,
      )

      expectMsg(RegistrationSuccessfulMessage(weatherActor.ref, Some(0L)))
    }

    "recognize, that a valid coordinate yet is registered" in {
      /* The successful registration stems from the test above */
      EventFilter
        .warning(
          pattern = "Sending actor Actor\\[.*] is already registered",
          occurrences = 1,
        )
        .intercept {
          weatherActor ! RegisterForWeatherMessage(
            validCoordinate.latitude,
            validCoordinate.longitude,
          )
        }
      expectNoMessage()
    }

    "send out correct weather information upon activity start trigger and request the triggering for the next tick" in {
      /* Send out an activity start trigger as the scheduler */
      scheduler.send(weatherActor, Activation(0))

      scheduler.expectMsg(Completion(weatherActor.toTyped, Some(3600)))

      expectMsg(
        ProvideWeatherMessage(
          0,
          weatherActor,
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
      scheduler.send(weatherActor, Activation(3600))

      scheduler.expectMsg(Completion(weatherActor.toTyped))

      expectMsg(
        ProvideWeatherMessage(
          3600,
          weatherActor,
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
