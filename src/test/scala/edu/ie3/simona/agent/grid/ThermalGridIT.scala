/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.models.result.system.EmResult
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.hp.HpAgent
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.config.SimonaConfig.HpRuntimeConfig
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.model.thermal.ThermalHouseTestData
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.{
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.ontology.messages.services.WeatherMessage.{
  ProvideWeatherMessage,
  RegisterForWeatherMessage,
  WeatherData,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.simona.test.common.input.EmInputTestData
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.QuantityMatchers.equalWithTolerance
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.WattsPerSquareMeter
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.scaladsl.adapter.{TypedActorRefOps, _}
import org.apache.pekko.testkit.TestActorRef
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.mockito.MockitoSugar
import squants.motion.MetersPerSecond
import squants.thermal.Celsius

import java.time.ZonedDateTime
import scala.language.postfixOps

/** Test to ensure the functions that a [[GridAgent]] in center position should
  * be able to do if the DBFSAlgorithm is used. The scheduler, the weather
  * service as well as the inferior and superior [[GridAgent]] s are simulated
  * by the TestKit. By now this test does NOT cover interactions with generation
  * or load asset agents due to unavailability during test development. Hence it
  * would make sense to extend this test in the future to include asset agent
  * interaction or cover this behaviour by another (integration) test!
  */
class ThermalGridIT
    extends ScalaTestWithActorTestKit
    with ThermalHouseTestData
    with AnyWordSpecLike
    with should.Matchers
    with EmInputTestData
    with MockitoSugar
    with DefaultTestData
//    with TestSpawnerTyped
    {
  private implicit val classicSystem: ActorSystem = system.toClassic
  protected implicit val simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z")

  val scheduler: TestProbe[SchedulerMessage] = TestProbe("scheduler")
  val runtimeEvents: TestProbe[RuntimeEvent] =
    TestProbe("runtimeEvents")
  val primaryServiceProxy =
    TestProbe[ServiceMessage]("PrimaryServiceProxy")

  val weatherService = TestProbe[ServiceMessage]("WeatherService")

  private val environmentRefs = EnvironmentRefs(
    scheduler = scheduler.ref,
    runtimeEventListener = runtimeEvents.ref,
    primaryServiceProxy = primaryServiceProxy.ref.toClassic,
    weather = weatherService.ref.toClassic,
    evDataService = None,
  )

  val resultListener: TestProbe[ResultEvent] = TestProbe("resultListener")

  "A Thermal Grid with thermal house, storage and heat pump not under the control of an energy management" should {
    "be initialized correctly and run through some activations" in {
      val heatPumpAgent = TestActorRef(
        new HpAgent(
          scheduler = scheduler.ref.toClassic,
          initStateData = ParticipantInitializeStateData(
            hpInputModel,
            defaultThermalGrid,
            HpRuntimeConfig(
              calculateMissingReactivePowerWithModel = true,
              1.0,
              List.empty[String],
            ),
            primaryServiceProxy.ref.toClassic,
            Iterable(ActorWeatherService(weatherService.ref.toClassic)),
            defaultSimulationStart,
            defaultSimulationEnd,
            defaultResolution,
            simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
            defaultOutputConfig,
            None,
          ),
          listener = Iterable(resultListener.ref.toClassic),
        ),
        "HeatPumpAgent1",
      )

      scheduler.expectNoMessage()

      /* INIT */

      // heat pump
      heatPumpAgent ! Activation(INIT_SIM_TICK)

      primaryServiceProxy.expectMessage(
        PrimaryServiceRegistrationMessage(adaptedHpInputModel.getUuid)
      )
      heatPumpAgent ! RegistrationFailedMessage(
        primaryServiceProxy.ref.toClassic
      )

      weatherService.expectMessage(
        RegisterForWeatherMessage(
          hpInputModel.getNode.getGeoPosition.getY,
          hpInputModel.getNode.getGeoPosition.getX,
        )
      )

      heatPumpAgent ! RegistrationSuccessfulMessage(
        weatherService.ref.toClassic,
        Some(0L),
      )

      scheduler.expectMessage(Completion(heatPumpAgent))

      val weatherDependentAgents = Seq(heatPumpAgent)

      /* TICK 0
     LOAD: 0.000269 MW
     PV:  -0.005685 MW
     Heat pump: off, can be turned on or stay off
     -> set point ~3.5 kW (bigger than 50 % rated apparent power): turned on
     -> remaining -0.000566 MW
       */

      heatPumpAgent ! Activation(0)

      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          0,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(400d),
            WattsPerSquareMeter(200d),
            Celsius(0d),
            MetersPerSecond(0d),
          ),
          Some(7200),
        )
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 0.toDateTime
          emResult.getP should equalWithTolerance(
            (-0.000566087824).asMegaWatt
          )
          emResult.getQ should equalWithTolerance(0.001073120041.asMegaVar)
      }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(7200)))

      /* TICK 7200
     LOAD: 0.000269 MW (unchanged)
     PV:  -0.003797 MW
     Heat pump: running (turned on from last request), can also be turned off
     -> set point ~3.5 kW (bigger than 50 % rated apparent power): stays turned on with unchanged state
     -> remaining 0 MW
       */

      heatPumpAgent ! Activation(7200)

      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          7200,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(300d),
            WattsPerSquareMeter(500d),
            Celsius(0d),
            MetersPerSecond(0d),
          ),
          Some(14400),
        )
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 7200.toDateTime
          emResult.getP should equalWithTolerance(0.00132184544484.asMegaWatt)
          emResult.getQ should equalWithTolerance(0.001073120041.asMegaVar)
      }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(14400)))

      /* TICK 14400
     LOAD: 0.000269 MW (unchanged)
     PV:  -0.000066 MW
     Heat pump: Is still running, can still be turned off
     -> flex signal is 0 MW: Heat pump is turned off
       */

      heatPumpAgent ! Activation(14400)

      // it got cloudy now...
      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          14400,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(5d),
            WattsPerSquareMeter(5d),
            Celsius(0d),
            MetersPerSecond(0d),
          ),
          Some(21600),
        )
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 14400L.toDateTime
          emResult.getP should equalWithTolerance(0.000202956264.asMegaWatt)
          emResult.getQ should equalWithTolerance(0.000088285537.asMegaVar)
      }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(21600)))

      /* TICK 21600
     LOAD: 0.000269 MW (unchanged)
     PV:  -0.000032 MW
     Heat pump: Is not running, can run or stay off
     -> flex signal is 0 MW: Heat pump is turned off
       */

      heatPumpAgent ! Activation(21600)

      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          21600,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(5d),
            WattsPerSquareMeter(5d),
            Celsius(0d),
            MetersPerSecond(0d),
          ),
          Some(28800),
        )
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 21600.toDateTime
          emResult.getP should equalWithTolerance(0.0002367679996.asMegaWatt)
          emResult.getQ should equalWithTolerance(0.000088285537.asMegaVar)
      }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(28665)))

      /* TICK 28666
     LOAD: 0.000269 MW (unchanged)
     PV:  -0.000032 MW (unchanged)
     Heat pump: Is turned on again and cannot be turned off
     -> flex signal is no control -> 0.00485 MW
       */

      heatPumpAgent ! Activation(28665)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 28665.toDateTime
          emResult.getP should equalWithTolerance(0.0050867679996.asMegaWatt)
          emResult.getQ should equalWithTolerance(0.001073120040.asMegaVar)
      }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(28800)))
    }
  }
}
