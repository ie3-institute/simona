/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.hp.HpAgent
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.config.SimonaConfig.HpRuntimeConfig
import edu.ie3.simona.event.ResultEvent.{
  CylindricalStorageResultEvent,
  ParticipantResultEvent,
  ThermalHouseResultEvent,
}
import edu.ie3.simona.event.notifier.NotifierConfig
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
  protected val simulationEndDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-02T02:00:00Z")

  private val resolution =
    simonaConfig.simona.powerflow.resolution.getSeconds

  private val outputConfigOn = NotifierConfig(
    simulationResultInfo = true,
    powerRequestReply = false,
    flexResult = false,
  )

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
            typicalHpInputModel,
            typicalThermalGrid,
            HpRuntimeConfig(
              calculateMissingReactivePowerWithModel = true,
              1.0,
              List.empty[String],
            ),
            primaryServiceProxy.ref.toClassic,
            Iterable(ActorWeatherService(weatherService.ref.toClassic)),
            simulationStartDate,
            simulationEndDate,
            resolution,
            simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
            outputConfigOn,
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
        PrimaryServiceRegistrationMessage(typicalHpInputModel.getUuid)
      )
      heatPumpAgent ! RegistrationFailedMessage(
        primaryServiceProxy.ref.toClassic
      )

      weatherService.expectMessage(
        RegisterForWeatherMessage(
          typicalHpInputModel.getNode.getGeoPosition.getY,
          typicalHpInputModel.getNode.getGeoPosition.getX,
        )
      )

      heatPumpAgent ! RegistrationSuccessfulMessage(
        weatherService.ref.toClassic,
        Some(0L),
      )
      val weatherDependentAgents = Seq(heatPumpAgent)

      // scheduler.expectMessage(Completion(heatPumpAgent,Some(0L)))

      // weatherService ! Activation(0L)

      /* TICK 0
     House demand heating :
     House demand water   :
     ThermalStorage       :
     DomesticWaterStorage :

     Heat pump: off, can be turned on or stay off
     -> set point ~3.5 kW (bigger than 50 % rated apparent power): turned on
       */
      // scheduler ! ScheduleActivation(heatPumpAgent, 0L)
      heatPumpAgent ! Activation(0L)

      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          0,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(7200),
        )
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 0.toDateTime
          hpResult.getP should equalWithTolerance(0.0038.asMegaWatt)
          hpResult.getQ should equalWithTolerance(
            0.0012489995996796802.asMegaVar
          )
      }

      resultListener.expectMessageType[ThermalHouseResultEvent] match {
        case ThermalHouseResultEvent(thermalHouseResult) =>
          thermalHouseResult.getInputModel shouldBe typicalThermalHouse.getUuid
          thermalHouseResult.getTime shouldBe (-1).toDateTime
          thermalHouseResult.getqDot() should equalWithTolerance(0.0.asMegaWatt)
          thermalHouseResult.getIndoorTemperature should equalWithTolerance(
            20.asDegreeCelsius
          )
      }

      resultListener.expectMessageType[CylindricalStorageResultEvent] match {
        case CylindricalStorageResultEvent(thermalStorageResult) =>
          thermalStorageResult.getInputModel shouldBe typicalThermalStorage.getUuid
          thermalStorageResult.getTime shouldBe (-1).toDateTime
          thermalStorageResult.getqDot() should equalWithTolerance(0.asMegaWatt)
          thermalStorageResult.getEnergy should equalWithTolerance(
            0.asMegaWattHour
          )
      }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(0L)))

      // House is fully heated up
      heatPumpAgent ! Activation(6353L)

      // Results of 6353 for hp
      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 6353.toDateTime
          hpResult.getP should equalWithTolerance(0.0038.asMegaWatt)
          hpResult.getQ should equalWithTolerance(
            0.0012489995996796802.asMegaVar
          )
      }

      // Results of 0 for house and storage
      resultListener.expectMessageType[ThermalHouseResultEvent] match {
        case ThermalHouseResultEvent(thermalHouseResult) =>
          thermalHouseResult.getInputModel shouldBe typicalThermalHouse.getUuid
          thermalHouseResult.getTime shouldBe 0.toDateTime
          thermalHouseResult.getqDot() should equalWithTolerance(
            0.011.asMegaWatt
          )
          thermalHouseResult.getIndoorTemperature should equalWithTolerance(
            19.9999074074074.asDegreeCelsius
          )
      }

      resultListener.expectMessageType[CylindricalStorageResultEvent] match {
        case CylindricalStorageResultEvent(thermalStorageResult) =>
          thermalStorageResult.getInputModel shouldBe typicalThermalStorage.getUuid
          thermalStorageResult.getTime shouldBe 0.toDateTime
          thermalStorageResult.getqDot() should equalWithTolerance(0.asMegaWatt)
          thermalStorageResult.getEnergy should equalWithTolerance(
            0.asMegaWattHour
          )
      }
      // FIXME? Why next tick 6353?
      scheduler.expectMessage(Completion(heatPumpAgent, Some(6353L)))

      heatPumpAgent ! Activation(7200L)

      // weather update (unchanged)
      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          7200L,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(1d),
            WattsPerSquareMeter(1d),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(28800L),
        )
      }

      // Results of 7200 for hp
      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 7200.toDateTime
          hpResult.getP should equalWithTolerance(0.0038.asMegaWatt)
          hpResult.getQ should equalWithTolerance(
            0.0012489995996796802.asMegaVar
          )
      }

      // Results of 6353 for house and storage
      resultListener.expectMessageType[ThermalHouseResultEvent] match {
        case ThermalHouseResultEvent(thermalHouseResult) =>
          thermalHouseResult.getInputModel shouldBe typicalThermalHouse.getUuid
          thermalHouseResult.getTime shouldBe 6353.toDateTime
          thermalHouseResult.getqDot() should equalWithTolerance(0.asMegaWatt)
          thermalHouseResult.getIndoorTemperature should equalWithTolerance(
            21.99992810459535.asDegreeCelsius
          )
      }

      resultListener.expectMessageType[CylindricalStorageResultEvent] match {
        case CylindricalStorageResultEvent(thermalStorageResult) =>
          thermalStorageResult.getInputModel shouldBe typicalThermalStorage.getUuid
          thermalStorageResult.getTime shouldBe 6353.toDateTime
          thermalStorageResult.getqDot() should equalWithTolerance(
            0.011.asMegaWatt
          )
          thermalStorageResult.getEnergy should equalWithTolerance(
            0.asMegaWattHour
          )
      }

      // FIXME? Why next tick 7200?
      scheduler.expectMessage(Completion(heatPumpAgent, Some(7200L)))

      // Tick where storage will be full
      heatPumpAgent ! Activation(9770L)

      // Results of 9770 for hp
      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 9770.toDateTime
          hpResult.getP should equalWithTolerance(0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(
            0.asMegaVar
          )
      }

      // Results of 7200 for house and storage
      resultListener.expectMessageType[ThermalHouseResultEvent] match {
        case ThermalHouseResultEvent(thermalHouseResult) =>
          thermalHouseResult.getInputModel shouldBe typicalThermalHouse.getUuid
          thermalHouseResult.getTime shouldBe 7200.toDateTime
          thermalHouseResult.getqDot() should equalWithTolerance(
            0.asMegaWatt
          )
          thermalHouseResult.getIndoorTemperature should equalWithTolerance(
            21.9152283301339.asDegreeCelsius
          )
      }

      resultListener.expectMessageType[CylindricalStorageResultEvent] match {
        case CylindricalStorageResultEvent(thermalStorageResult) =>
          thermalStorageResult.getInputModel shouldBe typicalThermalStorage.getUuid
          thermalStorageResult.getTime shouldBe 7200.toDateTime
          thermalStorageResult.getqDot() should equalWithTolerance(
            0.011.asMegaWatt
          )
          thermalStorageResult.getEnergy should equalWithTolerance(
            0.002588055555555.asMegaWattHour
          )
      }

      // FIXME? Why next tick 9770?
      scheduler.expectMessage(Completion(heatPumpAgent, Some(9770L)))

      // house would reach lowerTempBoundary at tick 47518
      // but now it's getting colder which should decrease inner temp of house faster
      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          28800L,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(2d),
            WattsPerSquareMeter(2d),
            Celsius(-25d),
            MetersPerSecond(0d),
          ),
          Some(50400L),
        )
      }

      heatPumpAgent ! Activation(28800L)
      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 28800.toDateTime
          hpResult.getP should equalWithTolerance(0.0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.0.asMegaVar)
      }

      // Results of 9770 for house and storage
      resultListener.expectMessageType[ThermalHouseResultEvent] match {
        case ThermalHouseResultEvent(thermalHouseResult) =>
          thermalHouseResult.getInputModel shouldBe typicalThermalHouse.getUuid
          thermalHouseResult.getTime shouldBe 9770.toDateTime
          thermalHouseResult.getqDot() should equalWithTolerance(
            0.asMegaWatt
          )
          thermalHouseResult.getIndoorTemperature should equalWithTolerance(
            21.65903523084334.asDegreeCelsius
          )
      }

      resultListener.expectMessageType[CylindricalStorageResultEvent] match {
        case CylindricalStorageResultEvent(thermalStorageResult) =>
          thermalStorageResult.getInputModel shouldBe typicalThermalStorage.getUuid
          thermalStorageResult.getTime shouldBe 9770.toDateTime
          thermalStorageResult.getqDot() should equalWithTolerance(0.asMegaWatt)
          thermalStorageResult.getEnergy should equalWithTolerance(
            0.01044.asMegaWattHour
          )
      }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(28800L)))

      heatPumpAgent ! Activation(31106L)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 31106.toDateTime
          hpResult.getP should equalWithTolerance(0.0038.asMegaWatt)
          hpResult.getQ should equalWithTolerance(
            0.0012489995996796802.asMegaVar
          )
      }

      resultListener.expectMessageType[ThermalHouseResultEvent] match {
        case ThermalHouseResultEvent(thermalHouseResult) =>
          thermalHouseResult.getInputModel shouldBe typicalThermalHouse.getUuid
          thermalHouseResult.getTime shouldBe 28800.toDateTime
          thermalHouseResult.getqDot() should equalWithTolerance(0.0.asMegaWatt)
          thermalHouseResult.getIndoorTemperature should equalWithTolerance(
            18.3704373032769.asDegreeCelsius
          )
      }
      resultListener.expectMessageType[CylindricalStorageResultEvent] match {
        case CylindricalStorageResultEvent(thermalStorageResult) =>
          thermalStorageResult.getInputModel shouldBe typicalThermalStorage.getUuid
          thermalStorageResult.getTime shouldBe 28800.toDateTime
          thermalStorageResult.getqDot() should equalWithTolerance(
            0.0.asMegaWatt
          )
          thermalStorageResult.getEnergy should equalWithTolerance(
            0.01044.asMegaWattHour
          )
      }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(31106)))

      heatPumpAgent ! Activation(47225L)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 47225.toDateTime
          hpResult.getP should equalWithTolerance(0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(
            0.asMegaVar
          )
      }

      resultListener.expectMessageType[ThermalHouseResultEvent] match {
        case ThermalHouseResultEvent(thermalHouseResult) =>
          thermalHouseResult.getInputModel shouldBe typicalThermalHouse.getUuid
          thermalHouseResult.getTime shouldBe 31106.toDateTime
          thermalHouseResult.getqDot() should equalWithTolerance(
            0.011.asMegaWatt
          )
          thermalHouseResult.getIndoorTemperature should equalWithTolerance(
            18.00002164245703.asDegreeCelsius
          )
      }
      resultListener.expectMessageType[CylindricalStorageResultEvent] match {
        case CylindricalStorageResultEvent(thermalStorageResult) =>
          thermalStorageResult.getInputModel shouldBe typicalThermalStorage.getUuid
          thermalStorageResult.getTime shouldBe 31106.toDateTime
          thermalStorageResult.getqDot() should equalWithTolerance(
            0.0.asMegaWatt
          )
          thermalStorageResult.getEnergy should equalWithTolerance(
            0.01044.asMegaWattHour
          )
      }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(47225)))

      heatPumpAgent ! Activation(50400L)

      // it's getting warmer again
      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          50400L,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(3d),
            WattsPerSquareMeter(3d),
            Celsius(5d),
            MetersPerSecond(0d),
          ),
          Some(151200L),
        )
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 50400.toDateTime
          hpResult.getP should equalWithTolerance(0.0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.0.asMegaVar)
      }

      // results of tick 47225 where house reached the upper boundary
      // should be heated by using thermal energy from the storage FIXME
      resultListener.expectMessageType[ThermalHouseResultEvent] match {
        case ThermalHouseResultEvent(thermalHouseResult) =>
          thermalHouseResult.getInputModel shouldBe typicalThermalHouse.getUuid
          thermalHouseResult.getTime shouldBe 47225.toDateTime
          thermalHouseResult.getqDot() should equalWithTolerance(0.asMegaWatt)
          thermalHouseResult.getIndoorTemperature should equalWithTolerance(
            21.99992035040234.asDegreeCelsius
          )
      }
      resultListener.expectMessageType[CylindricalStorageResultEvent] match {
        case CylindricalStorageResultEvent(thermalStorageResult) =>
          thermalStorageResult.getInputModel shouldBe typicalThermalStorage.getUuid
          thermalStorageResult.getTime shouldBe 47225.toDateTime
          thermalStorageResult.getqDot() should equalWithTolerance(
            0.0.asMegaWatt
          )
          thermalStorageResult.getEnergy should equalWithTolerance(
            0.01044.asMegaWattHour
          )
      }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(50400L)))

    }
  }
}
