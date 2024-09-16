/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.hp.HpAgent
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.config.SimonaConfig.HpRuntimeConfig
import edu.ie3.simona.event.ResultEvent.{
  CylindricalThermalStorageResult,
  DomesticHotWaterStorageResult,
  ParticipantResultEvent,
  ThermalHouseResult,
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
import edu.ie3.simona.test.common.input.ThermalGridITInputTestData
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
import org.apache.pekko.actor.typed.scaladsl.adapter._
import org.apache.pekko.testkit.TestActorRef
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.mockito.MockitoSugar
import squants.motion.MetersPerSecond
import squants.thermal.Celsius

import java.time.ZonedDateTime
import scala.language.postfixOps

/** Test to ensure the functions that a thermal grid and its connected assets is
  * capable.
  */
class ThermalGridIT
    extends ScalaTestWithActorTestKit
    with ThermalHouseTestData
    with AnyWordSpecLike
    with should.Matchers
    with ThermalGridITInputTestData
    with MockitoSugar
    with DefaultTestData {
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

  val resultListener: TestProbe[ResultEvent] = TestProbe("resultListener")

  "A Thermal Grid with thermal house, storage and heat pump not under the control of an energy management" should {
    "be initialized correctly and run through some activations" in {
      val heatPumpAgent = TestActorRef(
        new HpAgent(
          scheduler = scheduler.ref.toClassic,
          initStateData = ParticipantInitializeStateData(
            typicalHpInputModel,
            thermalGridForThermalGridIT,
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

      val pRunningHp = 0.0038.asMegaWatt
      val qRunningHp = 0.0012489995996796802.asMegaVar

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

      // TODOs
      // - check storedEnergy of domestic hot water storage for all steps
      // - hourly results for covering the hot water storage demand from heat storage

      /* TICK 0
      Start of Simulation
      House demand heating : requiredDemand = 0.0 kWh, additionalDemand ~ 15 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 10.44 kWh, additionalDemand = 10.44 kWh
      DomesticWaterStorage : tba
      Heat pump: turned on - to serve the storage demand
       */

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
          hpResult.getP should equalWithTolerance(pRunningHp)
          hpResult.getQ should equalWithTolerance(
            qRunningHp
          )
      }

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe (-1).toDateTime
              qDot should equalWithTolerance(0.0.asMegaWatt)
              indoorTemperature should equalWithTolerance(20.asDegreeCelsius)
            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe (-1).toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(0.asMegaWattHour)
            case DomesticHotWaterStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
              time shouldBe (-1).toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(0.00149814.asMegaWattHour)
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(0L)))

      /* TICK 23
      Domestic hot water storage stops discharging
      House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 17.37 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      DomesticWaterStorage : tba
      Heat pump: stays on since it was on and the house as additional demand
       */

      heatPumpAgent ! Activation(23)

      // Results of tick 23 for hp
      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 23.toDateTime
          hpResult.getP should equalWithTolerance(pRunningHp)
          hpResult.getQ should equalWithTolerance(
            qRunningHp
          )
      }

      // Results of 0 for house and storage
      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 0.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                19.9999074074074.asDegreeCelsius
              )
            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 0.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              energy should equalWithTolerance(0.asMegaWattHour)
            case DomesticHotWaterStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
              time shouldBe 0.toDateTime
              qDot should equalWithTolerance((-0.01055664943).asMegaWatt)
              // FIXME We do make a significant error here since discharging little time frames with high power at a resolution of one second makes error. Maybe one can create this two staged.
              //  First calculate the duration at full power, take the next second and lower the power to the value matching the demand.
              // Done: But maybe additionally some separate test just for this, thus we test this before the integration test....
              energy should equalWithTolerance(0.00149814.asMegaWattHour)
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      // FIXME? Why next tick 23?
      scheduler.expectMessage(Completion(heatPumpAgent, Some(23)))

      /* TICK 3417
      ThermalStorage is fully heated up
      House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 17.37 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      DomesticWaterStorage : tba
      Heat pump: stays on since it was on and the house as additional demand
       */

      heatPumpAgent ! Activation(3417)

      // Results of tick 3417 for hp
      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 3417.toDateTime
          hpResult.getP should equalWithTolerance(pRunningHp)
          hpResult.getQ should equalWithTolerance(
            qRunningHp
          )
      }

      // Results of 0 for house and storage
      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 23.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                19.9977777856653.asDegreeCelsius
              )
            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 23.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              energy should equalWithTolerance(0.000070277777.asMegaWattHour)
            case DomesticHotWaterStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
              time shouldBe 23.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(0.00143069474.asMegaWattHour)
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      // FIXME? Why next tick 3417?
      scheduler.expectMessage(Completion(heatPumpAgent, Some(3417)))

      /* TICK 7200
      New weather data (unchanged) incoming + Domestic hot water storage will cover hot water demand
      House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 8.41 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      DomesticWaterStorage : tba
      Heat pump: stays on
       */

      heatPumpAgent ! Activation(7200L)

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
          hpResult.getP should equalWithTolerance(pRunningHp)
          hpResult.getQ should equalWithTolerance(
            qRunningHp
          )
      }

      // Results of 3417 for house and storage
      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 3417.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                19.683546460463276.asDegreeCelsius
              )

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 3417.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(0.01044.asMegaWattHour)
            case DomesticHotWaterStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
              time shouldBe 3417.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(0.0014306947.asMegaWattHour)
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      // FIXME? Why next tick 7200?
      scheduler.expectMessage(Completion(heatPumpAgent, Some(7200L)))

      /* TICK 7220
Domestic hot water storage stops discharging
House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 8.41 kWh
House demand water   : tba
ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
DomesticWaterStorage : tba
Heat pump: stays on
       */

      heatPumpAgent ! Activation(7220L)

      // Results of 7220 for hp
      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 7220.toDateTime
          hpResult.getP should equalWithTolerance(pRunningHp)
          hpResult.getQ should equalWithTolerance(
            qRunningHp
          )
      }

      // Results of 7200 for house and storage
      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 7200.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                20.8789247706.asDegreeCelsius
              )
            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 7200.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(0.01044.asMegaWattHour)
            case DomesticHotWaterStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
              time shouldBe 7200.toDateTime
              qDot should equalWithTolerance((-0.0107912416).asMegaWatt)
              energy should equalWithTolerance(0.00143069464.asMegaWattHour)
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      // FIXME? Why next tick 7220?
      scheduler.expectMessage(Completion(heatPumpAgent, Some(7220L)))

      /* TICK 10799
      House reaches upper temperature boundary
      House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      DomesticWaterStorage : tba
      Heat pump: turned off
       */

      heatPumpAgent ! Activation(10799)

      // Results of 10799 for hp
      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 10799.toDateTime
          hpResult.getP should equalWithTolerance(0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.asMegaVar)
      }

      // Results of 7220 for house and storages
      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 7220.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                20.88515596136936.asDegreeCelsius
              )
            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 7220.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(0.01044.asMegaWattHour)
            case DomesticHotWaterStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
              time shouldBe 7220.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(
                0.001370743397260274.asMegaWattHour
              )
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      // FIXME? Why next tick 10799?
      scheduler.expectMessage(Completion(heatPumpAgent, Some(10799)))

      /* TICK 28800
      House would reach lowerTempBoundary at tick 50797
      but now it's getting colder which should decrease inner temp of house faster
      House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      DomesticWaterStorage : tba
      Heat pump: stays off
       */

      heatPumpAgent ! Activation(28800L)

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
          Some(45000L),
        )
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 28800.toDateTime
          hpResult.getP should equalWithTolerance(0.0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.0.asMegaVar)
      }

      // Results of 10798 for house and storage
      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 10799.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                22.0001449495703.asDegreeCelsius
              )
            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 10799.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(0.01044.asMegaWattHour)
            case DomesticHotWaterStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
              time shouldBe 10799.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(
                0.001370743397260274.asMegaWattHour
              )
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(28800L)))

      /* TICK 28987
   Domestic hot water storage will be empty
   House demand heating : requiredDemand = 15.0 kWh, additionalDemand = 30.00 kWh
   House demand water   : tba
   ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
   DomesticWaterStorage : tba
   Heat pump: stays off, demand should be covered by storage
       */

      heatPumpAgent ! Activation(28987)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 28987.toDateTime
          hpResult.getP should equalWithTolerance(0.0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(
            0.0.asMegaVar
          )
      }

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 28800.toDateTime
              qDot should equalWithTolerance(0.0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                20.2000352857288.asDegreeCelsius
              )

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 28800.toDateTime
              qDot should equalWithTolerance(0.0.asMegaWatt)
              energy should equalWithTolerance(0.01044.asMegaWattHour)
            case DomesticHotWaterStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
              time shouldBe 28800.toDateTime
              qDot should equalWithTolerance((-0.010964363167533513).asMegaWatt)
              energy should equalWithTolerance(
                0.001370743397260274.asMegaWattHour
              )
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(28987)))

      /* TICK 41951
      House reach lowerTemperatureBoundary
      House demand heating : requiredDemand = 15.0 kWh, additionalDemand = 30.00 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      DomesticWaterStorage : tba
      Heat pump: stays off, demand should be covered by storage
       */

      heatPumpAgent ! Activation(41951)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 41951.toDateTime
          hpResult.getP should equalWithTolerance(0.0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(
            0.0.asMegaVar
          )
      }

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 28987.toDateTime
              qDot should equalWithTolerance(0.0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                20.16873007610494.asDegreeCelsius
              )

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 28987.toDateTime
              qDot should equalWithTolerance(0.0.asMegaWatt)
              energy should equalWithTolerance(0.01044.asMegaWattHour)
            case DomesticHotWaterStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
              time shouldBe 28987.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(
                0.0008012056438356164.asMegaWattHour
              )
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(41951)))

      /* TICK 42171
      Domestic hot water storage will stop discharging, and its SOC will be less than 20%, thus it need to be recharged
      House demand heating : requiredDemand = 15.0 kWh, additionalDemand = 30.00 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      DomesticWaterStorage : tba
      Heat pump: stays off, demand should be covered by storage
       */

      heatPumpAgent ! Activation(42171)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 42171.toDateTime
          hpResult.getP should equalWithTolerance(0.0038.asMegaWatt)
          hpResult.getQ should equalWithTolerance(
            0.0012489995996796802.asMegaVar
          )
      }

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 41951.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                17.9999618660804.asDegreeCelsius
              )

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 41951.toDateTime
              qDot should equalWithTolerance((-0.011).asMegaWatt)
              energy should equalWithTolerance(0.01044.asMegaWattHour)
            case DomesticHotWaterStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
              time shouldBe 41951.toDateTime
              qDot should equalWithTolerance((-0.010975183262764632).asMegaWatt)
              energy should equalWithTolerance(
                0.0008012056438356164.asMegaWattHour
              )
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(42171)))

      /* TICK 42619
      Domestic hot water storage will be full
      Additional trigger caused by (unchanged) weather data should not change this
      House demand heating : requiredDemand = 9.78 kWh, additionalDemand = 24.78 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 8.87 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      Heat pump: Since Hp is running, it will kept running to heat house and recharge thermal storage
       */

      heatPumpAgent ! Activation(42619)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 42619.toDateTime
          hpResult.getP should equalWithTolerance(0.0038.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.00124899959968.asMegaVar)
      }

      // Results of 42171 for house and storage
      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 42171.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                18.0545544897451.asDegreeCelsius
              )
            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 42171.toDateTime
              qDot should equalWithTolerance((-0.011).asMegaWatt)
              energy should equalWithTolerance(
                0.009767777777777778.asMegaWattHour
              )
            case DomesticHotWaterStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
              time shouldBe 42171.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              energy should equalWithTolerance(0.00013049999.asMegaWattHour)
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(42619)))

      /* TICK 45000
      Additional trigger caused by (unchanged) weather data should not change this
      House demand heating : requiredDemand = 9.78 kWh, additionalDemand = 24.78 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 8.87 kWh
      DomesticWaterStorage : tba
      Heat pump: stays off
       */

      heatPumpAgent ! Activation(45000)

      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          45000,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(3d),
            WattsPerSquareMeter(3d),
            Celsius(-25d),
            MetersPerSecond(0d),
          ),
          Some(57600),
        )
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 45000.toDateTime
          hpResult.getP should equalWithTolerance(0.0038.asMegaWatt)
          hpResult.getQ should equalWithTolerance(
            0.0012489995996796802.asMegaVar
          )
      }

      // Results of 34132 for house and storage
      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 42619.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                18.1656343400732.asDegreeCelsius
              )
            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 42619.toDateTime
              qDot should equalWithTolerance(0.0.asMegaWatt)
              energy should equalWithTolerance(0.0083988888889.asMegaWattHour)
            case DomesticHotWaterStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
              time shouldBe 42619.toDateTime
              qDot should equalWithTolerance(0.0.asMegaWatt)
              energy should equalWithTolerance(0.00149814.asMegaWattHour)
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(45000)))

      /* TICK 45078
     Storage will be empty
     House demand heating : requiredDemand = 8.87kWh, additionalDemand = 23.87 kWh
     House demand water   : tba
     ThermalStorage       : requiredDemand = 10.44 kWh, additionalDemand = 10.44 kWh
     DomesticWaterStorage : tba
     Heat pump: will be turned on - to serve the remaining heat demand of house (and refill storage later)
       */

      heatPumpAgent ! Activation(45078)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 45078.toDateTime
          hpResult.getP should equalWithTolerance(pRunningHp)
          hpResult.getQ should equalWithTolerance(
            qRunningHp
          )
      }

      // Results of 45000 for house and storage
      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 45000.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                18.7550144313187.asDegreeCelsius
              )
            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 45000.toDateTime
              qDot should equalWithTolerance(0.0.asMegaWatt)
              energy should equalWithTolerance(
                0.008398888888888886.asMegaWattHour
              )
            case DomesticHotWaterStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
              time shouldBe 45000.toDateTime
              qDot should equalWithTolerance((-0.01089500358271865).asMegaWatt)
              energy should equalWithTolerance(
                0.0014981399999999998.asMegaWattHour
              )
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(45078)))

      /* TICK 57600
      New weather data: it's getting warmer again
      House demand heating : requiredDemand = 0.00 kWh, additionalDemand = 1.70 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 10.44 kWh, additionalDemand = 10.44 kWh
      DomesticWaterStorage : tba
      Heat pump: stays on
       */

      heatPumpAgent ! Activation(57600)

      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          57600,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(4d),
            WattsPerSquareMeter(4d),
            Celsius(5d),
            MetersPerSecond(0d),
          ),
          Some(151200),
        )
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 57600.toDateTime
          hpResult.getP should equalWithTolerance(pRunningHp)
          hpResult.getQ should equalWithTolerance(qRunningHp)
      }

      // Results of 45540 for house and storage
      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 45078.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                18.7741518715941.asDegreeCelsius
              )

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 45078.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(0.00839888889.asMegaWattHour)
            case DomesticHotWaterStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
              time shouldBe 45078.toDateTime
              qDot should equalWithTolerance(0.0.asMegaWatt)
              energy should equalWithTolerance(0.001262081589.asMegaWattHour)
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }
      scheduler.expectMessage(Completion(heatPumpAgent, Some(57600)))

      /* TICK 57848
      ?
      House demand heating : requiredDemand = 0.00 kWh, additionalDemand = 0.00 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 10.44 kWh, additionalDemand = 10.44 kWh
      DomesticWaterStorage : tba
      Heat pump: ?
       */

      heatPumpAgent ! Activation(57848)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 57848.toDateTime
          hpResult.getP should equalWithTolerance(pRunningHp)
          hpResult.getQ should equalWithTolerance(qRunningHp)
      }

      // Results of 57600 for house and storage
      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 57600.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                21.8455595392382.asDegreeCelsius
              )
            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 57600.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(
                0.008398888889.asMegaWattHour
              )
            case DomesticHotWaterStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
              time shouldBe 57600.toDateTime
              qDot should equalWithTolerance((-0.010987050463985857).asMegaWatt)
              energy should equalWithTolerance(0.001262081589.asMegaWattHour)
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(57848)))

      /* TICK 58048
      House will reach the upperTemperatureBoundary
      House demand heating : requiredDemand = 0.00 kWh, additionalDemand = 0.00 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 10.44 kWh, additionalDemand = 10.44 kWh
      DomesticWaterStorage : tba
      Heat pump: stays on to refill the storage now
       */

      heatPumpAgent ! Activation(58048)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 58048.toDateTime
          hpResult.getP should equalWithTolerance(pRunningHp)
          hpResult.getQ should equalWithTolerance(qRunningHp)
      }

      // Results of 57848 for house and storage
      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 57848.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                21.93112361788363.asDegreeCelsius
              )
            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 57848.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(
                0.008398888889.asMegaWattHour
              )
            case DomesticHotWaterStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
              time shouldBe 57848.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(0.00050519589041.asMegaWattHour)
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(58048)))

      /* TICK 58716
     Storage will be fully charged
     House demand heating : requiredDemand = ?0.00 kWh, additionalDemand = 0.00 kWh
     House demand water   : tba
     ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
     DomesticWaterStorage : tba
     Heat pump: stays on to refill the storage now
       */

      heatPumpAgent ! Activation(58716)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 58716.toDateTime
          hpResult.getP should equalWithTolerance(0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.asMegaVar)
      }

      // Results of 58048 for house and storage
      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 58048.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                22.0000635263.asDegreeCelsius
              )

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 58048.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              energy should equalWithTolerance(
                0.0083988888889.asMegaWattHour
              )
            case DomesticHotWaterStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
              time shouldBe 58048.toDateTime
              qDot should equalWithTolerance(0.0.asMegaWatt)
              energy should equalWithTolerance(0.00050519589041.asMegaWattHour)
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(58716)))
    }
  }
}
