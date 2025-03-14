/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.hp.HpAgent
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.agent.participant2.ParticipantAgent.{
  DataProvision,
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.agent.participant2.ParticipantAgentInit
import edu.ie3.simona.agent.participant2.ParticipantAgentInit.{
  ParticipantRefs,
  SimulationParameters,
}
import edu.ie3.simona.config.RuntimeConfig.{HpRuntimeConfig, PvRuntimeConfig}
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent._
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.model.thermal.ThermalHouseTestData
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.WeatherMessage.{
  RegisterForWeatherMessage,
  WeatherData,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.service.ServiceType
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
import squants.Each
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
  protected implicit val temperatureTolerance = 0.01
  protected val simulationEndDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-02T02:00:00Z")

  private val resolution =
    simonaConfig.simona.powerflow.resolution.toSeconds

  private val outputConfigOn = NotifierConfig(
    simulationResultInfo = true,
    powerRequestReply = false,
    flexResult = false,
  )

  "A Thermal Grid with thermal house, storage and heat pump not under the control of energy management" should {
    "be initialized correctly and run through some activations" in {
      val scheduler: TestProbe[SchedulerMessage] = TestProbe("scheduler")
      val primaryServiceProxy =
        TestProbe[ServiceMessage]("PrimaryServiceProxy")

      val weatherService = TestProbe[ServiceMessage]("WeatherService")

      val resultListener: TestProbe[ResultEvent] = TestProbe("resultListener")

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
        PrimaryServiceRegistrationMessage(
          heatPumpAgent.ref,
          typicalHpInputModel.getUuid,
        )
      )
      heatPumpAgent ! RegistrationFailedMessage(
        primaryServiceProxy.ref.toClassic
      )

      weatherService.expectMessage(
        RegisterForWeatherMessage(
          heatPumpAgent.ref,
          typicalHpInputModel.getNode.getGeoPosition.getY,
          typicalHpInputModel.getNode.getGeoPosition.getX,
        )
      )

      heatPumpAgent ! RegistrationSuccessfulMessage(
        weatherService.ref.toClassic,
        0,
      )
      val weatherDependentAgents = Seq(heatPumpAgent)

      scheduler.expectMessage(Completion(heatPumpAgent, Some(0)))

      /* TICK 0
      Start of Simulation
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      House demand water   : requiredDemand ~ 0.0674 kWh, possibleDemand ~ 0.067 kWh
      ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: turned on - to serve the storage demand
       */

      heatPumpAgent ! Activation(0)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          0,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(3600),
        )
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 0.toDateTime
          hpResult.getP should equalWithTolerance(pRunningHp)
          hpResult.getQ should equalWithTolerance(qRunningHp)
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
              time shouldBe 0.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                19.99.asDegreeCelsius
              )(temperatureTolerance)
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

      scheduler.expectMessage(Completion(heatPumpAgent, Some(23)))

      /* TICK 23
      Domestic hot water storage stops discharging
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 15.02 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 10.37 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays on since it was on and the house has possible demand
       */

      heatPumpAgent ! Activation(23)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 23.toDateTime
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

      scheduler.expectMessage(Completion(heatPumpAgent, Some(3416)))

      /* TICK 3416
      ThermalStorage is fully heated up
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 2.37 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays on since it was on and the house has possible demand
       */

      heatPumpAgent ! Activation(3416)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 3416.toDateTime
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
              time shouldBe 3416.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                19.68.asDegreeCelsius
              )(temperatureTolerance)

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 3416.toDateTime
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

      scheduler.expectMessage(Completion(heatPumpAgent, Some(3600)))

      /* TICK 3600
      New weather data (unchanged) incoming + Domestic hot water storage will cover hot water demand
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.94 kWh
      House demand water   : requiredDemand = 0.06 kWh, possibleDemand = 0.06 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays on, we got triggered by incoming weather data. So we continue with same behaviour as before
       */

      heatPumpAgent ! Activation(3600)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          3600,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(1d),
            WattsPerSquareMeter(1d),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(21600),
        )
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 3600.toDateTime
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
              time shouldBe 3600.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                19.74.asDegreeCelsius
              )(temperatureTolerance)
            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 3600.toDateTime
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

      scheduler.expectMessage(Completion(heatPumpAgent, Some(7220)))

      /* TICK 7220
      Domestic hot water storage stops discharging
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 8.41 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays on
       */

      heatPumpAgent ! Activation(7220L)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 7220.toDateTime
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

      scheduler.expectMessage(Completion(heatPumpAgent, Some(4417)))

      /* TICK 4419
      House reaches upper temperature boundary
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: turned off
       */

      heatPumpAgent ! Activation(4417)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 4417.toDateTime
          hpResult.getP should equalWithTolerance(0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.asMegaVar)
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
              time shouldBe 4417.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                19.99.asDegreeCelsius
              )(temperatureTolerance)
            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 4417.toDateTime
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

      scheduler.expectMessage(Completion(heatPumpAgent, Some(21600)))

      /* TICK 21600
      House would reach lowerTempBoundary at tick 50797.
      But now it's getting colder which should decrease inner temp of house faster.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 11.9 kWh
      House demand water   : requiredDemand = 0.57 kWh, possibleDemand = 0.57 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays off
       */

      heatPumpAgent ! Activation(21600)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          21600,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(2d),
            WattsPerSquareMeter(2d),
            Celsius(-55d),
            MetersPerSecond(0d),
          ),
          Some(25000),
        )
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 21600.toDateTime
          hpResult.getP should equalWithTolerance(0.0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.0.asMegaVar)
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
              time shouldBe 21600.toDateTime
              qDot should equalWithTolerance(0.0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                18.40.asDegreeCelsius
              )(temperatureTolerance)

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 21600.toDateTime
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

      /* TICK 28987
      Domestic hot water storage will stop discharging
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.00 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays off
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

      scheduler.expectMessage(Completion(heatPumpAgent, Some(23103)))

      /* TICK 23103
      House reach lowerTemperatureBoundary
      House demand heating : requiredDemand = 15.0 kWh, possibleDemand = 15.00 kWh
      House demand water   : requiredDemand = 0.67 kWh, possibleDemand = 0.67 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays off, demand should be covered by storage
       */

      heatPumpAgent ! Activation(23103)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 23103.toDateTime
          hpResult.getP should equalWithTolerance(0.0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.0.asMegaVar)
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
              time shouldBe 23103.toDateTime
              qDot should equalWithTolerance(0.01044.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                18.00.asDegreeCelsius
              )(temperatureTolerance)

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 23103.toDateTime
              qDot should equalWithTolerance(-0.01044.asMegaWatt)
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

      /* TICK 42171
      Domestic hot water storage will stop discharging, and its SOC will be less than 20%, thus it need to be recharged,
      heat demand of house will still be covered from thermal storage
      House demand heating : requiredDemand = 14.59 kWh, possibleDemand = 29.59 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.67 kWh
      DomesticWaterStorage : requiredDemand = 1.37 kWh, possibleDemand = 1.37 kWh
      Heat pump: turns on to cover demand of domestic hot water storage
       */

      heatPumpAgent ! Activation(42171)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 42171.toDateTime
          hpResult.getP should equalWithTolerance(pRunningHp)
          hpResult.getQ should equalWithTolerance(qRunningHp)
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

      /* TICK 42619
      Domestic hot water storage will be full
      House demand heating : requiredDemand = 13.76 kWh, possibleDemand = 28.76 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 2.04 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: Since Hp is running, it will kept running to heat house and recharge thermal storage
       */

      heatPumpAgent ! Activation(42619)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 42619.toDateTime
          hpResult.getP should equalWithTolerance(pRunningHp)
          hpResult.getQ should equalWithTolerance(0.00124899959968.asMegaVar)
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

      scheduler.expectMessage(Completion(heatPumpAgent, Some(25000)))

      /* TICK 25000
        Storage will be empty at tick 26705
        FIXME
        Additional trigger caused by (unchanged) weather data should not change this
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 13.35 kWh
        House demand water   : requiredDemand = 0.24 kWh, possibleDemand = 0.24 kWh
        ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 5.50 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        Heat pump: stays off
       */

      heatPumpAgent ! Activation(25000)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          25000,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(3d),
            WattsPerSquareMeter(3d),
            Celsius(-55d),
            MetersPerSecond(0d),
          ),
          Some(28000),
        )
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 25000.toDateTime
          hpResult.getP should equalWithTolerance(pRunningHp)
          hpResult.getQ should equalWithTolerance(qRunningHp)
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
              time shouldBe 25000.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                18.22.asDegreeCelsius
              )(temperatureTolerance)

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 25000.toDateTime
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
                0.0049387.asMegaWattHour
              )
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(26702)))

      /* TICK 26702
        //FIXME?
    DomesticWaterStorage will stop discharging to cover water demand
        House demand heating : requiredDemand = 0.0kWh, possibleDemand = 11.87 kWh
       House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
     ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 2.04 kWh
     DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        Heat pump: kept running - to serve the remaining heat demand of house (and refill storage later)
       */

      heatPumpAgent ! Activation(26702)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 26702.toDateTime
          hpResult.getP should equalWithTolerance(pRunningHp)
          hpResult.getQ should equalWithTolerance(qRunningHp)
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
              time shouldBe 26702.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                18.41.asDegreeCelsius
              )(temperatureTolerance)

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 26702.toDateTime
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

      scheduler.expectMessage(Completion(heatPumpAgent, Some(28000)))

      /* TICK 28000
        New weather data: it's getting warmer again
        FIXME
        House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 10.55 kWh
        House demand water   : requiredDemand = 0.76 kWh, possibleDemand = 0.76 kWh
        ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        Heat pump: stays on
       */

      heatPumpAgent ! Activation(28000)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          28000,
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
          hpResult.getTime shouldBe 28000.toDateTime
          hpResult.getP should equalWithTolerance(pRunningHp)
          hpResult.getQ should equalWithTolerance(qRunningHp)
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
              time shouldBe 28000.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                18.59.asDegreeCelsius
              )(temperatureTolerance)
            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 28000.toDateTime
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

      scheduler.expectMessage(Completion(heatPumpAgent, Some(31940)))

      /* TICK 31940
        House will reach the upperTemperatureBoundary
        House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 0.00 kWh
        ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        Heat pump: stays on to recharge the storage now
       */
      /* TICK 57848
      FIXME
DomesticWaterStorage will stop discharging to cover water demand
House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 0.52 kWh
House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 2.04 kWh
DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
Heat pump: ?
 */

      heatPumpAgent ! Activation(31940)
      heatPumpAgent ! Activation(58048)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 31940.toDateTime
          hpResult.getP should equalWithTolerance(pRunningHp)
          hpResult.getQ should equalWithTolerance(qRunningHp)
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
              time shouldBe 31940.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                19.99.asDegreeCelsius
              )(temperatureTolerance)

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 31940.toDateTime
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

      scheduler.expectMessage(Completion(heatPumpAgent, Some(35356)))

      /* TICK 35356
        Storage will be fully charged, but meanwhile the house cooled a bit
        House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 1.42 kWh
        ThermalStorage       : requiredDemand = 0.00 kWh, possibleDemand = 0.00 kWh
        Heat pump: stays on
       */

      /* TICK 58716
     Storage will be fully charged
     House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
     House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
     ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
     DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
     Heat pump: turns off, all demands are covered
       */

      heatPumpAgent ! Activation(35356)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 35356.toDateTime
          hpResult.getP should equalWithTolerance(pRunningHp)
          hpResult.getQ should equalWithTolerance(qRunningHp)
      }

      Range(0, 2)
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
              time shouldBe 35356.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                19.81.asDegreeCelsius
              )(temperatureTolerance)

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 35356.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(
                0.01044.asMegaWattHour
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(35894)))

      /* TICK 35894
      Neither house nor storage have any demand
      House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 0.00 kWh
      ThermalStorage       : requiredDemand = 0.00 kWh, possibleDemand = 0.00 kWh
      Heat pump: turned off
       */

      heatPumpAgent ! Activation(35894)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 35894.toDateTime
          hpResult.getP should equalWithTolerance(0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.asMegaVar)
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
              time shouldBe 35894.toDateTime
              qDot should equalWithTolerance(0.0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                19.99.asDegreeCelsius
              )(temperatureTolerance)

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 35894.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(
                0.01044.asMegaWattHour
              )
            case DomesticHotWaterStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
              time shouldBe 58716.toDateTime
              qDot should equalWithTolerance(0.0.asMegaWatt)
              energy should equalWithTolerance(0.00050519589041.asMegaWattHour)
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(71891)))

    }
  }
}
