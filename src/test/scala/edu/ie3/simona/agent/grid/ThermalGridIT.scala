/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.hp.HpAgent
import edu.ie3.simona.agent.participant.load.LoadAgent.FixedLoadAgent
import edu.ie3.simona.agent.participant.pv.PvAgent
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.config.SimonaConfig.{
  EmRuntimeConfig,
  HpRuntimeConfig,
  LoadRuntimeConfig,
  PvRuntimeConfig,
}
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.{
  CylindricalThermalStorageResult,
  EmResult,
  HpResult,
  ParticipantResultEvent,
  ThermalHouseResult,
}
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.model.thermal.ThermalHouseTestData
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
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

/** Test to ensure the functions that a thermal grid and its connected assets is
  * capable.
  */
class ThermalGridIT
    extends ScalaTestWithActorTestKit
    with ThermalHouseTestData
    with AnyWordSpecLike
    with should.Matchers
    with EmInputTestData
    with MockitoSugar
    with DefaultTestData {
  private implicit val classicSystem: ActorSystem = system.toClassic

  private val resolution =
    simonaConfig.simona.powerflow.resolution.getSeconds

  private val outputConfigOn = NotifierConfig(
    simulationResultInfo = true,
    powerRequestReply = false,
    flexResult = false,
  )

  private val outputConfigOff = NotifierConfig(
    simulationResultInfo = false,
    powerRequestReply = false,
    flexResult = false,
  )

  "A Thermal Grid with thermal house, storage and heat pump not under the control of energy management" should {
    "be initialized correctly and run through some activations" in {
      implicit val simulationStartDate: ZonedDateTime =
        TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z")
      val simulationEndDate: ZonedDateTime =
        TimeUtil.withDefaults.toZonedDateTime("2020-01-02T02:00:00Z")

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
        Some(0),
      )
      val weatherDependentAgents = Seq(heatPumpAgent)

      scheduler.expectMessage(Completion(heatPumpAgent, Some(0)))

      /* TICK 0
      Start of Simulation
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand ~ 15 kWh
      ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
      Heat pump: turned on - to serve the storage demand
       */

      heatPumpAgent ! Activation(0)

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
              time shouldBe 0.toDateTime
              qDot should equalWithTolerance(0.0.asMegaWatt)
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
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(3417)))

      /* TICK 3417
      Storage is fully heated up
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 17.37 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays on since it was on and the house has possible demand
       */

      heatPumpAgent ! Activation(3417)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 3417.toDateTime
          hpResult.getP should equalWithTolerance(pRunningHp)
          hpResult.getQ should equalWithTolerance(
            qRunningHp
          )
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
              time shouldBe 3417.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                19.6835196903292.asDegreeCelsius
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
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(7200)))

      /* TICK 7200
      New weather data (unchanged) incoming
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 8.41 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays on, we got triggered by incoming weather data. So we continue with same behaviour as before
       */

      heatPumpAgent ! Activation(7200)

      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          7200,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(1d),
            WattsPerSquareMeter(1d),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(28800),
        )
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 7200.toDateTime
          hpResult.getP should equalWithTolerance(pRunningHp)
          hpResult.getQ should equalWithTolerance(
            qRunningHp
          )
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
              time shouldBe 7200.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                20.8788983755569.asDegreeCelsius
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
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(10798)))

      /* TICK 10798
      House reaches upper temperature boundary
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: turned off
       */

      heatPumpAgent ! Activation(10798)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 10798.toDateTime
          hpResult.getP should equalWithTolerance(0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.asMegaVar)
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
              time shouldBe 10798.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                21.9998899446115.asDegreeCelsius
              )
            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 10798.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(0.01044.asMegaWattHour)
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(28800)))

      /* TICK 28800
      House would reach lowerTempBoundary at tick 50797
      but now it's getting colder which should decrease inner temp of house faster
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays off
       */

      heatPumpAgent ! Activation(28800)

      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          28800,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(2d),
            WattsPerSquareMeter(2d),
            Celsius(-25d),
            MetersPerSecond(0d),
          ),
          Some(45000),
        )
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 28800.toDateTime
          hpResult.getP should equalWithTolerance(0.0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.0.asMegaVar)
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
              time shouldBe 28800.toDateTime
              qDot should equalWithTolerance(0.0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                20.19969728245267.asDegreeCelsius
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
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(41940)))

      /* TICK 41940
      House reach lowerTemperatureBoundary
      House demand heating : requiredDemand = 15.0 kWh, possibleDemand = 30.00 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays off, demand should be covered by storage
       */

      heatPumpAgent ! Activation(41940)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 41940.toDateTime
          hpResult.getP should equalWithTolerance(0.0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.0.asMegaVar)
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
              time shouldBe 41940.toDateTime
              qDot should equalWithTolerance(0.01044.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                17.9999786813733.asDegreeCelsius
              )
            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 41940.toDateTime
              qDot should equalWithTolerance(-0.01044.asMegaWatt)
              energy should equalWithTolerance(0.01044.asMegaWattHour)
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(45000)))

      /* TICK 45000
      Storage will be empty at tick 45540
      Additional trigger caused by (unchanged) weather data should not change this
      House demand heating : requiredDemand = 9.78 kWh, possibleDemand = 24.78 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 8.87 kWh
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
          hpResult.getP should equalWithTolerance(0.0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.0.asMegaVar)
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
              time shouldBe 45000.toDateTime
              qDot should equalWithTolerance(0.01044.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                18.69584558965105.asDegreeCelsius
              )

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 45000.toDateTime
              qDot should equalWithTolerance(-0.01044.asMegaWatt)
              energy should equalWithTolerance(
                0.00156599999999999.asMegaWattHour
              )
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(45540)))

      /* TICK 45540
      Storage will be empty
      House demand heating : requiredDemand = 8.87kWh, possibleDemand = 23.87 kWh
      ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
      DomesticWaterStorage : tba
      Heat pump: will be turned on - to serve the remaining heat demand of house (and refill storage later)
       */

      heatPumpAgent ! Activation(45540)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 45540.toDateTime
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
              time shouldBe 45540.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                18.81725389847177.asDegreeCelsius
              )

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 45540.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(0.asMegaWattHour)
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(57600)))

      /* TICK 57600
      New weather data: it's getting warmer again
      House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 1.70 kWh
      ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
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
              time shouldBe 57600.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                21.77341655767336.asDegreeCelsius
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
              energy should equalWithTolerance(0.asMegaWattHour)
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(58256)))

      /* TICK 58256
      House will reach the upperTemperatureBoundary
      House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 0.00 kWh
      ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
      Heat pump: stays on to refill the storage now
       */

      heatPumpAgent ! Activation(58256)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 58256.toDateTime
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
              time shouldBe 58256.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                21.999922627074.asDegreeCelsius
              )

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 58256.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              energy should equalWithTolerance(
                0.asMegaWattHour
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(61673)))

      /* TICK 61673
      Storage will be fully charged
      House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 0.00 kWh
      ThermalStorage       : requiredDemand = 0.00 kWh, possibleDemand = 0.00 kWh
      Heat pump: turned off
       */

      heatPumpAgent ! Activation(61673)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 61673.toDateTime
          hpResult.getP should equalWithTolerance(0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.asMegaVar)
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
              time shouldBe 61673.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                21.7847791618269.asDegreeCelsius
              )

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 61673.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(
                0.01044.asMegaWattHour
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(122555)))

    }
  }

  "A Thermal Grid with thermal house, thermal storage and heat pump that is controlled by an energy management" should {
    "be initialized correctly and run through some activations" in {
      implicit val simulationStartWithPv: ZonedDateTime =
        TimeUtil.withDefaults.toZonedDateTime("2020-06-01T10:00:00Z")
      val simulationEndWithPv: ZonedDateTime =
        TimeUtil.withDefaults.toZonedDateTime("2020-06-02T10:00:00Z")

      val scheduler: TestProbe[SchedulerMessage] = TestProbe("scheduler")
      val primaryServiceProxy =
        TestProbe[ServiceMessage]("PrimaryServiceProxy")

      val weatherService = TestProbe[ServiceMessage]("WeatherService")

      val resultListener: TestProbe[ResultEvent] = TestProbe("resultListener")

      val modelConfig: EmRuntimeConfig = EmRuntimeConfig(
        calculateMissingReactivePowerWithModel = false,
        scaling = 1d,
        uuids = List("default"),
        aggregateFlex = "SELF_OPT",
        curtailRegenerative = false,
      )

      val emAgent = spawn(
        EmAgent(
          emInput,
          modelConfig,
          outputConfigOn,
          "PRIORITIZED",
          simulationStartWithPv,
          parent = Left(scheduler.ref),
          listener = Iterable(resultListener.ref),
        ),
        "EmAgent",
      )
      val pvAgent = TestActorRef(
        new PvAgent(
          scheduler = scheduler.ref.toClassic,
          initStateData = ParticipantInitializeStateData(
            pvInput2,
            PvRuntimeConfig(
              calculateMissingReactivePowerWithModel = true,
              scaling = 2d,
              uuids = List.empty,
            ),
            primaryServiceProxy.ref.toClassic,
            Iterable(ActorWeatherService(weatherService.ref.toClassic)),
            simulationStartWithPv,
            simulationEndWithPv,
            resolution,
            simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
            outputConfigOff,
            Some(emAgent),
          ),
          listener = Iterable(resultListener.ref.toClassic),
        ),
        "PvAgent",
      )

      val loadAgent = TestActorRef(
        new FixedLoadAgent(
          scheduler = scheduler.ref.toClassic,
          initStateData = ParticipantInitializeStateData(
            loadInput,
            LoadRuntimeConfig(
              calculateMissingReactivePowerWithModel = true,
              scaling = 1d,
              modelBehaviour = "fix",
              reference = "power",
              uuids = List.empty,
            ),
            primaryServiceProxy.ref.toClassic,
            None,
            simulationStartWithPv,
            simulationEndWithPv,
            resolution,
            simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
            outputConfigOff,
            Some(emAgent),
          ),
          listener = Iterable(resultListener.ref.toClassic),
        ),
        "LoadAgent",
      )

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
            simulationStartWithPv,
            simulationEndWithPv,
            resolution,
            simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
            outputConfigOn,
            Some(emAgent),
          ),
          listener = Iterable(resultListener.ref.toClassic),
        ),
        "HeatPumpAgent1",
      )

      val pRunningHp = 0.0038.asMegaWatt
      val qRunningHp = 0.0012489995996796802.asMegaVar

      scheduler.expectNoMessage()

      /* INIT */

      // load
      loadAgent ! Activation(INIT_SIM_TICK)

      primaryServiceProxy.expectMessage(
        PrimaryServiceRegistrationMessage(loadInput.getUuid)
      )
      loadAgent ! RegistrationFailedMessage(primaryServiceProxy.ref.toClassic)

      // the order of the two messages is not given
      val emAgentActivation = scheduler
        .receiveMessages(2)
        .flatMap {
          case Completion(ref, maybeNewTick) =>
            ref shouldBe loadAgent.toTyped
            maybeNewTick shouldBe None
            None
          case ScheduleActivation(ref, tick, unlockKey) =>
            // em agent schedules itself
            tick shouldBe 0
            unlockKey shouldBe None
            Some(ref)
          case unexpected =>
            fail(s"Received unexpected message $unexpected")
          // TODO clarify RuntimeException required / fix message
        }
        .headOption
        .getOrElse(throw new RuntimeException("RuntimeException"))

      // pv
      pvAgent ! Activation(INIT_SIM_TICK)

      primaryServiceProxy.expectMessage(
        PrimaryServiceRegistrationMessage(pvInput2.getUuid)
      )
      pvAgent ! RegistrationFailedMessage(primaryServiceProxy.ref.toClassic)

      // deal with weather service registration
      weatherService.expectMessage(
        RegisterForWeatherMessage(
          pvInput2.getNode.getGeoPosition.getY,
          pvInput2.getNode.getGeoPosition.getX,
        )
      )

      pvAgent ! RegistrationSuccessfulMessage(
        weatherService.ref.toClassic,
        Some(0L),
      )

      scheduler.expectMessage(Completion(pvAgent))

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
      val weatherDependentAgents = Seq(heatPumpAgent, pvAgent)

      /* TICK 0
      Start of Simulation
      House demand heating : requiredDemand = 0.0 kWh, additionalDemand ~ 15 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 10.44 kWh, additionalDemand = 10.44 kWh
      DomesticWaterStorage : tba
      Heat pump: turned on - to serve the storage demand
       */

      /* TICK 0
       LOAD: 0.269 kW
       PV:  -5.617 kW
       STORAGE: SOC 0 %
       -> charge with 5 kW
       -> remaining -0.348 kW
       */

      emAgentActivation ! Activation(0)

      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          0,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(540d),
            WattsPerSquareMeter(400d),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(7200),
        )
      }

      Range(0, 1)
        .map { _ =>
          resultListener.expectMessageType[ParticipantResultEvent]
        }
        .foreach { case ParticipantResultEvent(participantResult) =>
          println(participantResult)
          participantResult match {
            case HpResult(hpResult) =>
              hpResult._2 shouldBe typicalHpInputModel.getUuid
              hpResult._1 shouldBe 0.toDateTime
              hpResult._3 should equalWithTolerance(pRunningHp)
              hpResult._4 should equalWithTolerance(
                qRunningHp
              )
          }
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
              time shouldBe 0.toDateTime
              qDot should equalWithTolerance(0.0.asMegaWatt)
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
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, None))
      scheduler.expectMessage(Completion(emAgentActivation, Some(3417)))

      /* TICK 3417
      Storage is fully heated up
      House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 17.37 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      DomesticWaterStorage : tba
      Heat pump: stays on since it was on and the house as additional demand
       */

      emAgentActivation ! Activation(3417)

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ParticipantResultEvent]
        }
        .foreach { case ParticipantResultEvent(participantResult) =>
          println(participantResult)
          participantResult match {
            case HpResult(hpResult) =>
              hpResult._2 shouldBe typicalHpInputModel.getUuid
              hpResult._1 shouldBe 3417.toDateTime
              hpResult._3 should equalWithTolerance(pRunningHp)
              hpResult._4 should equalWithTolerance(
                qRunningHp
              )
            case EmResult(emResult) =>
              emResult._2 shouldBe emInput.getUuid
              emResult._1 shouldBe 0.toDateTime
              emResult._3 should equalWithTolerance(-0.00802906873.asMegaWatt)
              emResult._4 should equalWithTolerance(0.00133728514.asMegaVar)
          }
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
              time shouldBe 3417.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                19.6835196903292.asDegreeCelsius
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
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, None))
      scheduler.expectMessage(Completion(emAgentActivation, Some(7200)))

      /* TICK 7200
      New weather data (unchanged) incoming
      House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 8.41 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      DomesticWaterStorage : tba
      Heat pump: stays on
       */

      emAgentActivation ! Activation(7200L)

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

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ParticipantResultEvent]
        }
        .foreach { case ParticipantResultEvent(participantResult) =>
          println(participantResult)
          participantResult match {
            case HpResult(hpResult) =>
              hpResult._2 shouldBe typicalHpInputModel.getUuid
              hpResult._1 shouldBe 7200.toDateTime
              hpResult._3 should equalWithTolerance(pRunningHp)
              hpResult._4 should equalWithTolerance(
                qRunningHp
              )
            case EmResult(emResult) =>
              emResult._2 shouldBe emInput.getUuid
              emResult._1 shouldBe 3417.toDateTime
              emResult._3 should equalWithTolerance(-0.00802906873.asMegaWatt)
              emResult._4 should equalWithTolerance(0.00133728514.asMegaVar)
          }
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
              time shouldBe 7200.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                20.8788983755569.asDegreeCelsius
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
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(7200L)))

      /* TICK 10798
      House reaches upper temperature boundary
      House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      DomesticWaterStorage : tba
      Heat pump: turned off
       */

      heatPumpAgent ! Activation(10798)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 10798.toDateTime
          hpResult.getP should equalWithTolerance(0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.asMegaVar)
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
              time shouldBe 10798.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                21.9998899446115.asDegreeCelsius
              )
            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 10798.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(0.01044.asMegaWattHour)
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(10798)))

    }
  }
}
