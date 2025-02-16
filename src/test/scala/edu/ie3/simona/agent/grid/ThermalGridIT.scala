/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.hp.HpAgent
import edu.ie3.simona.agent.participant.pv.PvAgent
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.config.SimonaConfig.{
  EmRuntimeConfig,
  HpRuntimeConfig,
  PvRuntimeConfig,
}
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
import scala.concurrent.duration.{FiniteDuration, SECONDS}
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
        TimeUtil.withDefaults.toZonedDateTime("2020-06-12T10:00:00Z")

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
            // FIXME replace pvInput2 against pvInput when the other PR is merged
            pvInput2,
            PvRuntimeConfig(
              calculateMissingReactivePowerWithModel = true,
              scaling = 1d,
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

      val heatPumpAgentWithEm = TestActorRef(
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
        "HeatPumpAgentWithEm",
      )

      val pRunningHp = 0.0038.asMegaWatt
      val qRunningHp = 0.0012489995996796802.asMegaVar

      scheduler.expectNoMessage()

      /* INIT */

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
      heatPumpAgentWithEm ! Activation(INIT_SIM_TICK)

      primaryServiceProxy.expectMessage(
        PrimaryServiceRegistrationMessage(typicalHpInputModel.getUuid)
      )
      heatPumpAgentWithEm ! RegistrationFailedMessage(
        primaryServiceProxy.ref.toClassic
      )

      weatherService.expectMessage(
        RegisterForWeatherMessage(
          typicalHpInputModel.getNode.getGeoPosition.getY,
          typicalHpInputModel.getNode.getGeoPosition.getX,
        )
      )

      heatPumpAgentWithEm ! RegistrationSuccessfulMessage(
        weatherService.ref.toClassic,
        Some(0L),
      )

      // the order of the two messages is not given
      val emAgentActivation = scheduler
        .receiveMessages(2)
        .flatMap {
          case Completion(ref, maybeNewTick) =>
            ref shouldBe heatPumpAgentWithEm.toTyped
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

      val weatherDependentAgents = Seq(heatPumpAgentWithEm, pvAgent)

      /* TICK 0
      Start of Simulation, No sun at the moment
      House demand heating : requiredDemand = 0.0 kWh, additionalDemand 0.0 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 10.44 kWh, additionalDemand = 10.44 kWh
      DomesticWaterStorage : tba
      Heat pump: stays out - since requiredDemand of ThermalStorage not necessarily demand hp operation
       */

      // FIXME
      /* TICK 0
       PV:  -5.617 kW
       -> charge with 5 kW
       -> remaining -0.348 kW
       */

      emAgentActivation ! Activation(0)

      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          0,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(0),
            WattsPerSquareMeter(0),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(1800),
        )
      }

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 0.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 0.toDateTime
                emResult._3 should equalWithTolerance(0.asMegaWatt)
                emResult._4 should equalWithTolerance(0.asMegaVar)
            }
          case ThermalResultEvent(thermalUnitResult) =>
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
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(1800)))

      /* TICK 1800
    New Weather: The sun comes out, PV will produce
    FIXME
    House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 17.37 kWh
    House demand water   : tba
    ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
    DomesticWaterStorage : tba
    Heat pump: turns on - since now we have flexibility potential available which can be used by hp to serve the reqDemand of ThermalStorage
       */

      emAgentActivation ! Activation(1800)

      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          1800,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(540),
            WattsPerSquareMeter(400),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(5400),
        )
      }

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 1800.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 1800.toDateTime
                emResult._3 should equalWithTolerance(-0.00250470163.asMegaWatt)
                emResult._4 should equalWithTolerance(0.0012489996.asMegaVar)
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case ThermalHouseResult(
                    time,
                    inputModel,
                    qDot,
                    indoorTemperature,
                  ) =>
                inputModel shouldBe typicalThermalHouse.getUuid
                time shouldBe 1800.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.83324135802466.asDegreeCelsius
                )

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 1800.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                energy should equalWithTolerance(0.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(5217)))

      /* TICK 5217
      Storage is fully heated up
      FIXME
      House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 17.37 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      DomesticWaterStorage : tba
      Heat pump: stays on since it was on and the house as additional demand
       */

      emAgentActivation ! Activation(5217)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 5217.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 5217.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.0025047016300778495.asMegaWatt
                )
                emResult._4 should equalWithTolerance(0.0012489996.asMegaVar)
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case ThermalHouseResult(
                    time,
                    inputModel,
                    qDot,
                    indoorTemperature,
                  ) =>
                inputModel shouldBe typicalThermalHouse.getUuid
                time shouldBe 5217.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.51896289239367.asDegreeCelsius
                )

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 5217.toDateTime
                qDot should equalWithTolerance(0.0.asMegaWatt)
                energy should equalWithTolerance(0.01044.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(5400)))

      /* TICK 5400
      New weather data, sun is gone again, thus we should now heat the house by storage and only till targetTemperature
      House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 8.41 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      DomesticWaterStorage : tba
      Heat pump: turns off
       */

      emAgentActivation ! Activation(5400)

      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          5400,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(0),
            WattsPerSquareMeter(0),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(9200),
        )
      }

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 5400.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 5400.toDateTime
                emResult._3 should equalWithTolerance(0.asMegaWatt)
                emResult._4 should equalWithTolerance(0.asMegaVar)
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case ThermalHouseResult(
                    time,
                    inputModel,
                    qDot,
                    indoorTemperature,
                  ) =>
                inputModel shouldBe typicalThermalHouse.getUuid
                time shouldBe 5400.toDateTime
                qDot should equalWithTolerance(0.01044.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.5769000397666.asDegreeCelsius
                )

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 5400.toDateTime
                qDot should equalWithTolerance(-0.01044.asMegaWatt)
                energy should equalWithTolerance(0.01044.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(6831)))

      /* TICK 6831
     The house reaches target temperature
     House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
     House demand water   : tba
     ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
     DomesticWaterStorage : tba
     Heat pump: turned off
       */

      emAgentActivation ! Activation(6831)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 6831.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 6831.toDateTime
                emResult._3 should equalWithTolerance(0.0.asMegaWatt)
                emResult._4 should equalWithTolerance(0.0.asMegaVar)
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case ThermalHouseResult(
                    time,
                    inputModel,
                    qDot,
                    indoorTemperature,
                  ) =>
                inputModel shouldBe typicalThermalHouse.getUuid
                time shouldBe 6831.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.9999624695558.asDegreeCelsius
                )

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 6831.toDateTime
                qDot should equalWithTolerance(0.0.asMegaWatt)
                energy should equalWithTolerance(0.0062900999999.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(9200)))

      /* TICK 9200
     The sun is back again, storage first
     House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
     House demand water   : tba
     ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
     DomesticWaterStorage : tba
     Heat pump: turned off
       */

      // PV 5.155 kW

      emAgentActivation ! Activation(9200)

      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          9200,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(450),
            WattsPerSquareMeter(250),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(21600),
        )
      }

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 9200.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 9200.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.0013557965636002037.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  0.0012489995996796802.asMegaVar
                )
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case ThermalHouseResult(
                    time,
                    inputModel,
                    qDot,
                    indoorTemperature,
                  ) =>
                inputModel shouldBe typicalThermalHouse.getUuid
                time shouldBe 9200.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.78061094699885.asDegreeCelsius
                )

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 9200.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                energy should equalWithTolerance(
                  0.006290099999999.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(10558)))

      /* TICK 10558
      Storage is full, now heating the house first till target temperature then continuing till upper temperature boundary
      FIXME
      House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      DomesticWaterStorage : tba
      Heat pump: stays on
       */

      emAgentActivation ! Activation(10558)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 10558.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 10558.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.0013557965636002037.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  0.0012489995996796802.asMegaVar
                )
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case ThermalHouseResult(
                    time,
                    inputModel,
                    qDot,
                    indoorTemperature,
                  ) =>
                inputModel shouldBe typicalThermalHouse.getUuid
                time shouldBe 10558.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.6559736519395.asDegreeCelsius
                )

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 10558.toDateTime
                qDot should equalWithTolerance(0.0.asMegaWatt)
                energy should equalWithTolerance(0.01044.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(11646)))

      /* TICK 11646
House reaches again target temperature boundary
FIXME
House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
House demand water   : tba
ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
DomesticWaterStorage : tba
Heat pump: stays on
       */

      emAgentActivation ! Activation(11646)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 11646.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 11646.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.0013557965636002037.asMegaWatt
                )
                // FIXME: Include a case where Pv is producing, and Hp is not running, emResult should be p of Pv and q of Pv. Instead q is 0 - this is wrong, should be checked by EmAgentIT!
                emResult._4 should equalWithTolerance(
                  0.0012489995996796802.asMegaVar
                )
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case ThermalHouseResult(
                    time,
                    inputModel,
                    qDot,
                    indoorTemperature,
                  ) =>
                inputModel shouldBe typicalThermalHouse.getUuid
                time shouldBe 11646.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.99987846922355.asDegreeCelsius
                )

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 11646.toDateTime
                qDot should equalWithTolerance(0.0.asMegaWatt)
                energy should equalWithTolerance(0.01044.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(17999)))

      /* TICK 17999
House reaches upper temperature boundary
FIXME
House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
House demand water   : tba
ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
DomesticWaterStorage : tba
Heat pump: stays on
       */

      emAgentActivation ! Activation(17999)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 17999.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 17999.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.005155796563600204.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  0.asMegaVar
                )
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case ThermalHouseResult(
                    time,
                    inputModel,
                    qDot,
                    indoorTemperature,
                  ) =>
                inputModel shouldBe typicalThermalHouse.getUuid
                time shouldBe 17999.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  21.99989984731625.asDegreeCelsius
                )

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 17999.toDateTime
                qDot should equalWithTolerance(0.0.asMegaWatt)
                energy should equalWithTolerance(0.01044.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(21600)))

      /* TICK 21600
      FIXME
   House would reach lowerTempBoundary at tick 50797
   but now it's getting colder which should decrease inner temp of house faster, sun is also gone
   House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
   ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
   Heat pump: stays off
       */

      emAgentActivation ! Activation(21600)

      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          21600,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(-25d),
            MetersPerSecond(0d),
          ),
          Some(45000),
        )
      }

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 21600.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 21600.toDateTime
                emResult._3 should equalWithTolerance(
                  0.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  0.asMegaVar
                )
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case ThermalHouseResult(
                    time,
                    inputModel,
                    qDot,
                    indoorTemperature,
                  ) =>
                inputModel shouldBe typicalThermalHouse.getUuid
                time shouldBe 21600.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  21.6398011830563.asDegreeCelsius
                )

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
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(42671)))

      /* TICK 42671
    House reach lowerTemperatureBoundary
    FIXME
    House demand heating : requiredDemand = 15.0 kWh, possibleDemand = 30.00 kWh
    ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
    Heat pump: stays off, demand should be covered by storage
       */

      emAgentActivation ! Activation(42671)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 42671.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 42671.toDateTime
                emResult._3 should equalWithTolerance(
                  0.0.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  0.asMegaVar
                )
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case ThermalHouseResult(
                    time,
                    inputModel,
                    qDot,
                    indoorTemperature,
                  ) =>
                inputModel shouldBe typicalThermalHouse.getUuid
                time shouldBe 42671.toDateTime
                qDot should equalWithTolerance(0.01044.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  17.99999655072967.asDegreeCelsius
                )

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 42671.toDateTime
                qDot should equalWithTolerance(-0.01044.asMegaWatt)
                energy should equalWithTolerance(0.01044.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(45000)))

      /* TICK 45000
    Storage will be empty at tick 46271
    Additional trigger caused by (unchanged) weather data should not change this
    FIXME
    House demand heating : requiredDemand = 9.78 kWh, possibleDemand = 24.78 kWh
    ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 8.87 kWh
    Heat pump: stays off
       */

      emAgentActivation ! Activation(45000)

      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          45000,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(-25d),
            MetersPerSecond(0d),
          ),
          Some(57600),
        )
      }
      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 45000.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 45000.toDateTime
                emResult._3 should equalWithTolerance(
                  0.0.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  0.asMegaVar
                )
            }
          case ThermalResultEvent(thermalUnitResult) =>
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
                  18.52962843233465.asDegreeCelsius
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
                energy should equalWithTolerance(0.0036859.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(46271)))

      /* TICK 46271
    Storage will be empty, since we haven't surplus energy from pv this not necessarily requires hp to run.
    FIXME
    House demand heating : requiredDemand = 8.87kWh, possibleDemand = 23.87 kWh
    ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
    DomesticWaterStorage : tba
    Heat pump: will be turned on - to serve the remaining heat demand of house (and refill storage later)
       */

      emAgentActivation ! Activation(46271)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 46271.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 46271.toDateTime
                emResult._3 should equalWithTolerance(
                  0.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  0.asMegaVar
                )
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case ThermalHouseResult(
                    time,
                    inputModel,
                    qDot,
                    indoorTemperature,
                  ) =>
                inputModel shouldBe typicalThermalHouse.getUuid
                time shouldBe 46271.toDateTime
                qDot should equalWithTolerance(0.0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  18.81617007034394.asDegreeCelsius
                )

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 46271.toDateTime
                qDot should equalWithTolerance(0.0.asMegaWatt)
                energy should equalWithTolerance(0.0.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(51300)))

      /* TICK 51300
House reach lower temperature boundary
House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 1.70 kWh
ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
Heat pump: stays on
       */

      emAgentActivation ! Activation(51300)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 51300.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 51300.toDateTime
                emResult._3 should equalWithTolerance(
                  0.0038.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  0.0012489995996796802.asMegaVar
                )
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case ThermalHouseResult(
                    time,
                    inputModel,
                    qDot,
                    indoorTemperature,
                  ) =>
                inputModel shouldBe typicalThermalHouse.getUuid
                time shouldBe 51300.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  18.00005333225594.asDegreeCelsius
                )

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 51300.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(57600)))

      /* TICK 57600
    New weather data: it's getting warmer again
    Note: One could argue, that new weather should not change the operation of an agent (at least, if the weather did not change the flexOptions), but so far
    we don't check for this. Thus, the Hp will stop operation since it can be turn off (lower Temp < innerTemp < targetTemp && storage must not directly recharged)
    House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 1.70 kWh
    ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
    Heat pump: turned off
       */

      emAgentActivation ! Activation(57600)

      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          57600,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(5d),
            MetersPerSecond(0d),
          ),
          Some(151200),
        )
      }

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 57600.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 57600.toDateTime
                emResult._3 should equalWithTolerance(
                  0.0.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  0.asMegaVar
                )
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case ThermalHouseResult(
                    time,
                    inputModel,
                    qDot,
                    indoorTemperature,
                  ) =>
                inputModel shouldBe typicalThermalHouse.getUuid
                time shouldBe 57600.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.56338542116996.asDegreeCelsius
                )

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 57600.toDateTime
                qDot should equalWithTolerance(0.0.asMegaWatt)
                energy should equalWithTolerance(0.0.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(86585)))

      /* TICK 86585
    House will reach the lower temperature boundary
    House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 0.00 kWh
    ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
    Heat pump: stays on to refill the storage now
       */

      emAgentActivation ! Activation(86585)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 86585.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 86585.toDateTime
                emResult._3 should equalWithTolerance(
                  pRunningHp
                )
                emResult._4 should equalWithTolerance(
                  qRunningHp
                )
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case ThermalHouseResult(
                    time,
                    inputModel,
                    qDot,
                    indoorTemperature,
                  ) =>
                inputModel shouldBe typicalThermalHouse.getUuid
                time shouldBe 86585.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  17.9999790269751.asDegreeCelsius
                )

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 86585.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(92152)))

      /* TICK 92152
House will reach target temperature
House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 0.00 kWh
ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
Heat pump: stays on to refill the storage now
       */

      emAgentActivation ! Activation(92152)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 92152.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 92152.toDateTime
                emResult._3 should equalWithTolerance(
                  0.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  0.asMegaVar
                )
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case ThermalHouseResult(
                    time,
                    inputModel,
                    qDot,
                    indoorTemperature,
                  ) =>
                inputModel shouldBe typicalThermalHouse.getUuid
                time shouldBe 92152.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.9999757557041.asDegreeCelsius
                )

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 92152.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(128152)))

    }
  }
}
