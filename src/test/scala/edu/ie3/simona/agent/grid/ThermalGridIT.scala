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
        Some(0L),
      )
      val weatherDependentAgents = Seq(heatPumpAgent)

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
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(0L)))

      /* TICK 3417
      Storage is fully heated up
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
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      // FIXME? Why next tick 3417?
      scheduler.expectMessage(Completion(heatPumpAgent, Some(3417)))

      /* TICK 7200
      New weather data (unchanged) incoming
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

      // FIXME? Why next tick 7200?
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

      // Results of 10798 for hp
      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 10798.toDateTime
          hpResult.getP should equalWithTolerance(0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.asMegaVar)
      }

      // Results of 7200 for house and storage
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

      // FIXME? Why next tick 10798?
      scheduler.expectMessage(Completion(heatPumpAgent, Some(10798)))

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

      scheduler.expectMessage(Completion(heatPumpAgent, Some(28800L)))

      /* TICK 41940
      House reach lowerTemperatureBoundary
      House demand heating : requiredDemand = 15.0 kWh, additionalDemand = 30.00 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      DomesticWaterStorage : tba
      Heat pump: stays off, demand should be covered by storage
       */

      heatPumpAgent ! Activation(41940)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 41940.toDateTime
          hpResult.getP should equalWithTolerance(0.0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(
            0.0.asMegaVar
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

      /* TICK 45000
      Storage will be empty at tick 45540
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
          hpResult.getP should equalWithTolerance(0.0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.0.asMegaVar)
      }

      // Results of 34132 for house and storage
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
              qDot should equalWithTolerance((-0.01044).asMegaWatt)
              energy should equalWithTolerance(0.01044.asMegaWattHour)
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(45000)))

      /* TICK 45540
     Storage will be empty
     House demand heating : requiredDemand = 8.87kWh, additionalDemand = 23.87 kWh
     House demand water   : tba
     ThermalStorage       : requiredDemand = 10.44 kWh, additionalDemand = 10.44 kWh
     DomesticWaterStorage : tba
     Heat pump: will be turned on - to serve the remaining heat demand of house (and refill storage later)
       */

      heatPumpAgent ! Activation(45540)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 45540.toDateTime
          hpResult.getP should equalWithTolerance(pRunningHp)
          hpResult.getQ should equalWithTolerance(
            qRunningHp
          )
      }

      // Results of 45000 for house and storage
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
              qDot should equalWithTolerance((-0.01044).asMegaWatt)
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

      /* TICK 58256
      House will reach the upperTemperatureBoundary
      House demand heating : requiredDemand = 0.00 kWh, additionalDemand = 0.00 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 10.44 kWh, additionalDemand = 10.44 kWh
      DomesticWaterStorage : tba
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

      // Results of 57600 for house and storage
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
              energy should equalWithTolerance(
                0.asMegaWattHour
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(58256)))

      /* TICK 61673
     Storage will be fully charged
     House demand heating : requiredDemand = ?0.00 kWh, additionalDemand = 0.00 kWh
     House demand water   : tba
     ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
     DomesticWaterStorage : tba
     Heat pump: stays on to refill the storage now
       */

      heatPumpAgent ! Activation(61673)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 61673.toDateTime
          hpResult.getP should equalWithTolerance(0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.asMegaVar)
      }

      // Results of 58256 for house and storage
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

    }
  }
}
