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
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.test.common.{DefaultTestData, TestSpawnerTyped}
import edu.ie3.simona.test.common.input.EmInputTestData
import edu.ie3.simona.util.SimonaConstants.{INIT_SIM_TICK, PRE_INIT_TICK}
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
    with EmInputTestData
    with MockitoSugar
    with DefaultTestData
    with TestSpawnerTyped {
  private implicit val classicSystem: ActorSystem = system.toClassic
  protected implicit val temperatureTolerance = 0.01

  private val resolution =
    simonaConfig.simona.powerflow.resolution.toSeconds

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
      ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
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
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(3416)))

      /* TICK 3416
      Storage is fully heated up
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 2.37 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
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
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(3600)))

      /* TICK 3600
      New weather data (unchanged) incoming
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.94 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
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
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
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
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
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
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
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
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(25000)))

      /* TICK 25000
        Storage will be empty at tick 26705
        Additional trigger caused by (unchanged) weather data should not change this
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 13.35 kWh
        ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 5.50 kWh
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
              time shouldBe 25000.toDateTime
              qDot should equalWithTolerance(0.01044.asMegaWatt)
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
              qDot should equalWithTolerance(-0.01044.asMegaWatt)
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
        Storage will be empty
        House demand heating : requiredDemand = 0.0kWh, possibleDemand = 11.87 kWh
        ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        DomesticWaterStorage : tba
        Heat pump: will be turned on - to serve the remaining heat demand of house (and refill storage later)
       */

      heatPumpAgent ! Activation(26702)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 26702.toDateTime
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
              energy should equalWithTolerance(0.asMegaWattHour)
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(28000)))

      /* TICK 28000
        New weather data: it's getting warmer again
        House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 10.55 kWh
        ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
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
              energy should equalWithTolerance(0.asMegaWattHour)
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(31940)))

      /* TICK 31940
        House will reach the upperTemperatureBoundary
        House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 0.00 kWh
        ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        Heat pump: stays on to recharge the storage now
       */

      heatPumpAgent ! Activation(31940)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 31940.toDateTime
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
                0.asMegaWattHour
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(35356)))

      /* TICK 35356
        Storage will be fully charged, but meanwhile the house cooled a bit
        House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 1.42 kWh
        ThermalStorage       : requiredDemand = 0.00 kWh, possibleDemand = 0.00 kWh
        Heat pump: stays on
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
              time shouldBe 35894.toDateTime
              qDot should equalWithTolerance(0.0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                20.asDegreeCelsius
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
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(71891)))

    }
  }

  "A Thermal Grid with thermal house, thermal storage and heat pump that is controlled by an energy management" should {
    "be initialized correctly and run through some activations" in {
      implicit val simulationStartWithPv: ZonedDateTime =
        TimeUtil.withDefaults.toZonedDateTime("2020-06-01T10:00:00Z")
      val simulationEndWithPv: ZonedDateTime =
        TimeUtil.withDefaults.toZonedDateTime("2020-06-12T10:00:00Z")

      val simulationParams = SimulationParameters(
        expectedPowerRequestTick = Long.MaxValue,
        requestVoltageDeviationTolerance = Each(1e-14d),
        simulationStart = simulationStartWithPv,
        simulationEnd = simulationEndWithPv,
      )

      val gridAgent = TestProbe[GridAgent.Request]("GridAgent")
      val resultListener: TestProbe[ResultEvent] = TestProbe("resultListener")
      val scheduler: TestProbe[SchedulerMessage] = TestProbe("scheduler")
      val primaryServiceProxy =
        TestProbe[ServiceMessage]("PrimaryServiceProxy")

      val weatherService = TestProbe[ServiceMessage]("WeatherService")

      val participantRefs = ParticipantRefs(
        gridAgent = gridAgent.ref,
        primaryServiceProxy = primaryServiceProxy.ref.toClassic,
        services =
          Map(ServiceType.WeatherService -> weatherService.ref.toClassic),
        resultListener = Iterable(resultListener.ref),
      )

      val keys = ScheduleLock
        .multiKey(TSpawner, scheduler.ref, PRE_INIT_TICK, 3)
        .iterator
      val lockActivation =
        scheduler.expectMessageType[ScheduleActivation].actor
      lockActivation ! Activation(PRE_INIT_TICK)

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

      val pvAgent = spawn(
        ParticipantAgentInit(
          pvInputContainer,
          PvRuntimeConfig(calculateMissingReactivePowerWithModel = true),
          outputConfigOff,
          participantRefs,
          simulationParams,
          Right(emAgent),
          keys.next(),
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

      val emInitSchedule = scheduler.expectMessageType[ScheduleActivation]
      emInitSchedule.tick shouldBe INIT_SIM_TICK
      val emAgentActivation = emInitSchedule.actor

      /* INIT */

      emAgentActivation ! Activation(INIT_SIM_TICK)

      primaryServiceProxy.receiveMessages(1) should contain oneOf (
        PrimaryServiceRegistrationMessage(
          heatPumpAgentWithEm.ref,
          typicalHpInputModel.getUuid,
        ),
        PrimaryServiceRegistrationMessage(
          pvAgent.toClassic,
          pvInput.getUuid,
        )
      )

      // pv
      pvAgent ! RegistrationFailedMessage(primaryServiceProxy.ref.toClassic)

      // deal with weather service registration
      weatherService.expectMessage(
        RegisterForWeatherMessage(
          pvAgent.toClassic,
          pvInput.getNode.getGeoPosition.getY,
          pvInput.getNode.getGeoPosition.getX,
        )
      )

      pvAgent ! RegistrationSuccessfulMessage(
        weatherService.ref.toClassic,
        0L,
      )

      scheduler.expectMessage(Completion(emAgentActivation, Some(0)))

      // heat pump
      heatPumpAgentWithEm ! Activation(INIT_SIM_TICK)

      primaryServiceProxy.expectMessage(
        PrimaryServiceRegistrationMessage(
          heatPumpAgentWithEm.ref,
          typicalHpInputModel.getUuid,
        )
      )
      heatPumpAgentWithEm ! RegistrationFailedMessage(
        primaryServiceProxy.ref.toClassic
      )

      weatherService.expectMessage(
        RegisterForWeatherMessage(
          heatPumpAgentWithEm.ref,
          typicalHpInputModel.getNode.getGeoPosition.getY,
          typicalHpInputModel.getNode.getGeoPosition.getX,
        )
      )

      heatPumpAgentWithEm ! RegistrationSuccessfulMessage(
        weatherService.ref.toClassic,
        0L,
      )

      scheduler.expectMessage(Completion(heatPumpAgentWithEm))

      val weatherDependentAgents = Seq(heatPumpAgentWithEm, pvAgent.toClassic)

      /* TICK 0
        Start of Simulation, No sun at the moment.
        PV: 0.0 kW
        House demand heating : requiredDemand = 0.0 kWh, additionalDemand 0.0 kWh
        House demand water   : tba
        ThermalStorage       : requiredDemand = 10.44 kWh, additionalDemand = 10.44 kWh
        DomesticWaterStorage : tba
        Heat pump: stays out - since requiredDemand of ThermalStorage not necessarily demand hp operation.
       */

      emAgentActivation ! Activation(0)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
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
                  20.asDegreeCelsius
                )(temperatureTolerance)

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
        New Weather: The sun comes out, PV will produce.
        PV: -6.3 kW
        House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 1.25 kWh
        House demand water   : tba
        ThermalStorage       : requiredDemand = 10.44 kWh, additionalDemand = 10.44 kWh
        DomesticWaterStorage : tba
        Heat pump: turns on - since now we have flexibility potential available which can be used by hp to serve the reqDemand of ThermalStorage
       */

      emAgentActivation ! Activation(1800)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
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
                emResult._3 should equalWithTolerance(
                  -0.002517561515.asMegaWatt
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
                time shouldBe 1800.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.83.asDegreeCelsius
                )(temperatureTolerance)

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

      scheduler.expectMessage(Completion(emAgentActivation, Some(5216)))

      /* TICK 5216
      Storage is fully heated up, meanwhile house cooled a bit.
      PV: -6.3 kW
      House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 3.6 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      DomesticWaterStorage : tba
      Heat pump: stays on since it was on and the house has additional demand
       */

      emAgentActivation ! Activation(5216)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 5216.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 5216.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.0025175615153993284.asMegaWatt
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
                time shouldBe 5216.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.52.asDegreeCelsius
                )(temperatureTolerance)

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 5216.toDateTime
                qDot should equalWithTolerance(0.0.asMegaWatt)
                energy should equalWithTolerance(0.01044.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(5400)))

      /* TICK 5400
      PV: 0.0 kW
      New weather data, sun is gone again, thus we should now heat the house by storage.
      House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 3.17 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      DomesticWaterStorage : tba
      Heat pump: turns off
       */

      emAgentActivation ! Activation(5400)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
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
                  19.58.asDegreeCelsius
                )(temperatureTolerance)

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

      scheduler.expectMessage(Completion(emAgentActivation, Some(6829)))

      /* TICK 6829
     The house reaches target temperature
     PV: 0.0 kW
     House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
     House demand water   : tba
     ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 4.15 kWh
     DomesticWaterStorage : tba
     Heat pump: stays off
       */

      emAgentActivation ! Activation(6829)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 6829.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 6829.toDateTime
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
                time shouldBe 6829.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  20.asDegreeCelsius
                )(temperatureTolerance)

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 6829.toDateTime
                qDot should equalWithTolerance(0.0.asMegaWatt)
                energy should equalWithTolerance(0.0062900999999.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(9200)))

      /* TICK 9200
     The sun is back again, storage first.
     PV: -5.2 kW
     House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 1.64 kWh
     House demand water   : tba
     ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 4.15 kWh
     DomesticWaterStorage : tba
     Heat pump: turned off
       */

      emAgentActivation ! Activation(9200)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          9200,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(450),
            WattsPerSquareMeter(250),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(12000),
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
                  -0.0013527980811294546.asMegaWatt
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
                  19.78.asDegreeCelsius
                )(temperatureTolerance)

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

      scheduler.expectMessage(Completion(emAgentActivation, Some(10556)))

      /* TICK 10556
      Storage is full, now heating the house till target temperature.
      PV: -5.2 kW
      House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 2.58 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      DomesticWaterStorage : tba
      Heat pump: stays on
       */

      emAgentActivation ! Activation(10556)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 10556.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 10556.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.0013527980811294546.asMegaWatt
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
                time shouldBe 10556.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.66.asDegreeCelsius
                )(temperatureTolerance)

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 10556.toDateTime
                qDot should equalWithTolerance(0.0.asMegaWatt)
                energy should equalWithTolerance(0.01044.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(11644)))

      /* TICK 11644
      House reaches target temperature boundary.
      PV: -5.2 kW
      House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      DomesticWaterStorage : tba
      Heat pump: stays on
       */

      emAgentActivation ! Activation(11644)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 11644.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 11644.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.005152798081129455.asMegaWatt
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
                time shouldBe 11644.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  20.asDegreeCelsius
                )(temperatureTolerance)

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 11644.toDateTime
                qDot should equalWithTolerance(0.0.asMegaWatt)
                energy should equalWithTolerance(0.01044.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(12000)))

      /* TICK 12000
      House would reach lowerTempBoundary at tick 23809,
      but now it's getting colder which should decrease inner temp of house faster, but the sun is still there.
      PV: -5.2 kW
      House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 0.25 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      DomesticWaterStorage : tba
      Heat pump: turned on, since there is additionalDemand and setPower is 3800 W which is > 0.5 sRated of Hp
       */

      emAgentActivation ! Activation(12000)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          12000,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(450),
            WattsPerSquareMeter(250),
            Celsius(-25d),
            MetersPerSecond(0d),
          ),
          Some(12500),
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
                hpResult._1 shouldBe 12000.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 12000.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.0014021250411259763.asMegaWatt
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
                time shouldBe 12000.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.97.asDegreeCelsius
                )(temperatureTolerance)

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 12000.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.01044.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(12138)))

      /* TICK 12138
      PV: 0.0 kW
      House reaches the target temperature.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays off
       */

      emAgentActivation ! Activation(12138)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 12138.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 12138.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.005202125041125976.asMegaWatt
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
                time shouldBe 12138.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  20.asDegreeCelsius
                )(temperatureTolerance)

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 12138.toDateTime
                qDot should equalWithTolerance(0.0.asMegaWatt)
                energy should equalWithTolerance(0.01044.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(12500)))

      /* TICK 12500
       PV: 0.0 kW
       Inner temperature of the house is decreasing but above the lower boundary.
       Thus, updated weather data (sun is gone) should not change behaviour.
       House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.45 kWh
       ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
       Heat pump: stays off
       */

      emAgentActivation ! Activation(12500)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          12500,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(-25d),
            MetersPerSecond(0d),
          ),
          Some(25200),
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
                hpResult._1 shouldBe 12500.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 12500.toDateTime
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
                time shouldBe 12500.toDateTime
                qDot should equalWithTolerance(0.0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.94.asDegreeCelsius
                )(temperatureTolerance)

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 12500.toDateTime
                qDot should equalWithTolerance(0.0.asMegaWatt)
                energy should equalWithTolerance(0.01044.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(24153)))

      /* TICK 24153
        House reaches lower boundary, since we haven't surplus energy from pv we would use the energy from storage to heat the house.
        PV: 0.0 kW
        House demand heating : requiredDemand = 15.0 kWh, additionalDemand = 15.0 kWh
        House demand water   : tba
        ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        DomesticWaterStorage : tba
        Heat pump: stays off
       */

      emAgentActivation ! Activation(24153)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 24153.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 24153.toDateTime
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
                time shouldBe 24153.toDateTime
                qDot should equalWithTolerance(0.01044.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  18.asDegreeCelsius
                )(temperatureTolerance)

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 24153.toDateTime
                qDot should equalWithTolerance(-0.01044.asMegaWatt)
                energy should equalWithTolerance(0.01044.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(25200)))

      /* TICK 25200
        The sun comes out and it's getting warmer.
        PV: -4.4 kW
        House demand heating : requiredDemand = 15.0 kWh, possibleDemand = 13.21 kWh
        ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 3.04 kWh
        Heat pump: will be turned on and will continue heating the house
       */

      emAgentActivation ! Activation(25200)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          25200,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(450),
            WattsPerSquareMeter(250),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(27500),
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
                hpResult._1 shouldBe 25200.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 25200.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.000557218282208516.asMegaWatt
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
                time shouldBe 25200.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  18.24.asDegreeCelsius
                )(temperatureTolerance)

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 25200.toDateTime
                qDot should equalWithTolerance(0.0.asMegaWatt)
                energy should equalWithTolerance(
                  0.007403699999999999.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(27500)))

      /* TICK 27500
        Additional trigger caused by (unchanged) weather data should not change this.
        PV: -3.9 kW
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 7.67 kWh
        ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 3.04 kWh
        Heat pump: stays on
       */
      emAgentActivation ! Activation(27500)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          27500,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(450),
            WattsPerSquareMeter(250),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(31000),
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
                hpResult._1 shouldBe 27500.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 27500.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.00006389649707132048.asMegaWatt
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
                time shouldBe 27500.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  18.98.asDegreeCelsius
                )(temperatureTolerance)

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 27500.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.007403699999999999.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(30710)))

      /* TICK 30710
        House reaches target temperature, since Hp is running we now charge the storage.
        PV: -3.9 kW
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 3.04 kWh
        DomesticWaterStorage : tba
        Heat pump: stays on - to serve the remaining heat demand of the storage.
       */

      emAgentActivation ! Activation(30710)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 30710.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 30710.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.00006389649707132048.asMegaWatt
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
                time shouldBe 30710.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  20.asDegreeCelsius
                )(temperatureTolerance)

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 30710.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                energy should equalWithTolerance(
                  0.007403699999999999.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(31000)))

      /* TICK 31000
      The sun is gone again, it's getting colder as well.
      PV: 0.0 kW
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.2 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 2.15 kWh
      Heat pump: Will be turned off since no required demand need to be covered.
       */
      emAgentActivation ! Activation(31000)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          31000,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(0),
            WattsPerSquareMeter(0),
            Celsius(-35d),
            MetersPerSecond(0d),
          ),
          Some(145000),
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
                hpResult._1 shouldBe 31000.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 31000.toDateTime
                emResult._3 should equalWithTolerance(
                  0.0.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  0.0.asMegaVar
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
                time shouldBe 31000.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.97.asDegreeCelsius
                )(temperatureTolerance)

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 31000.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.008289811111111111.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(40690)))

      /* TICK 40690
        House reach lower temperature boundary
        PV: 0.0 kW
        House demand heating : requiredDemand = 15.00 kWh, possibleDemand = 15.00 kWh
        ThermalStorage       : requiredDemand = 0.00 kWh, possibleDemand = 2.15 kWh
        Heat pump: stays off - demand will be covered by storage.
       */

      emAgentActivation ! Activation(40690)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 40690.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 40690.toDateTime
                emResult._3 should equalWithTolerance(
                  0.0.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  0.0.asMegaVar
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
                time shouldBe 40690.toDateTime
                qDot should equalWithTolerance(0.01044.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  18.asDegreeCelsius
                )(temperatureTolerance)

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 40690.toDateTime
                qDot should equalWithTolerance(-0.01044.asMegaWatt)
                energy should equalWithTolerance(
                  0.008289811111111111.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(43548)))

      /* TICK 43548
        Storage is empty now.
        Note: One could argue, that new weather should not change the operation of an agent (at least,
        if the weather did not change the flexOptions), but so far we don't check for this.
        Thus, the Hp will stop operation since it can be turned off
        (lower Temp < innerTemp < targetTemp && storage must not directly recharged).
        PV: 0.0 kW
        House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 10.92 kWh
        ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        Heat pump: turned off
       */

      emAgentActivation ! Activation(43548)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 43548.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 43548.toDateTime
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
                time shouldBe 43548.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  18.54.asDegreeCelsius
                )(temperatureTolerance)

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 43548.toDateTime
                qDot should equalWithTolerance(0.0.asMegaWatt)
                energy should equalWithTolerance(0.0.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(46292)))

      /* TICK 46292
        House will reach the lower temperature boundary
        PV: 0.0 kW
        House demand heating : requiredDemand = 15.00 kWh, possibleDemand = 15.00 kWh
        ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        Heat pump: turned on to heat the house
       */

      emAgentActivation ! Activation(46292)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 46292.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 46292.toDateTime
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
                time shouldBe 46292.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  18.asDegreeCelsius
                )(temperatureTolerance)

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 46292.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(55765)))

      /* TICK 55765
        House will reach target temperature
        PV: 0.0 kW
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        Heat pump: turned off - no surplus energy to recharge the storage now
       */

      emAgentActivation ! Activation(55765)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 55765.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 55765.toDateTime
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
                time shouldBe 55765.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  20.asDegreeCelsius
                )(temperatureTolerance)

              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 55765.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(65582)))

    }
  }
}
