/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.agent.participant.ParticipantAgent.{
  DataProvision,
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.agent.participant.ParticipantAgentInit
import edu.ie3.simona.agent.participant.ParticipantAgentInit.{
  ParticipantRefs,
  SimulationParameters,
}
import edu.ie3.simona.config.RuntimeConfig.{HpRuntimeConfig, PvRuntimeConfig}
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.*
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
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.ServiceType
import edu.ie3.simona.test.common.input.{
  EmInputTestData,
  ThermalGridITInputTestData,
}
import edu.ie3.simona.test.common.{DefaultTestData, TestSpawnerTyped}
import edu.ie3.simona.test.matchers.QuantityMatchers
import edu.ie3.simona.util.SimonaConstants.{INIT_SIM_TICK, PRE_INIT_TICK}
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.QuantityUtils.*
import edu.ie3.util.scala.quantities.WattsPerSquareMeter
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.mockito.MockitoSugar
import squants.Each
import squants.motion.MetersPerSecond
import squants.thermal.Celsius
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.time.ZonedDateTime
import javax.measure.quantity.Temperature
import scala.language.postfixOps

/** Test to ensure the functions that a thermal grid and its connected assets is
  * capable.
  */
class ThermalGridIT
    extends ScalaTestWithActorTestKit
    with ThermalHouseTestData
    with AnyWordSpecLike
    with should.Matchers
    with QuantityMatchers
    with ThermalGridITInputTestData
    with EmInputTestData
    with MockitoSugar
    with DefaultTestData
    with TestSpawnerTyped {
  protected given temperatureTolerance: ComparableQuantity[Temperature] =
    Quantities.getQuantity(0.01, Units.CELSIUS)

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

      val simulationParams = SimulationParameters(
        expectedPowerRequestTick = Long.MaxValue,
        requestVoltageDeviationTolerance = Each(1e-14d),
        simulationStart = simulationStartDate,
        simulationEnd = simulationEndDate,
      )

      val gridAgent = TestProbe[GridAgent.Request]("GridAgent")
      val resultListener = TestProbe[ResultEvent]("ResultListener")
      val scheduler: TestProbe[SchedulerMessage] = TestProbe("scheduler")
      val primaryServiceProxy =
        TestProbe[ServiceMessage]("PrimaryServiceProxy")

      val weatherService = TestProbe[ServiceMessage]("WeatherService")

      val participantRefs = ParticipantRefs(
        gridAgent = gridAgent.ref,
        primaryServiceProxy = primaryServiceProxy.ref,
        services = Map(ServiceType.WeatherService -> weatherService.ref),
        resultListener = Iterable(resultListener.ref),
      )

      val key = ScheduleLock.singleKey(TSpawner, scheduler.ref, PRE_INIT_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      val hpAgent = spawn(
        ParticipantAgentInit(
          hpInputContainerLittleWaterStorage,
          HpRuntimeConfig(),
          outputConfigOn,
          participantRefs,
          simulationParams,
          Left(scheduler.ref),
          key,
        ),
        "HeatPumpAgent1",
      )

      val pRunningHp = 0.0038.asMegaWatt
      val qRunningHp = 0.0012489995996796802.asMegaVar

      val hpInitSchedule = scheduler.expectMessageType[ScheduleActivation]
      hpInitSchedule.tick shouldBe INIT_SIM_TICK
      val heatPumpAgent = hpInitSchedule.actor

      /* INIT */

      heatPumpAgent ! Activation(INIT_SIM_TICK)

      primaryServiceProxy.expectMessage(
        PrimaryServiceRegistrationMessage(
          hpAgent,
          typicalHpInputModel.getUuid,
        )
      )

      // heat pump
      hpAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

      weatherService.expectMessage(
        RegisterForWeatherMessage(
          hpAgent,
          typicalHpInputModel.getNode.getGeoPosition.getY,
          typicalHpInputModel.getNode.getGeoPosition.getX,
        )
      )

      hpAgent ! RegistrationSuccessfulMessage(
        weatherService.ref,
        0,
      )
      val weatherDependentAgents = Seq(hpAgent)

      scheduler.expectMessage(Completion(heatPumpAgent, Some(0)))
      resultListener.expectNoMessage()

      /* TICK 0
      Start of Simulation
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      House demand water   : requiredDemand ~ 0.0674 kWh, possibleDemand ~ 0.067 kWh
      HeatStorage          : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: turned on - to serve the heat storage demand
       */
      heatPumpAgent ! Activation(0)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          0,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(3600),
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
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
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
                indoorTemperature should equalWithTolerance(20.asDegreeCelsius)
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
                qDot should equalWithTolerance(-0.004955161979312273.asMegaWatt)
                energy should equalWithTolerance(0.000522.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(49)))

      /* TICK 49
      Domestic hot water storage stops discharging
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 10.29 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.067 kWh
      Heat pump: stays on to serve the heat storage demand
       */
      heatPumpAgent ! Activation(49)

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(hpResult) =>
            hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
            hpResult.getTime shouldBe 49.toDateTime
            hpResult.getP should equalWithTolerance(pRunningHp)
            hpResult.getQ should equalWithTolerance(qRunningHp)
          case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 49.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.0004545547397260274.asMegaWattHour
                )
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(3416)))

      /* TICK 3416
      Heat storage is fully heated up
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 2.36 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.067 kWh
      Heat pump: stays on since it was on and the house has possible demand
       */
      heatPumpAgent ! Activation(3416)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 3416.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
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
                time shouldBe 3416.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.68.asDegreeCelsius
                )
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
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(3600)))

      /* TICK 3600
      New weather data (unchanged) incoming + Domestic hot water storage will cover hot water demand
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.92 kWh
      House demand water   : requiredDemand = 0.037 kWh, possibleDemand = 0.037 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.067 kWh
      Heat pump: stays on, we got triggered by incoming weather data. So we continue with same behaviour as before
       */
      heatPumpAgent ! Activation(3600)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          3600,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(1d),
            WattsPerSquareMeter(1d),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(21600),
        )
      }

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(hpResult) =>
            hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
            hpResult.getTime shouldBe 3600.toDateTime
            hpResult.getP should equalWithTolerance(pRunningHp)
            hpResult.getQ should equalWithTolerance(
              qRunningHp
            )
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 3600.toDateTime
                qDot should equalWithTolerance(-0.004995945205479452.asMegaWatt)
                energy should equalWithTolerance(
                  0.0004545547397260274.asMegaWattHour
                )
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(3627)))

      /* TICK 3627
      Domestic hot water storage will stop discharging.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.85 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.1 kWh
      Heat pump: stays on - continue with same behaviour as before
       */
      heatPumpAgent ! Activation(3627)

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(hpResult) =>
            hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
            hpResult.getTime shouldBe 3627.toDateTime
            hpResult.getP should equalWithTolerance(pRunningHp)
            hpResult.getQ should equalWithTolerance(
              qRunningHp
            )
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 3627.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.000417085151.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(4412)))

      /* TICK 4412
      House reaches target temperature boundary
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.1 kWh
      Heat pump: turned off
       */
      heatPumpAgent ! Activation(4412)

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 4412.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)
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
                time shouldBe 4412.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.99.asDegreeCelsius
                )
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(7200)))

      /* We'll jump through a bunch of activations caused from DomesticHotWaterStorage being active.
      The results are checked implicitly through the state of stored energy at the next result check.
       */
      val activationTicksBlock =
        Seq(7200, 7217, 10800, 10809, 14400, 14411, 18000, 18017, 21600)

      val tickPairs = activationTicksBlock.zipWithIndex.collect {
        case (tick, index) if index < activationTicksBlock.length - 1 =>
          (tick, activationTicksBlock(index + 1))
      }

      tickPairs.foreach { case (currentTick, nextTick) =>
        heatPumpAgent ! Activation(currentTick)
        // Each step we expect a message for the Hp and the DomesticHotWaterStorage, but we don't check the content here.
        Range(0, 2)
          .map { _ =>
            resultListener.expectMessageType[ResultEvent]
          }
          .foreach {
            case ParticipantResultEvent(participantResult) =>
              participantResult match {
                case HpResult(_) =>
                case _           => fail("unexpected result")
              }
            case ThermalResultEvent(thermalUnitResult) =>
              thermalUnitResult match {
                case DomesticHotWaterStorageResult(_) =>
                case _ => fail("unexpected result")
              }
          }
        resultListener.expectNoMessage()
        scheduler.expectMessage(Completion(heatPumpAgent, Some(nextTick)))
      }

      /* TICK 21600
      House would reach lowerTempBoundary at tick 50797.
      But now it's getting colder which should decrease inner temp of house faster.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 11.56 kWh
      House demand water   : requiredDemand = 0.09 kWh, possibleDemand = 0.09 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.18 kWh
      Heat pump: stays off
       */
      heatPumpAgent ! Activation(21600)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          21600,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(2d),
            WattsPerSquareMeter(2d),
            Celsius(-55d),
            MetersPerSecond(0d),
          ),
          Some(25000),
        )
      }

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(hpResult) =>
            hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
            hpResult.getTime shouldBe 21600.toDateTime
            hpResult.getP should equalWithTolerance(0.asMegaWatt)
            hpResult.getQ should equalWithTolerance(0.asMegaVar)
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 21600.toDateTime
                qDot should equalWithTolerance(-0.004980573066.asMegaWatt)
                energy should equalWithTolerance(0.00034589293.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(21665)))

      /* TICK 21665
     Domestic storage stops discharging
     House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 11.7 kWh
     House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
     ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
     DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.27 kWh
     Heat pump: stays off
       */
      heatPumpAgent ! Activation(21665)

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(hpResult) =>
            hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
            hpResult.getTime shouldBe 21665.toDateTime
            hpResult.getP should equalWithTolerance(0.asMegaWatt)
            hpResult.getQ should equalWithTolerance(0.asMegaVar)
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 21665.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.00025596591780821927.asMegaWattHour
                )
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(23288)))

      /* TICK 23288
      House reach lowerTemperatureBoundary
      House demand heating : requiredDemand = 15.0 kWh, possibleDemand = 15.00 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.27 kWh
      Heat pump: stays off, demand should be covered by storage
       */
      heatPumpAgent ! Activation(23288)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 23288.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)
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
                time shouldBe 23288.toDateTime
                qDot should equalWithTolerance(0.01044.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  18.00.asDegreeCelsius
                )
              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 23288.toDateTime
                qDot should equalWithTolerance(-0.01044.asMegaWatt)
                energy should equalWithTolerance(0.01044.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(25000)))

      /* TICK 25000
      Additional trigger caused by (unchanged) weather data should not change this
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 13.51 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 4.96 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.27 kWh
      Heat pump: stays off, demand should be covered by storage
       */
      heatPumpAgent ! Activation(25000)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          25000,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(3d),
            WattsPerSquareMeter(3d),
            Celsius(-55d),
            MetersPerSecond(0d),
          ),
          Some(28000),
        )
      }
      resultListener.expectMessageType[ResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 25000.toDateTime
          hpResult.getP should equalWithTolerance(0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.asMegaVar)
      }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(25200)))

      /* TICK 25200
      DomesticHotWaterStorage
      House demand heating : requiredDemand = 0.0kWh, possibleDemand = 13.34 kWh
      House demand water   : requiredDemand = 0.18 kWh, possibleDemand = 0.18 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 5.5 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.27 kWh
      Heat pump: stays off
       */
      heatPumpAgent ! Activation(25200)

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 25200.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 25200.toDateTime
                qDot should equalWithTolerance(-0.004992011390357028.asMegaWatt)
                energy should equalWithTolerance(0.000255965918.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(25327)))

      /* TICK 25327
      DomesticHotWaterStorage stops discharging.
      House demand heating : requiredDemand = 0.0kWh, possibleDemand = 13.23 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 5.9 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.44 kWh
      Heat pump: turned on - to serve the remaining heat demand of house (and refill storage later)
       */
      heatPumpAgent ! Activation(25327)

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 25327.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 25327.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.000079858849.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(26887)))

      /* TICK 26887
      Heat storage is empty
      House demand heating : requiredDemand = 0.0kWh, possibleDemand = 11.88 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.44 kWh
      Heat pump: turned on - to serve the remaining heat demand of house (and refill storage later)
       */
      heatPumpAgent ! Activation(26887)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 26887.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
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
                time shouldBe 26887.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  18.415.asDegreeCelsius
                )
              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 26887.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(28000)))

      /* TICK 28000
        New weather data: it's getting warmer again
        House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 10.75 kWh
        House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.44 kWh
        Heat pump: stays on
       */
      heatPumpAgent ! Activation(28000)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          28000,
          weatherService.ref,
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

      // Since this activation is caused by new weather data, we don't expect any
      // message for house or storage since there is no change of their operating
      // point nor one of it reached any boundary.
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(28800)))

      /* TICK 28800
      DomesticHotWaterStorage
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 8.6 kWh
      House demand water   : requiredDemand = 0.25 kWh, possibleDemand = 0.25 kWh
      ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.44 kWh
      Heat pump: stays on to recharge the ThermalStorage now
       */
      heatPumpAgent ! Activation(28800)

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 28800.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 28800.toDateTime
                qDot should equalWithTolerance(-0.004956756164383566.asMegaWatt)
                energy should equalWithTolerance(0.000079858849.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(28858)))

      /* TICK 28858
     DomesticHotWaterStorage is empty
     House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 8.5 kWh
     House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
     ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
     DomesticWaterStorage : requiredDemand = 0.522 kWh, possibleDemand = 0.522 kWh
     Heat pump: stays on, qDot should be split between DomesticHotWaterStorage and House
       */
      heatPumpAgent ! Activation(28858)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 28858.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
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
                time shouldBe 28858.toDateTime
                qDot should equalWithTolerance(0.0055.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  18.87.asDegreeCelsius
                )
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 28858.toDateTime
                qDot should equalWithTolerance(0.0055.asMegaWatt)
                energy should equalWithTolerance(0.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(29199)))

      /* TICK 29199
      House will reach the upperTemperatureBoundary
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 8.07 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays on to recharge the ThermalStorage now
       */
      heatPumpAgent ! Activation(29199)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 29199.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
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
                time shouldBe 29199.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  18.92.asDegreeCelsius
                )
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 29199.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.000522.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(32239)))

      /* TICK 32239
        House will reach the upperTemperatureBoundary
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        Heat pump: stays on to recharge the ThermalStorage now
       */
      heatPumpAgent ! Activation(32239)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 32239.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
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
                time shouldBe 32239.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.99.asDegreeCelsius
                )
              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 32239.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                energy should equalWithTolerance(0.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(32400)))

      /* TICK 32400
       DomesticHotWaterStorage will cover demand
       House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
       House demand water   : requiredDemand = 0.21 kWh, possibleDemand = 0.21 kWh
       ThermalStorage       : requiredDemand = 0.00 kWh, possibleDemand = 9.95 kWh
       DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
       Heat pump: stays on
       */
      heatPumpAgent ! Activation(32400)

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 32400.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 32400.toDateTime
                qDot should equalWithTolerance(-0.004992701085216154.asMegaWatt)
                energy should equalWithTolerance(0.000522.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(32554)))

      /* TICK 32554
      DomesticHotWaterStorage stops discharging.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.13 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 9.5 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.21 kWh
      Heat pump: stays on
       */
      heatPumpAgent ! Activation(32554)

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 32554.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 32554.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.0003084233424.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(35655)))

      /* TICK 35655
      Storage will be fully charged, but meanwhile the house cooled a bit
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.4 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.21 kWh
      Heat pump: stays on to recharge the storage now
       */
      heatPumpAgent ! Activation(35655)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 35655.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
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
                time shouldBe 35655.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.81.asDegreeCelsius
                )
              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 35655.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.01044.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(36000)))

      /* TICK 36000
      DomesticHotWaterStorage covers water demand
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.5 kWh
      House demand water   : requiredDemand = 0.23 kWh, possibleDemand = 0.23 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.21 kWh
      Heat pump: stays on
       */
      heatPumpAgent ! Activation(36000)

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 36000.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 36000.toDateTime
                qDot should equalWithTolerance(-0.004986861668742217.asMegaWatt)
                energy should equalWithTolerance(0.000308423342.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(36165)))

      /* TICK 36165
     Thermal House reaches target temperature, DomesticHotWaterStorage stops discharging.
     House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 0.0 kWh
     House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
     ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
     DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.44 kWh
     Heat pump: turned off - since neither house nor any storage have any demand
       */
      heatPumpAgent ! Activation(36165)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 36165.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)
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
                time shouldBe 36165.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.99.asDegreeCelsius
                )
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 36165.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.000079858849.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(39600)))
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
        primaryServiceProxy = primaryServiceProxy.ref,
        services = Map(ServiceType.WeatherService -> weatherService.ref),
        resultListener = Iterable(resultListener.ref),
      )

      val keys = ScheduleLock
        .multiKey(TSpawner, scheduler.ref, PRE_INIT_TICK, 2)
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

      val hpAgent = spawn(
        ParticipantAgentInit(
          hpInputContainerSmallWaterStorage,
          HpRuntimeConfig(),
          outputConfigOn,
          participantRefs,
          simulationParams,
          Right(emAgent),
          keys.next(),
        ),
        "HeatPumpAgentWithEm",
      )

      val pRunningHp = 0.0038.asMegaWatt
      val qRunningHp = 0.0012489995996796802.asMegaVar

      val emInitSchedule = scheduler.expectMessageType[ScheduleActivation]
      emInitSchedule.tick shouldBe INIT_SIM_TICK
      val emAgentActivation = emInitSchedule.actor

      scheduler.expectNoMessage()

      emInitSchedule.unlockKey.value.unlock()
      scheduler.expectMessage(Completion(lockActivation))

      /* INIT */

      emAgentActivation ! Activation(INIT_SIM_TICK)

      primaryServiceProxy.receiveMessages(2) should contain allOf (
        PrimaryServiceRegistrationMessage(
          hpAgent,
          typicalHpInputModel.getUuid,
        ),
        PrimaryServiceRegistrationMessage(
          pvAgent,
          pvInput.getUuid,
        )
      )

      // pv
      pvAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

      // deal with weather service registration
      weatherService.expectMessage(
        RegisterForWeatherMessage(
          pvAgent,
          pvInput.getNode.getGeoPosition.getY,
          pvInput.getNode.getGeoPosition.getX,
        )
      )

      pvAgent ! RegistrationSuccessfulMessage(
        weatherService.ref,
        0L,
      )

      // heat pump
      hpAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

      // deal with weather service registration
      weatherService.expectMessage(
        RegisterForWeatherMessage(
          hpAgent,
          typicalHpInputModel.getNode.getGeoPosition.getY,
          typicalHpInputModel.getNode.getGeoPosition.getX,
        )
      )

      hpAgent ! RegistrationSuccessfulMessage(
        weatherService.ref,
        0L,
      )
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(0)))

      val weatherDependentAgents = Seq(hpAgent.toClassic, pvAgent.toClassic)

      /* TICK 0
        Start of Simulation, No sun at the moment.
        PV: 0.0 kW
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        House demand water   : requiredDemand = 0.23 kWh, possibleDemand = 0.23 kWh
        ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        Heat pump: stays out - since requiredDemand of ThermalStorage not necessarily demand hp operation.
       */
      emAgentActivation ! Activation(0)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          0,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(0),
            WattsPerSquareMeter(0),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(1800),
        )
      }

      Range(0, 5)
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
                indoorTemperature should equalWithTolerance(20.asDegreeCelsius)
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
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
                time shouldBe 0.toDateTime
                qDot should equalWithTolerance(-0.004986861668742217.asMegaWatt)
                energy should equalWithTolerance(
                  0.0014981399999999998.asMegaWattHour
                )
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(165)))

      /* TICK 165
      Domestic hot water storage stops discharging.
      PV: 0.0 kW
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.1 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.23 kWh
      Heat pump: stays out - since requiredDemand of ThermalStorage not necessarily demand hp operation.
       */
      emAgentActivation ! Activation(165)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 165.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 165.toDateTime
                emResult._3 should equalWithTolerance(0.asMegaWatt)
                emResult._4 should equalWithTolerance(0.asMegaVar)
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
                time shouldBe 165.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.001269575506849315.asMegaWattHour
                )
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(1800)))

      /* TICK 1800
        New Weather: The sun comes out, PV will produce.
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.25 kWh
        House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.23 kWh
        Heat pump: turns on - since now we have flexibility potential available which can be
        used by hp to serve the reqDemand of ThermalStorage
       */
      emAgentActivation ! Activation(1800)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          1800,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(540),
            WattsPerSquareMeter(400),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(5400),
        )
      }

      Range(0, 3)
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
                emResult._4 should equalWithTolerance(
                  -0.00082748245392177.asMegaVar
                )
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
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
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(3600)))

      /* TICK 3600
      DomesticHotWaterStorage will serve the water demand of the house
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 2.5 kWh
      House demand water   : requiredDemand = 0.23 kWh, possibleDemand = 0.23 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 4.9 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.23 kWh
      Heat pump: stays on
       */
      emAgentActivation ! Activation(3600)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 3600.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 3600.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.002517561515.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  -0.00082748245392177.asMegaVar
                )
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
                time shouldBe 3600.toDateTime
                qDot should equalWithTolerance(-0.004986861668742217.asMegaWatt)
                energy should equalWithTolerance(
                  0.001269575506849315.asMegaWattHour
                )
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(3765)))

      /* TICK 3765
      DomesticHotWaterStorage stops discharging.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 2.6 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.44 kWh, possibleDemand = 4.4 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.46 kWh
      Heat pump: stays on - since now we have flexibility potential available which
      can be used by hp to serve the reqDemand of ThermalStorage
       */
      emAgentActivation ! Activation(3765)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 3765.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 3765.toDateTime
                emResult._3 should equalWithTolerance(-0.00251756152.asMegaWatt)
                emResult._4 should equalWithTolerance(-0.00082748245.asMegaVar)
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
                time shouldBe 3765.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.00104101101369863.asMegaWattHour
                )
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(5216)))

      /* TICK 5216
      Storage is fully heated up, meanwhile house cooled a bit.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 3.59 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays on since it was on and the house has possible demand
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
                emResult._3 should equalWithTolerance(-0.00251756152.asMegaWatt)
                emResult._4 should equalWithTolerance(-0.00082748245.asMegaVar)
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
                )
              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 5216.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.01044.asMegaWattHour)
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
                time shouldBe 5216.toDateTime
                qDot should equalWithTolerance(-0.010971095671.asMegaWatt)
                energy should equalWithTolerance(0.001269575507.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(5400)))

      /* TICK 5400
      PV: 0.0 kW
      New weather data, sun is gone again, thus we should now heat the house by storage.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 3.15 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.45 kWh
      Heat pump: turns off
       */
      emAgentActivation ! Activation(5400)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          5400,
          weatherService.ref,
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
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(6824)))

      /* TICK 6824
     The house reaches target temperature
     PV: 0.0 kW
     House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
     House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
     ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 4.13 kWh
     DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.45 kWh
     Heat pump: stays off
       */
      emAgentActivation ! Activation(6824)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 6824.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 6824.toDateTime
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
                time shouldBe 6824.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.99.asDegreeCelsius
                )
              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 6824.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.0063104.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(7200)))

      /* TICK 7200
     DomesticHotWaterStorage will serve the water demand of the house
     House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.26 kWh
     House demand water   : requiredDemand = 0.24 kWh, possibleDemand = 0.24 kWh
     ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 4.13 kWh
     DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.45 kWh
     Heat pump: stays off
       */
      emAgentActivation ! Activation(7200)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 7200.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 7200.toDateTime
                emResult._3 should equalWithTolerance(0.asMegaWatt)
                emResult._4 should equalWithTolerance(0.asMegaVar)
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
                time shouldBe 7200.toDateTime
                qDot should equalWithTolerance(-0.0049988839968.asMegaWatt)
                energy should equalWithTolerance(0.001041011014.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(7370)))

      /* TICK 7370
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.38 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 4.13 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.69 kWh
      Heat pump: stays off
       */
      emAgentActivation ! Activation(7370)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 7370.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 7370.toDateTime
                emResult._3 should equalWithTolerance(0.asMegaWatt)
                emResult._4 should equalWithTolerance(0.asMegaVar)
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
                time shouldBe 7370.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.0008049526027397258.asMegaWattHour
                )
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(9200)))

      /* TICK 9200
      The sun is back again, storage first.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.64 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 4.13 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.69 kWh
      Heat pump: turned on
       */
      emAgentActivation ! Activation(9200)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          9200,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(450),
            WattsPerSquareMeter(250),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(12000),
        )
      }

      Range(0, 3)
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
                emResult._3 should equalWithTolerance(-0.00135279808.asMegaWatt)
                emResult._4 should equalWithTolerance(-0.00044464323.asMegaVar)
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
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
                  0.0063104.asMegaWattHour
                )
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(10551)))

      /* TICK 10551
      Storage is full, now heating the house till target temperature.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 2.57 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays on
       */
      emAgentActivation ! Activation(10551)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 10551.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 10551.toDateTime
                emResult._3 should equalWithTolerance(-0.00135279808.asMegaWatt)
                emResult._4 should equalWithTolerance(-0.00044464323.asMegaVar)
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
                time shouldBe 10551.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.65.asDegreeCelsius
                )
              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 10551.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.01044.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(10800)))

      /* TICK 10800
      House has water demand
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.98 kWh
      House demand water   : requiredDemand = 0.24 kWh, possibleDemand = 0.24 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.69 kWh
      Heat pump: stays on
       */
      emAgentActivation ! Activation(10800)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 10800.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 10800.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.0013527980811294546.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  -0.0004446432267837181.asMegaVar
                )
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
                time shouldBe 10800.toDateTime
                qDot should equalWithTolerance(-0.004990169546282365.asMegaWatt)
                energy should equalWithTolerance(
                  0.0008049526027397258.asMegaWattHour
                )
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(10973)))

      /* TICK 10973
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.57 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.93 kWh
      Heat pump: stays on
       */
      emAgentActivation ! Activation(10973)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 10973.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 10973.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.0013527980811294546.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  -0.0004446432267837181.asMegaVar
                )
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
                time shouldBe 10973.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.0005651472328767123.asMegaWattHour
                )
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(11638)))

      /* TICK 11638
      House reaches target temperature boundary.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.93 kWh
      Heat pump: turned off
       */
      emAgentActivation ! Activation(11638)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 11638.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 11638.toDateTime
                emResult._3 should equalWithTolerance(-0.00515279808.asMegaWatt)
                emResult._4 should equalWithTolerance(-0.00169364283.asMegaVar)
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
                time shouldBe 11638.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.99.asDegreeCelsius
                )
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(12000)))

      /* TICK 12000
      House would reach lowerTempBoundary at tick 23809,
      but now it's getting colder which should decrease inner temp of house faster, but the sun is still there.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.25 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.93 kWh
      Heat pump: turned on, since there is possibleDemand and setPower is 3800 W which is > 0.5 sRated of Hp
       */
      emAgentActivation ! Activation(12000)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          12000,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(450),
            WattsPerSquareMeter(250),
            Celsius(-25d),
            MetersPerSecond(0d),
          ),
          Some(12500),
        )
      }

      Range(0, 3)
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
                emResult._3 should equalWithTolerance(-0.001402125.asMegaWatt)
                emResult._4 should equalWithTolerance(-0.0004608562.asMegaVar)
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
                  19.96.asDegreeCelsius
                )
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(12139)))

      /* TICK 12139
      House reaches the target temperature.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.93 kWh
      Heat pump: turned off
       */
      emAgentActivation ! Activation(12139)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 12139.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 12139.toDateTime
                emResult._3 should equalWithTolerance(-0.005202125.asMegaWatt)
                emResult._4 should equalWithTolerance(-0.0017098558.asMegaVar)
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
                time shouldBe 12139.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(20.asDegreeCelsius)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(12500)))

      /* TICK 12500
       PV: 0.0 kW
       Inner temperature of the house is decreasing but above the lower boundary.
       Thus, updated weather data (sun is gone) should not change behaviour.
       House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.45 kWh
       House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
       ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
       DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.93 kWh
       Heat pump: stays off
       */
      emAgentActivation ! Activation(12500)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          12500,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(-25d),
            MetersPerSecond(0d),
          ),
          Some(25200),
        )
      }

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ParticipantResultEvent(participantResult) =>
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
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(14400)))

      /* We'll jump through a bunch of activations caused from DomesticHotWaterStorage being active.
     The results are checked implicitly through the state of stored energy at the next result check.
       */
      val firstActivationTicksBlock =
        Seq(14400, 14538, 18000, 18119, 21600, 21717, 24413)

      val firstTickPairs = firstActivationTicksBlock.zipWithIndex.collect {
        case (tick, index) if index < firstActivationTicksBlock.length - 1 =>
          (tick, firstActivationTicksBlock(index + 1))
      }

      firstTickPairs.foreach { case (currentTick, nextTick) =>
        emAgentActivation ! Activation(currentTick)
        // Each step we expect a message for the Hp and the DomesticHotWaterStorage, but we don't check the content here.
        Range(0, 3)
          .map { _ =>
            resultListener.expectMessageType[ResultEvent]
          }
          .foreach {
            case ParticipantResultEvent(participantResult) =>
              participantResult match {
                case HpResult(_) =>
                case EmResult(_) =>
                case _           => fail("unexpected result")
              }
            case ThermalResultEvent(thermalUnitResult) =>
              thermalUnitResult match {
                case DomesticHotWaterStorageResult(_) =>
                case _ => fail("unexpected result")
              }
          }
        resultListener.expectNoMessage()
        scheduler.expectMessage(Completion(emAgentActivation, Some(nextTick)))
      }

      /* TICK 24413
        House reaches lower boundary, since we don't have surplus energy from pv, we would use the energy from storage to heat the house.
        PV: 0.0 kW
        House demand heating : requiredDemand = 15.0 kWh, possibleDemand = 15.0 kWh
        House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 1.45 kWh
        Heat pump: stays off
       */
      emAgentActivation ! Activation(24413)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 24413.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 24413.toDateTime
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
                time shouldBe 24413.toDateTime
                qDot should equalWithTolerance(0.01044.asMegaWatt)
                indoorTemperature should equalWithTolerance(18.asDegreeCelsius)
              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 24413.toDateTime
                qDot should equalWithTolerance(-0.01044.asMegaWatt)
                energy should equalWithTolerance(0.01044.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(25200)))

      /* TICK 25200
        The sun comes out and it's getting warmer.
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 13.66 kWh
        House demand water   : requiredDemand = 0.18 kWh, possibleDemand = 0.18 kWh
        ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 2.28 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 1.45 kWh
        Heat pump: will be turned on and will continue heating the house
       */
      emAgentActivation ! Activation(25200)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          25200,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(450),
            WattsPerSquareMeter(250),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(27500),
        )
      }

      Range(0, 5)
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
                emResult._3 should equalWithTolerance(-0.00055721828.asMegaWatt)
                emResult._4 should equalWithTolerance(-0.00018314879.asMegaVar)
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
                  18.18.asDegreeCelsius
                )
              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 25200.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.0081577.asMegaWattHour)
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
                time shouldBe 25200.toDateTime
                qDot should equalWithTolerance(-0.0049440244227.asMegaWatt)
                energy should equalWithTolerance(0.000048066904.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(25235)))

      /* TICK 25235
      DomesticHotWaterStorage is empty.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 13.57 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 2.28 kWh
      DomesticWaterStorage : requiredDemand = 1.5 kWh, possibleDemand = 1.5 kWh
      Heat pump: Stays on, but qDot will be split between house and hot water storage
       */
      emAgentActivation ! Activation(25235)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 25235.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 25235.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.000557218282208516.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  -0.000183148792477.asMegaVar
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
                time shouldBe 25235.toDateTime
                qDot should equalWithTolerance(0.0055.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  18.19.asDegreeCelsius
                )
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
                time shouldBe 25235.toDateTime
                qDot should equalWithTolerance(0.0055.asMegaWatt)
                energy should equalWithTolerance(0.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(26215)))

      /* TICK 26215
      DomesticHotWaterStorage is full
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 12.71 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 2.28 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays on - will continue heating the house only
       */
      emAgentActivation ! Activation(26215)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 26215.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 26215.toDateTime
                emResult._3 should equalWithTolerance(-0.00055721828.asMegaWatt)
                emResult._4 should equalWithTolerance(-0.00018314879.asMegaVar)
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
                time shouldBe 26215.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  18.30.asDegreeCelsius
                )
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
                time shouldBe 26215.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.00149814.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(27500)))

      /* TICK 27500
        Additional trigger caused by (unchanged) weather data should not change this.
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 9.62 kWh
        House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 2.28 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        Heat pump: stays on
       */
      emAgentActivation ! Activation(27500)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          27500,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(450),
            WattsPerSquareMeter(250),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(31000),
        )
      }

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ParticipantResultEvent(participantResult) =>
          participantResult match {
            case HpResult(hpResult) =>
              hpResult._2 shouldBe typicalHpInputModel.getUuid
              hpResult._1 shouldBe 27500.toDateTime
              hpResult._3 should equalWithTolerance(pRunningHp)
              hpResult._4 should equalWithTolerance(qRunningHp)
            case EmResult(emResult) =>
              emResult._2 shouldBe emInput.getUuid
              emResult._1 shouldBe 27500.toDateTime
              emResult._3 should equalWithTolerance(-0.000063896497.asMegaWatt)
              emResult._4 should equalWithTolerance(-0.00002100176.asMegaVar)
          }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(28800)))

      /* TICK 28800
      DomesticHotWaterStorage discharges
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 6.15 kWh
      House demand water   : requiredDemand = 0.21 kWh, possibleDemand = 0.21 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 2.28 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays on
       */
      emAgentActivation ! Activation(28800)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 28800.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 28800.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.000063896497.asMegaWatt
                )
                emResult._4 should equalWithTolerance(-0.000021001763.asMegaVar)
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
                time shouldBe 28800.toDateTime
                qDot should equalWithTolerance(-0.004992701085.asMegaWatt)
                energy should equalWithTolerance(0.00149814.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(28954)))

      /* TICK 28954
      DomesticHotWaterStorage stops discharging.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 6.15 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 2.28 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.21 kWh
      Heat pump: stays on
       */
      emAgentActivation ! Activation(28954)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 28954.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 28954.toDateTime
                emResult._3 should equalWithTolerance(-0.0000638965.asMegaWatt)
                emResult._4 should equalWithTolerance(-0.000021001763.asMegaVar)
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
                time shouldBe 28954.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.00128456334.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(31000)))

      /* TICK 31000
        The sun is gone again, it's getting colder as well.
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.29 kWh
        House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 2.28 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.21 kWh
        Heat pump: Will be turned off since setPower of EM is zero and
         the heating of the house can be continued from storage.
       */
      emAgentActivation ! Activation(31000)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          31000,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(0),
            WattsPerSquareMeter(0),
            Celsius(-35d),
            MetersPerSecond(0d),
          ),
          Some(46800),
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
                time shouldBe 31000.toDateTime
                qDot should equalWithTolerance(0.01044.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.83.asDegreeCelsius
                )
              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 31000.toDateTime
                qDot should equalWithTolerance(-0.01044.asMegaWatt)
                energy should equalWithTolerance(0.008157699999.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(31937)))

      /* TICK 31937
      House reaches target temperature.
      PV: 0.0 kW
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 5.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.21 kWh
      Heat pump: stays off.
       */
      emAgentActivation ! Activation(31937)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 31937.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 31937.toDateTime
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
                time shouldBe 31937.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.99.asDegreeCelsius
                )
              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 31937.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.0054404.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(32400)))

      /* We'll jump through a bunch of activations caused from DomesticHotWaterStorage being active.
 The results are checked implicitly through the state of stored energy at the next result check.
       */

      val secondActivationTicksBlock =
        Seq(32400, 32576, 36000, 36179, 39600, 39757, 41937)

      val secondTickPairs = secondActivationTicksBlock.zipWithIndex.collect {
        case (tick, index) if index < secondActivationTicksBlock.length - 1 =>
          (tick, secondActivationTicksBlock(index + 1))
      }

      secondTickPairs.foreach { case (currentTick, nextTick) =>
        emAgentActivation ! Activation(currentTick)
        // Each step we expect a message for the Hp and the DomesticHotWaterStorage, but we don't check the content here.
        Range(0, 3)
          .map { _ =>
            resultListener.expectMessageType[ResultEvent]
          }
          .foreach {
            case ParticipantResultEvent(participantResult) =>
              participantResult match {
                case HpResult(_) =>
                case EmResult(_) =>
                case _           => fail("unexpected result")
              }
            case ThermalResultEvent(thermalUnitResult) =>
              thermalUnitResult match {
                case DomesticHotWaterStorageResult(_) =>
                case _ => fail("unexpected result")
              }
          }
        resultListener.expectNoMessage()
        scheduler.expectMessage(Completion(emAgentActivation, Some(nextTick)))
      }

      /* TICK 41937
      House reaches lower temperature.
      PV: 0.0 kW
      House demand heating : requiredDemand = 15.0 kWh, possibleDemand = 15.0 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 5.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.92 kWh
      Heat pump: stays off - demand will be served by storage.
       */
      emAgentActivation ! Activation(41937)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 41937.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 41937.toDateTime
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
                time shouldBe 41937.toDateTime
                qDot should equalWithTolerance(0.01044.asMegaWatt)
                indoorTemperature should equalWithTolerance(18.asDegreeCelsius)
              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 41937.toDateTime
                qDot should equalWithTolerance(-0.01044.asMegaWatt)
                energy should equalWithTolerance(0.0054404.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(43200)))

      /* We'll jump through a bunch of activations caused from DomesticHotWaterStorage being active.
The results are checked implicitly through the state of stored energy at the next result check.
       */
      val thirdActivationTicksBlock =
        Seq(43200, 43322)

      val thirdTickPairs = thirdActivationTicksBlock.zipWithIndex.collect {
        case (tick, index) if index < thirdActivationTicksBlock.length - 1 =>
          (tick, thirdActivationTicksBlock(index + 1))
      }

      thirdTickPairs.foreach { case (currentTick, nextTick) =>
        emAgentActivation ! Activation(currentTick)
        // Each step we expect a message for the Hp and the DomesticHotWaterStorage, but we don't check the content here.
        Range(0, 3)
          .map { _ =>
            resultListener.expectMessageType[ResultEvent]
          }
          .foreach {
            case ParticipantResultEvent(participantResult) =>
              participantResult match {
                case HpResult(_) =>
                case EmResult(_) =>
                case _           => fail("unexpected result")
              }
            case ThermalResultEvent(thermalUnitResult) =>
              thermalUnitResult match {
                case DomesticHotWaterStorageResult(_) =>
                case _ => fail("unexpected result")
              }
          }
        resultListener.expectNoMessage()
        scheduler.expectMessage(Completion(emAgentActivation, Some(nextTick)))
      }

      /* TICK 43322
        Domestic hot water storage stops discharging
        House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 13.03 kWh
        House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 9.02 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 1.09 kWh
        Heat pump: stays off
       */
      emAgentActivation ! Activation(43322)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 43322.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 43322.toDateTime
                emResult._3 should equalWithTolerance(0.asMegaWatt)
                emResult._4 should equalWithTolerance(0.asMegaVar)
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
                time shouldBe 43322.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.000407775.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(43812)))

      /* TICK 43812
      Storage is empty now.
      Note: One could argue, that the Hp now should be started to continue heating of the house,
      but actually we don't support this. So the house is cooling down now.
      PV: 0.0 kW
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 12.33 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 1.09 kWh
      Heat pump: stays off.
       */
      emAgentActivation ! Activation(43812)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 43812.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 43812.toDateTime
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
                time shouldBe 43812.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  18.35.asDegreeCelsius
                )
              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 43812.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(45618)))

      /* TICK 45618
        House reaches lower temperature.
        PV: 0.0 kW
        House demand heating : requiredDemand = 15.0 kWh, possibleDemand = 15.0 kWh
        House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 1.09 kWh
        Heat pump: turned on to heat the house
       */
      emAgentActivation ! Activation(45618)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 45618.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 45618.toDateTime
                emResult._3 should equalWithTolerance(pRunningHp)
                emResult._4 should equalWithTolerance(qRunningHp)
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
                time shouldBe 45618.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(18.asDegreeCelsius)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(46800)))

      /* TICK 46800
        New weather data should not change behaviour.
        PV: 0.0 kW
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 13.13 kWh
        House demand water   : requiredDemand = 0.12 kWh, possibleDemand = 0.12 kWh
        ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 1.09 kWh
        Heat pump: stays on
       */
      emAgentActivation ! Activation(46800)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          46800,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(0),
            WattsPerSquareMeter(0),
            Celsius(-35d),
            MetersPerSecond(0d),
          ),
          Some(57600),
        )
      }

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 46800.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 46800.toDateTime
                emResult._3 should equalWithTolerance(pRunningHp)
                emResult._4 should equalWithTolerance(qRunningHp)
            }
          case ThermalResultEvent(thermalUnitResult) =>
            thermalUnitResult match {
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
                time shouldBe 46800.toDateTime
                qDot should equalWithTolerance(-0.004961490410958902.asMegaWatt)
                energy should equalWithTolerance(0.0004077749589.asMegaWattHour)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(46887)))

      /* We'll jump through a bunch of activations caused from DomesticHotWaterStorage being active.
    The results are checked implicitly through the state of stored energy at the next result check.
       */
      val fourthActivationTicksBlock =
        Seq(46887, 50400, 50449, 54000, 54027, 55261)

      val fourthTickPairs = fourthActivationTicksBlock.zipWithIndex.collect {
        case (tick, index) if index < fourthActivationTicksBlock.length - 1 =>
          (tick, fourthActivationTicksBlock(index + 1))
      }

      fourthTickPairs.foreach { case (currentTick, nextTick) =>
        emAgentActivation ! Activation(currentTick)
        // Each step we expect a message for the Hp and the DomesticHotWaterStorage, but we don't check the content here.
        Range(0, 3)
          .map { _ =>
            resultListener.expectMessageType[ResultEvent]
          }
          .foreach {
            case ParticipantResultEvent(participantResult) =>
              participantResult match {
                case HpResult(_) =>
                case EmResult(_) =>
                case _           => fail("unexpected result")
              }
            case ThermalResultEvent(thermalUnitResult) =>
              thermalUnitResult match {
                case DomesticHotWaterStorageResult(_) =>
                case _ => fail("unexpected result")
              }
          }
        resultListener.expectNoMessage()
        scheduler.expectMessage(Completion(emAgentActivation, Some(nextTick)))
      }

      /* TICK 55261
      House reaches target temperature.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 1.32 kWh
      Heat pump: turned off, storage won't be recharged since EM setPower is zero.
       */

      emAgentActivation ! Activation(55261)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 55261.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 55261.toDateTime
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
                time shouldBe 55261.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(20.asDegreeCelsius)
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(57600)))
    }
  }
}
