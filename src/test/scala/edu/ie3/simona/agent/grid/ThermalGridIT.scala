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
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.ServiceType
import edu.ie3.simona.test.common.input.{
  EmInputTestData,
  ThermalGridITInputTestData,
}
import edu.ie3.simona.test.common.{DefaultTestData, TestSpawnerTyped}
import edu.ie3.simona.util.SimonaConstants.{INIT_SIM_TICK, PRE_INIT_TICK}
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.QuantityMatchers.equalWithTolerance
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
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
    with EmInputTestData
    with MockitoSugar
    with DefaultTestData
    with TestSpawnerTyped {
  private implicit val quantityTolerance: Double = 1e-10d
  protected val temperatureTolerance: Double = 0.01

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
          typicalHpInputContainer,
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
      ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
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
                // FIXME We do make a significant error here since discharging little time frames with high power at a resolution of one second makes error. Maybe one can create this two staged.
                //  First calculate the duration at full power, take the next second and lower the power to the value matching the demand.
                // Done: But maybe additionally some separate test just for this, thus we test this before the integration test....
                energy should equalWithTolerance(0.00149814.asMegaWattHour)
            }
        }
      scheduler.expectMessage(Completion(heatPumpAgent, Some(49)))
      resultListener.expectNoMessage()

      /* TICK 49
      Domestic hot water storage stops discharging
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 10.37 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
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
                energy should equalWithTolerance(0.00143069474.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(3416)))
      resultListener.expectNoMessage()

      /* TICK 3416
      ThermalStorage is fully heated up
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 2.36 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
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
            }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(3600)))
      resultListener.expectNoMessage()

      /* TICK 3600
      New weather data (unchanged) incoming + Domestic hot water storage will cover hot water demand
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.92 kWh
      House demand water   : requiredDemand = 0.037 kWh, possibleDemand = 0.037 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
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
                energy should equalWithTolerance(0.00143069464.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(3627)))
      resultListener.expectNoMessage()

      /* TICK 3627
      Domestic hot water storage will stop discharging.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.90 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
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
                energy should equalWithTolerance(0.00139322515.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(4381)))
      resultListener.expectNoMessage()

      scheduler.expectMessage(Completion(heatPumpAgent, Some(4412)))

      /* TICK 4254
      House reaches target temperature boundary
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
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
                )(temperatureTolerance)
            }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(7200)))
      resultListener.expectNoMessage()

      /* We'll jump through a bunch of activations caused from DomesticHotWaterStorage being active.
      The results are checked implicitly through the state of stored energy at the next result check.
       */

      val firstActivationTicksBlock =
        Seq(7200, 7217, 10800, 10809, 14400, 14411, 18000, 18017, 21600)

      val firstTickPairs = firstActivationTicksBlock.zipWithIndex.collect {
        case (tick, index) if index < firstActivationTicksBlock.length - 1 =>
          (tick, firstActivationTicksBlock(index + 1))
      }

      firstTickPairs.foreach { case (currentTick, nextTick) =>
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
      House demand water   : requiredDemand = 0.16 kWh, possibleDemand = 0.16 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
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
            hpResult.getP should equalWithTolerance(0.0.asMegaWatt)
            hpResult.getQ should equalWithTolerance(0.0.asMegaVar)
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
                qDot should equalWithTolerance(
                  -0.004980573066385669.asMegaWatt
                )
                energy should equalWithTolerance(
                  // FIXME Check this result
                  0.001322032931506849.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(21665)))
      resultListener.expectNoMessage()

      /* TICK 21665
     Domestic storage stops discharging
     House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 12.0 kWh
     House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
     ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
     DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
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
            hpResult.getP should equalWithTolerance(0.0.asMegaWatt)
            hpResult.getQ should equalWithTolerance(0.0.asMegaVar)
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
                  0.001232105917808219.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(23209)))
      resultListener.expectNoMessage()

      /* TICK 23209
      House reach lowerTemperatureBoundary
      House demand heating : requiredDemand = 15.0 kWh, possibleDemand = 15.00 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays off, demand should be covered by storage
       */

      heatPumpAgent ! Activation(23209)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 23209.toDateTime
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
                time shouldBe 23209.toDateTime
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
                time shouldBe 23209.toDateTime
                qDot should equalWithTolerance(-0.01044.asMegaWatt)
                energy should equalWithTolerance(0.01044.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(25000)))
      resultListener.expectNoMessage()

      /* TICK 25000
      Additional trigger caused by (unchanged) weather data should not change this
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 13.34 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 5.5 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
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
          Some(28800),
        )
      }
      resultListener.expectMessageType[ResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 25000.toDateTime
          hpResult.getP should equalWithTolerance(0.0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.0.asMegaVar)
      }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(25200)))
      resultListener.expectNoMessage()

      /* TICK 25200
      DomesticHotWaterStorage
      House demand heating : requiredDemand = 0.0kWh, possibleDemand = 11.87 kWh
      House demand water   : requiredDemand = 0.18 kWh, possibleDemand = 0.18 kWh
      ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: turned on - to serve the remaining heat demand of house (and refill storage later)
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
                energy should equalWithTolerance(0.0012321059.asMegaWattHour)

            }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(25327)))
      resultListener.expectNoMessage()

      /* TICK 25327
      DomesticHotWaterStorage stops discharging.
      House demand heating : requiredDemand = 0.0kWh, possibleDemand = 11.87 kWh
      House demand water   : requiredDemand = 0.18 kWh, possibleDemand = 0.18 kWh
      ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
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
                energy should equalWithTolerance(
                  0.0010559988493150684.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(26809)))
      resultListener.expectNoMessage()

      /* TICK 26809
      Heat storage is empty
      House demand heating : requiredDemand = 0.0kWh, possibleDemand = 11.87 kWh
      House demand water   : requiredDemand = 0.18 kWh, possibleDemand = 0.18 kWh
      ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: turned on - to serve the remaining heat demand of house (and refill storage later)
       */

      heatPumpAgent ! Activation(26809)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 26809.toDateTime
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
                time shouldBe 26809.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  18.42.asDegreeCelsius
                )(temperatureTolerance)
              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 26809.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(0.asMegaWattHour)
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 226809.toDateTime
                qDot should equalWithTolerance(-0.01093078356.asMegaWatt)
                energy should equalWithTolerance(0.0012321059.asMegaWattHour)
            }
        }
      scheduler.expectMessage(Completion(heatPumpAgent, Some(28800)))
      resultListener.expectNoMessage()

      /* TICK 28800
        New weather data: it's getting warmer again
        FIXME
        House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 10.75 kWh
        House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        Heat pump: stays on
       */

      heatPumpAgent ! Activation(28800)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          28800,
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
                qDot should equalWithTolerance(-0.004985084455032759.asMegaWatt)
                energy should equalWithTolerance(
                  0.0010559988493150684.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(28984)))
      resultListener.expectNoMessage()

      /* We'll jump through a bunch of activations caused from DomesticHotWaterStorage being active.
The results are checked implicitly through the state of stored energy at the next result check.
       */

      val secondActivationTicksBlock = Seq(28984, 32400, 32458)

      val secondTickPairs = secondActivationTicksBlock.zipWithIndex.collect {
        case (tick, index) if index < secondActivationTicksBlock.length - 1 =>
          (tick, secondActivationTicksBlock(index + 1))
      }

      secondTickPairs.foreach { case (currentTick, nextTick) =>
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

      /* TICK 32458
        House will reach the upperTemperatureBoundary
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        House demand water   : requiredDemand = 0.47 kWh, possibleDemand = 0.47 kWh
        ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        Heat pump: stays on to recharge the ThermalStorage now
       */

      heatPumpAgent ! Activation(32458)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 32458.toDateTime
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
                time shouldBe 32458.toDateTime
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
                time shouldBe 32458.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                energy should equalWithTolerance(
                  0.asMegaWattHour
                )
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 32458.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.000720767681907133.asMegaWattHour
                )
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(35874)))

      /* TICK 35874
      Storage full? House will reach the upperTemperatureBoundary
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 9.97 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays on to recharge the storage now
       */

      heatPumpAgent ! Activation(35874)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 35874.toDateTime
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
                time shouldBe 35874.toDateTime
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
                time shouldBe 35874.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.01044.asMegaWattHour
                )
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(36000)))

      /* We'll jump through a bunch of activations caused from DomesticHotWaterStorage being active.
The results are checked implicitly through the state of stored energy at the next result check.
       */

      val thirdActivationTicksBlock = Seq(36000, 36165, 36409)

      val thirdTickPairs = thirdActivationTicksBlock.zipWithIndex.collect {
        case (tick, index) if index < thirdActivationTicksBlock.length - 1 =>
          (tick, thirdActivationTicksBlock(index + 1))
      }

      thirdTickPairs.foreach { case (currentTick, nextTick) =>
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

      /* TICK 36409
        Storage will be fully charged, but meanwhile the house cooled a bit
        House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 1.42 kWh
        House demand water   : requiredDemand = 0.21 kWh, possibleDemand = 0.21 kWh
        ThermalStorage       : requiredDemand = 0.00 kWh, possibleDemand = 0.00 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        Heat pump: stays on
       */

      heatPumpAgent ! Activation(36409)

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 36409.toDateTime
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
                time shouldBe 36409.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.99.asDegreeCelsius
                )(temperatureTolerance)
            }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(39600)))
      resultListener.expectNoMessage()

      /* We'll jump through a bunch of activations caused from DomesticHotWaterStorage being active.
The results are checked implicitly through the state of stored energy at the next result check.
       */

      val fourthActivationTicksBlock =
        Seq(39600, 39765, 43200, 43370, 46800, 46820)

      val fourthTickPairs = fourthActivationTicksBlock.zipWithIndex.collect {
        case (tick, index) if index < fourthActivationTicksBlock.length - 1 =>
          (tick, fourthActivationTicksBlock(index + 1))
      }

      fourthTickPairs.foreach { case (currentTick, nextTick) =>
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

      /* TICK 46820
      DomesticHotWaterStorage is empty and needs to get recharged. FIXME
      House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 1.42 kWh
      House demand water   : requiredDemand = 0.21 kWh, possibleDemand = 0.21 kWh
      ThermalStorage       : requiredDemand = 0.00 kWh, possibleDemand = 0.00 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays on
       */

      heatPumpAgent ! Activation(46820)

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 46820.toDateTime
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
                time shouldBe 46820.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                energy should equalWithTolerance(
                  0.asMegaWattHour
                )
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(47310)))

      /* TICK 47310
      DomesticHotWaterStorage is full.
      House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 1.42 kWh
      House demand water   : requiredDemand = 0.21 kWh, possibleDemand = 0.21 kWh
      ThermalStorage       : requiredDemand = 0.00 kWh, possibleDemand = 0.00 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays on to serve possibleDemand of thermalHouse
       */

      heatPumpAgent ! Activation(47310)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 47310.toDateTime
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
                time shouldBe 47310.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.40.asDegreeCelsius
                )(temperatureTolerance)
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 47310.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.0014981399999999998.asMegaWattHour
                )
            }
        }

      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(48990)))

      /* TICK 48990
      Storage will be fully charged, but meanwhile the house cooled a bit
      House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 1.42 kWh
      House demand water   : requiredDemand = 0.21 kWh, possibleDemand = 0.21 kWh
      ThermalStorage       : requiredDemand = 0.00 kWh, possibleDemand = 0.00 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays on
       */

      heatPumpAgent ! Activation(48990)

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 48990.toDateTime
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
                time shouldBe 48990.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.99.asDegreeCelsius
                )(temperatureTolerance)
            }
        }

      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(50400)))

      /* We'll jump through a bunch of activations caused from DomesticHotWaterStorage being active.
      The results are checked implicitly through the state of stored energy at the next result check.
       */

      val fifthActivationTicksBlock =
        Seq(50400, 50538, 54000, 54119, 57600, 57717, 61200, 61327, 64800,
          64954, 68400, 68576, 72000, 72179, 75600, 75673, 76163)

      val fifthTickPairs = fifthActivationTicksBlock.zipWithIndex.collect {
        case (tick, index) if index < fifthActivationTicksBlock.length - 1 =>
          (tick, fifthActivationTicksBlock(index + 1))
      }

      fifthTickPairs.foreach { case (currentTick, nextTick) =>
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

      /* TICK 76183
      House reaches lower temperature boundary. DomesticWaterStorage has also required demand.
      House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 15.0 kWh
      House demand water   : requiredDemand = 2.1 kWh, possibleDemand = 2.1 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 3.2 kWh, possibleDemand = 3.2 kWh
      Heat pump: turned on - to serve demand of DomesticWaterStorage and house.
       */

      heatPumpAgent ! Activation(76163)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 76163.toDateTime
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
                time shouldBe 76163.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  18.55.asDegreeCelsius
                )(temperatureTolerance)
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 76163.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.0014981399999999998.asMegaWattHour
                )
            }
        }
      resultListener.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(79200)))
    }
  }

  // TODO in the end, check if there is a case where domestic storage and house splits the thermal qDot of Hp

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
          typicalHpInputContainer,
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

      scheduler.expectMessage(Completion(emAgentActivation, Some(0)))

      val weatherDependentAgents = Seq(hpAgent.toClassic, pvAgent.toClassic)

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

              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 0.toDateTime
                qDot should equalWithTolerance(-0.004986861668742217.asMegaWatt)
                energy should equalWithTolerance(
                  0.0014981399999999998.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(165)))

      /* TICK 165
      Domestic hot water storage stops discharging.
  PV: 0.0 kW
  House demand heating : requiredDemand = 0.0 kWh, additionalDemand 0.0 kWh
  House demand water   : tba
  ThermalStorage       : requiredDemand = 10.44 kWh, additionalDemand = 10.44 kWh
  DomesticWaterStorage : tba
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
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 165.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.001269575506849315.asMegaWattHour
                )
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

      scheduler.expectMessage(Completion(emAgentActivation, Some(3600)))

      /* TICK 3600
      New Weather: The sun comes out, PV will produce.
      PV: -6.3 kW
      House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 1.25 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 10.44 kWh, additionalDemand = 10.44 kWh
      DomesticWaterStorage : tba
      Heat pump: turns on - since now we have flexibility potential available which can be used by hp to serve the reqDemand of ThermalStorage
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
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 3600.toDateTime
                qDot should equalWithTolerance(-0.004986861668742217.asMegaWatt)
                energy should equalWithTolerance(
                  0.001269575506849315.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(3765)))

      /* TICK 3765
New Weather: The sun comes out, PV will produce.
PV: -6.3 kW
House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 1.25 kWh
House demand water   : tba
ThermalStorage       : requiredDemand = 10.44 kWh, additionalDemand = 10.44 kWh
DomesticWaterStorage : tba
Heat pump: turns on - since now we have flexibility potential available which can be used by hp to serve the reqDemand of ThermalStorage
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
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 3765.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.00104101101369863.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(5216)))

      /* TICK 5216
      Storage is fully heated up, meanwhile house cooled a bit.
      PV: -6.3 kW
      House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 3.59 kWh
      House demand water   : tba
      ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
      DomesticWaterStorage : tba
      Heat pump: stays on since it was on and the house has additional demand
       */

      emAgentActivation ! Activation(5216)

      Range(0, 5)
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
                emResult._4 should equalWithTolerance(
                  -0.000827482453922.asMegaVar
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
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 5216.toDateTime
                qDot should equalWithTolerance(-0.010971095671.asMegaWatt)
                energy should equalWithTolerance(
                  0.001269575506849315.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(5291)))

      /* TICK 5291
Domestic storage stops discharging.
PV: -6.3 kW
House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 3.6 kWh
House demand water   : tba
ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
DomesticWaterStorage : tba
Heat pump: stays on since it was on and the house has additional demand
       */

      emAgentActivation ! Activation(5291)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 5291.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 5291.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.0025175615153993284.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  -0.000827482453922.asMegaVar
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
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 5291.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.00104101101369863.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(5400)))

      /* TICK 5400
      PV: 0.0 kW
      New weather data, sun is gone again, thus we should now heat the house by storage.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 3.15 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
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

      scheduler.expectMessage(Completion(emAgentActivation, Some(6824)))

      /* TICK 6824
     The house reaches target temperature
     PV: 0.0 kW
     House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
     ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 4.13 kWh
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
                time shouldBe 6824.toDateTime
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
                time shouldBe 6824.toDateTime
                qDot should equalWithTolerance(0.0.asMegaWatt)
                energy should equalWithTolerance(0.0063104.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(9200)))

      /* TICK 9200
     The sun is back again, storage first.
     PV: -5.2 kW
     House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.64 kWh
     ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 4.13 kWh
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
                  -0.000444643226783.asMegaVar
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
                time shouldBe 9200.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                energy should equalWithTolerance(
                  0.0063104.asMegaWattHour
                )
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 9200.toDateTime
                qDot should equalWithTolerance(-0.01089500358271865.asMegaWatt)
                energy should equalWithTolerance(
                  0.00104101101369863.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(9278)))

      /* TICK 9278
      Domestic hot water storage stops discharging.
      PV: -5.2 kW
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.64 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 4.15 kWh
      Heat pump: turned on
       */

      emAgentActivation ! Activation(9278)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 9278.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 9278.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.0013527980811294546.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  -0.00044464322678371.asMegaVar
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
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 9278.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.0008049526027397.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(10551)))

      /* TICK 10551
      Storage is full, now heating the house till target temperature.
      PV: -5.2 kW
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 2.57 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
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
                emResult._3 should equalWithTolerance(
                  -0.0013527980811294546.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  -0.0004446432267837.asMegaVar
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
                time shouldBe 10551.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.65.asDegreeCelsius
                )(temperatureTolerance)
              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 10551.toDateTime
                qDot should equalWithTolerance(0.0.asMegaWatt)
                energy should equalWithTolerance(0.01044.asMegaWattHour)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(11638)))

      /* TICK 11638
      House reaches target temperature boundary.
      PV: -5.2 kW
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: turned off
       */

      emAgentActivation ! Activation(11638)

      Range(0, 4)
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
                emResult._3 should equalWithTolerance(
                  -0.005152798081129455.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  -0.0016936428264633983.asMegaVar
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
                time shouldBe 11638.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  19.99.asDegreeCelsius
                )(temperatureTolerance)
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 11644.toDateTime
                qDot should equalWithTolerance(-0.01092783963932.asMegaWatt)
                energy should equalWithTolerance(
                  0.0008049526027397.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(11723)))

      /* TICK 11723
      Domestic hot water storage stops discharging.
      PV: -5.2 kW
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: turned off
       */

      emAgentActivation ! Activation(11723)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 11723.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 11723.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.005152798081129455.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  -0.0016936428264633983.asMegaVar
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
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 11723.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.000565147232876712.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(12000)))

      /* TICK 12000
      House would reach lowerTempBoundary at tick 23809,
      but now it's getting colder which should decrease inner temp of house faster, but the sun is still there.
      PV: -5.2 kW
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.25 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
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
                emResult._3 should equalWithTolerance(
                  -0.0014021250411259763.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  -0.00046085621449.asMegaVar
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
                  19.96.asDegreeCelsius
                )(temperatureTolerance)
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(12139)))

      /* TICK 12139
      PV: 0.0 kW
      House reaches the target temperature.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
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
                emResult._3 should equalWithTolerance(
                  -0.005202125041125976.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  -0.001709855814171.asMegaVar
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
                time shouldBe 12139.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  20.asDegreeCelsius
                )(temperatureTolerance)
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

      scheduler.expectMessage(Completion(emAgentActivation, Some(24152)))

      /* TICK 24152
        House reaches lower boundary, since we don't have surplus energy from pv, we would use the energy from storage to heat the house.
        PV: 0.0 kW
        House demand heating : requiredDemand = 15.0 kWh, possibleDemand = 15.0 kWh
        ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        Heat pump: stays off
       */

      emAgentActivation ! Activation(24152)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 24152.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 24152.toDateTime
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
                time shouldBe 24152.toDateTime
                qDot should equalWithTolerance(0.0055.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  18.asDegreeCelsius
                )(temperatureTolerance)
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 24152.toDateTime
                // FIXME, check if we consider the demand of the house for hot water here correct?
                qDot should equalWithTolerance(0.0055.asMegaWatt)
                energy should equalWithTolerance(
                  0.0005651472328767.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(24762)))

      /* TICK 24762
      Domestic hot water storage is now fully charged.
      PV: 0.0 kW
      House demand heating : requiredDemand = 15.0 kWh, possibleDemand = 15.0 kWh
      ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: turned off, since we setPower is below 50 % of sRated and we can continue heating the house from storage. FIXME: Check if the demand of the domestic storage realy comes the tick before and not way earlier...
       */

      emAgentActivation ! Activation(24762)

      Range(0, 5)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 24762.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 24762.toDateTime
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
                time shouldBe 24762.toDateTime
                qDot should equalWithTolerance(0.01044.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  18.02.asDegreeCelsius
                )(temperatureTolerance)
              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 24762.toDateTime
                qDot should equalWithTolerance(-0.01044.asMegaWatt)
                energy should equalWithTolerance(0.01044.asMegaWattHour)
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 24762.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.00149814.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(25200)))

      /* TICK 25200
        The sun comes out and it's getting warmer.
        PV: -4.4 kW
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 13.66 kWh
        ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 2.28 kWh
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
                time shouldBe 25200.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  18.13.asDegreeCelsius
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
                  0.009169799999999999.asMegaWattHour
                )
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 25200.toDateTime
                qDot should equalWithTolerance(-0.010930783561.asMegaWatt)
                energy should equalWithTolerance(
                  0.00149814.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(25258)))

      /* TICK 25258
Domestic hot water storage stops discharging.
  PV: -4.4 kW
  House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 13.21 kWh
  ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 3.04 kWh
  Heat pump: will be turned on and will continue heating the house
       */

      emAgentActivation ! Activation(25258)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 25258.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 25258.toDateTime
                emResult._3 should equalWithTolerance(
                  -0.000557218282208516.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  -0.000183148792477.asMegaVar
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
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 25258.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.0013220329315.asMegaWattHour
                )
            }
        }
      scheduler.expectMessage(Completion(emAgentActivation, Some(27500)))

      /* TICK 27500
        Additional trigger caused by (unchanged) weather data should not change this.
        PV: -3.9 kW
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 8.14 kWh
        ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 2.28 kWh
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
              emResult._3 should equalWithTolerance(
                -0.00006389649707132048.asMegaWatt
              )
              emResult._4 should equalWithTolerance(
                -0.00002100176296395.asMegaVar
              )
          }
        }

      // FIXME? This was 30708 before
      scheduler.expectMessage(Completion(emAgentActivation, Some(31000)))

      /* TICK 31000
        House reaches target temperature, since Hp is running we now charge the storage.
        PV: -3.9 kW
        The sun is gone again, it's getting colder as well.
        PV: 0.0 kW
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 2.28 kWh
        FIXME Heat pump: stays on - to serve the remaining heat demand of the storage.
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
          Some(145000),
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
                  19.98.asDegreeCelsius
                )(temperatureTolerance)
              case CylindricalThermalStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe typicalThermalStorage.getUuid
                time shouldBe 31000.toDateTime
                qDot should equalWithTolerance(-0.01044.asMegaWatt)
                energy should equalWithTolerance(
                  0.009169799999999999.asMegaWattHour
                )
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 31000.toDateTime
                qDot should equalWithTolerance(-0.010983942387.asMegaWatt)
                energy should equalWithTolerance(
                  0.001322032931506849.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(31070)))

      /* TICK 31070
FIXME
  PV: 0.0 kW
  House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
  ThermalStorage       : requiredDemand = 0.0 kWh, possibleDemand = 3.04 kWh
  Heat pump: stays on - to serve the remaining heat demand of the storage.
       */

      emAgentActivation ! Activation(31070)

      Range(0, 5)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 31070.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 31070.toDateTime
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
                time shouldBe 31070.toDateTime
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
                time shouldBe 31070.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.008966799999999999.asMegaWattHour
                )
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 31070.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.0011084562739726025.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(40868)))

      /* TICK 40868
        House reach lower temperature boundary
        PV: 0.0 kW
        House demand heating : requiredDemand = 15.00 kWh, possibleDemand = 15.00 kWh
        ThermalStorage       : requiredDemand = 0.00 kWh, possibleDemand = 2.05 kWh
        Heat pump: stays off - demand will be covered by storage.
       */

      emAgentActivation ! Activation(40868)

      Range(0, 5)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 40868.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 40868.toDateTime
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
                time shouldBe 40868.toDateTime
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
                time shouldBe 40868.toDateTime
                qDot should equalWithTolerance(-0.01044.asMegaWatt)
                energy should equalWithTolerance(
                  0.008966799999999999.asMegaWattHour
                )
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 40868.toDateTime
                qDot should equalWithTolerance(-0.010988926027397261.asMegaWatt)
                energy should equalWithTolerance(
                  0.0011084562739726025.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(41100)))

      /* TICK 41100
Domestic hot water storage stops discharging
  PV: 0.0 kW
  House demand heating : requiredDemand = 15.00 kWh, possibleDemand = 15.00 kWh
  ThermalStorage       : requiredDemand = 0.00 kWh, possibleDemand = 2.15 kWh
  Heat pump: stays off - demand will be covered by storage.
       */

      emAgentActivation ! Activation(41100)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 41100.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 41100.toDateTime
                emResult._3 should equalWithTolerance(
                  0.0.asMegaWatt
                )
                emResult._4 should equalWithTolerance(
                  0.0.asMegaVar
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
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 41100.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.00040028104109589014.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(43959)))

      /* TICK 43959
        Storage is empty now.
        Note: One could argue, that new weather should not change the operation of an agent (at least,
        if the weather did not change the flexOptions), but so far we don't check for this.
        Thus, the Hp will stop operation since it can be turned off
        (lower Temp < innerTemp < targetTemp && storage must not directly recharged).
        PV: 0.0 kW
        House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 10.89 kWh
        ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        Heat pump: stays off
       */

      emAgentActivation ! Activation(43959)

      Range(0, 5)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 43959.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 43959.toDateTime
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
                time shouldBe 43959.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
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
                time shouldBe 43959.toDateTime
                qDot should equalWithTolerance(0.0.asMegaWatt)
                energy should equalWithTolerance(0.0.asMegaWattHour)
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 43959.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                energy should equalWithTolerance(
                  0.00040028104109589.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(44318)))

      /* TICK 44318
        Domestic hot storage is now fully charged.
        PV: 0.0 kW
        House demand heating : requiredDemand = 0.00 kWh, possibleDemand = 10.92 kWh
        ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        Heat pump: stays off
       */

      emAgentActivation ! Activation(44318)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 44318.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 44318.toDateTime
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
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 44318.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.0014981399999999998.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(46925)))

      /* TICK 46925
        House will reach the lower temperature boundary
        PV: 0.0 kW
        House demand heating : requiredDemand = 15.00 kWh, possibleDemand = 15.00 kWh
        ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        Heat pump: turned on to heat the house
       */

      emAgentActivation ! Activation(46925)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 46925.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)
              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 46925.toDateTime
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
                time shouldBe 46925.toDateTime
                qDot should equalWithTolerance(0.011.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  18.asDegreeCelsius
                )(temperatureTolerance)
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 46925.toDateTime
                qDot should equalWithTolerance(-0.010791241643835612.asMegaWatt)
                energy should equalWithTolerance(
                  0.0014981399999999998.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(46965)))

      /* TICK 46965
  Domestic hot water storage stops discharging.
  PV: 0.0 kW
  House demand heating : requiredDemand = 15.00 kWh, possibleDemand = 15.00 kWh
  ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
  Heat pump: turned on to heat the house
       */

      emAgentActivation ! Activation(46965)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 46965.toDateTime
                hpResult._3 should equalWithTolerance(pRunningHp)
                hpResult._4 should equalWithTolerance(qRunningHp)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 46965.toDateTime
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
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 46965.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.001378237315068493.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(56399)))

      /* TICK 56399
        House will reach target temperature
        PV: 0.0 kW
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        Heat pump: turned off - no surplus energy to recharge the storage now
       */

      emAgentActivation ! Activation(56399)

      Range(0, 4)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 56399.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 56399.toDateTime
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
                time shouldBe 56399.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                indoorTemperature should equalWithTolerance(
                  20.asDegreeCelsius
                )(temperatureTolerance)
              case DomesticHotWaterStorageResult(
                    time,
                    inputModel,
                    qDot,
                    energy,
                  ) =>
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 56399.toDateTime
                qDot should equalWithTolerance(-0.01091970880626223.asMegaWatt)
                energy should equalWithTolerance(
                  0.001378237315068493.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(56441)))

      /* TICK 56441
Domestic hot water storage stops discharging.
  PV: 0.0 kW
  House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
  ThermalStorage       : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
  Heat pump: turned off - no surplus energy to recharge the storage now
       */

      emAgentActivation ! Activation(56441)

      Range(0, 3)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach {
          case ParticipantResultEvent(participantResult) =>
            participantResult match {
              case HpResult(hpResult) =>
                hpResult._2 shouldBe typicalHpInputModel.getUuid
                hpResult._1 shouldBe 56441.toDateTime
                hpResult._3 should equalWithTolerance(0.asMegaWatt)
                hpResult._4 should equalWithTolerance(0.asMegaVar)

              case EmResult(emResult) =>
                emResult._2 shouldBe emInput.getUuid
                emResult._1 shouldBe 56441.toDateTime
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
                inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
                time shouldBe 56441.toDateTime
                qDot should equalWithTolerance(0.asMegaWatt)
                energy should equalWithTolerance(
                  0.001250840712328767.asMegaWattHour
                )
            }
        }

      scheduler.expectMessage(Completion(emAgentActivation, Some(66218)))

    }
  }
}
