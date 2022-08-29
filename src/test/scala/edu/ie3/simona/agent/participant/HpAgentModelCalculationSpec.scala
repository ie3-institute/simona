/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import akka.actor.ActorSystem
import akka.testkit.TestFSMRef
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.system.HpInput
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ApparentPower,
  ApparentPowerAndHeat
}
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.hp.HpAgent
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.DataCollectionStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData._
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.agent.state.ParticipantAgentState.HandleInformation
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.HpRuntimeConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.integration.common.IntegrationSpecCommon
import edu.ie3.simona.model.participant.HpModel.{HpRelevantData, HpState}
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.ontology.messages.PowerMessage.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
  RequestAssetPowerMessage
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  IllegalTriggerMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.{
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage
}
import edu.ie3.simona.ontology.messages.services.WeatherMessage.{
  ProvideWeatherMessage,
  RegisterForWeatherMessage,
  WeatherData
}
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeParticipantAgentTrigger
}
import edu.ie3.simona.test.ParticipantAgentSpec
import edu.ie3.simona.test.common.model.participant.HpTestData
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.quantities.QuantityUtil
import org.scalatest.PrivateMethodTester
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units._

import java.io.File
import java.time.ZonedDateTime
import java.util.concurrent.TimeUnit

class HpAgentModelCalculationSpec
    extends ParticipantAgentSpec(
      ActorSystem(
        "HpAgentSpec",
        ConfigFactory
          .parseString("""
            |akka.loggers =["akka.event.slf4j.Slf4jLogger"]
            |akka.loglevel="DEBUG"
        """.stripMargin)
      )
    )
    with HpTestData
    with IntegrationSpecCommon
    with PrivateMethodTester {
  implicit val simulationStart: ZonedDateTime = defaultSimulationStart
  implicit val receiveTimeOut: Timeout = Timeout(10, TimeUnit.SECONDS)
  implicit val noReceiveTimeOut: Timeout = Timeout(1, TimeUnit.SECONDS)

  /* Alter the input model to have a voltage sensitive reactive power calculation */
  val hpInput: HpInput = inputModel

  private val testingTolerance = 1e-6 // Equality on the basis of 1 W
  private val simonaConfig: SimonaConfig = SimonaConfig(
    ConfigFactory
      .empty()
      .withFallback(ConfigFactory.parseFile(new File(configFile)))
      .resolve()
  )
  private val defaultOutputConfig = NotifierConfig(
    simonaConfig.simona.output.participant.defaultConfig.simulationResult,
    simonaConfig.simona.output.participant.defaultConfig.powerRequestReply
  )
  private val participantConfigUtil = ConfigUtil.ParticipantConfigUtil(
    simonaConfig.simona.runtime.participant
  )
  private val modelConfig =
    participantConfigUtil.getOrDefault[HpRuntimeConfig](
      hpInput.getUuid
    )
  private val noServices = None
  private val services = Some(
    Vector(
      ActorWeatherService(weatherService.ref)
    )
  )
  private val resolution = simonaConfig.simona.powerflow.resolution.getSeconds

  "A heat pump agent depending on no services" should {
    "be instantiated correctly" in {
      val hpAgent = TestFSMRef(
        new HpAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      hpAgent.stateName shouldBe Uninitialized
      // ParticipantUninitializedStateData is an empty class (due to typing). If it contains content one day
      inside(hpAgent.stateData) {
        case _: ParticipantUninitializedStateData[_] => succeed
        case _ =>
          fail(
            s"Expected $ParticipantUninitializedStateData, but got ${hpAgent.stateData}."
          )
      }
    }

    "fail initialisation and stay in uninitialized state" in {
      val hpAgent = TestFSMRef(
        new HpAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val triggerId = 0
      scheduler.send(
        hpAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              HpInput,
              HpRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = hpInput,
              modelConfig = modelConfig,
              secondaryDataServices = noServices,
              simulationStartDate = defaultSimulationStart,
              simulationEndDate = defaultSimulationEnd,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          triggerId,
          hpAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(hpAgent, RegistrationFailedMessage)

      scheduler.receiveOne(receiveTimeOut.duration) match {
        case IllegalTriggerMessage(_, _) => logger.debug("Got correct message")
        case m =>
          fail(
            s"Did not fail initialization because of missing weather service. Received: $m"
          )
      }

      /* agent should stay uninitialized */
      hpAgent.stateName shouldBe Uninitialized
      hpAgent.stateData match {
        case _: ParticipantInitializingStateData[_, _, _] => succeed
        case _ => fail("Expected to get initializing state data")
      }
    }
  }

  "A heat pump agent depending on one secondary data service" should {
    "be instantiated correctly" in {
      val hpAgent = TestFSMRef(
        new HpAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      hpAgent.stateName shouldBe Uninitialized
      // ParticipantUninitializedStateData is an empty class (due to typing). If it contains content one day
      inside(hpAgent.stateData) {
        case _: ParticipantUninitializedStateData[_] => succeed
        case _ =>
          fail(
            s"Expected $ParticipantUninitializedStateData, but got ${hpAgent.stateData}."
          )
      }
    }

    "end in correct state with correct state data after initialisation" in {
      val hpAgent = TestFSMRef(
        new HpAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val triggerId = 0
      scheduler.send(
        hpAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPowerAndHeat,
            ParticipantInitializeStateData[
              HpInput,
              HpRuntimeConfig,
              ApparentPowerAndHeat
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = hpInput,
              thermalGrid = thermalGrid,
              modelConfig = modelConfig,
              secondaryDataServices = services,
              simulationStartDate = defaultSimulationStart,
              simulationEndDate = defaultSimulationEnd,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          triggerId,
          hpAgent
        )
      )

      /* Actor should ask for registration with primary service */
      primaryServiceProxy.expectMsg(
        PrimaryServiceRegistrationMessage(hpInput.getUuid)
      )
      /* State should be information handling and having correct state data */
      hpAgent.stateName shouldBe HandleInformation
      hpAgent.stateData match {
        case ParticipantInitializingStateData(
              inputModel,
              modelConfig,
              secondaryDataServices,
              defaultSimulationStart,
              defaultSimulationEnd,
              resolution,
              requestVoltageDeviationThreshold,
              outputConfig
            ) =>
          inputModel shouldBe WithHeatInputContainer(hpInput, thermalGrid)
          modelConfig shouldBe modelConfig
          secondaryDataServices shouldBe services
          defaultSimulationStart shouldBe this.defaultSimulationStart
          defaultSimulationEnd shouldBe this.defaultSimulationEnd
          resolution shouldBe this.resolution
          requestVoltageDeviationThreshold shouldBe simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold
          outputConfig shouldBe defaultOutputConfig
        case unsuitableStateData =>
          fail(s"Agent has unsuitable state data '$unsuitableStateData'.")
      }

      /* Refuse registration */
      primaryServiceProxy.send(hpAgent, RegistrationFailedMessage)

      /* Expect a registration message */
      weatherService.expectMsg(
        RegisterForWeatherMessage(51.4843281, 7.4116482)
      )

      /* ... as well as corresponding state and state data */
      hpAgent.stateName shouldBe HandleInformation
      hpAgent.stateData match {
        case CollectRegistrationConfirmMessages(
              ParticipantModelBaseStateData(
                startDate,
                endDate,
                _,
                services,
                outputConfig,
                additionalActivationTicks,
                foreseenDataTicks,
                _,
                voltageValueStore,
                resultValueStore,
                requestValueStore,
                _
              ),
              awaitRegistrationResponsesFrom,
              foreseenNextDataTicks
            ) =>
          /* Base state data */
          startDate shouldBe defaultSimulationStart
          endDate shouldBe defaultSimulationEnd
          services shouldBe Some(
            Vector(
              ActorWeatherService(weatherService.ref)
            )
          )
          outputConfig shouldBe NotifierConfig(
            simulationResultInfo = true,
            powerRequestReply = false
          )
          additionalActivationTicks shouldBe Array.emptyLongArray
          foreseenDataTicks shouldBe Map.empty
          voltageValueStore shouldBe ValueStore(
            resolution * 10,
            Map(0L -> Quantities.getQuantity(1d, PU))
          )
          resultValueStore shouldBe ValueStore.forResult(resolution, 10)
          requestValueStore shouldBe ValueStore[ApparentPowerAndHeat](
            resolution * 10
          )

          /* Additional information */
          awaitRegistrationResponsesFrom shouldBe Vector(weatherService.ref)
          foreseenNextDataTicks shouldBe Map.empty
        case _ =>
          fail(
            s"Did not find expected state data $CollectRegistrationConfirmMessages, but ${hpAgent.stateData}"
          )
      }

      /* Reply, that registration was successful */
      weatherService.send(hpAgent, RegistrationSuccessfulMessage(Some(4711L)))

      /* Expect a completion message */
      scheduler.expectMsg(
        CompletionMessage(
          triggerId,
          Some(
            Seq(
              ScheduleTriggerMessage(ActivityStartTrigger(4711), hpAgent)
            )
          )
        )
      )

      /* ... as well as corresponding state and state data */
      hpAgent.stateName shouldBe Idle
      hpAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[_, _, _] =>
          /* Only check the awaited next data ticks, as the rest has yet been checked */
          baseStateData.foreseenDataTicks shouldBe Map(
            weatherService.ref -> Some(4711L)
          )
        case _ =>
          fail(
            s"Did not find expected state data $ParticipantModelBaseStateData, but ${hpAgent.stateData}"
          )
      }
    }

    "answer with zero power, if asked directly after initialisation" in {
      val hpAgent = TestFSMRef(
        new HpAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val triggerId = 0
      scheduler.send(
        hpAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPowerAndHeat,
            ParticipantInitializeStateData[
              HpInput,
              HpRuntimeConfig,
              ApparentPowerAndHeat
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = hpInput,
              thermalGrid = thermalGrid,
              modelConfig = modelConfig,
              secondaryDataServices = services,
              simulationStartDate = defaultSimulationStart,
              simulationEndDate = defaultSimulationEnd,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          triggerId,
          hpAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(hpAgent, RegistrationFailedMessage)

      /* Expect a registration message */
      weatherService.expectMsg(
        RegisterForWeatherMessage(51.4843281, 7.4116482)
      )
      weatherService.send(hpAgent, RegistrationSuccessfulMessage(Some(900L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]

      hpAgent.stateName shouldBe Idle
      /* State data has already been tested */

      hpAgent ! RequestAssetPowerMessage(
        0L,
        Quantities.getQuantity(1d, PU),
        Quantities.getQuantity(0d, PU)
      )
      expectMsg(
        AssetPowerChangedMessage(
          Quantities.getQuantity(0d, MEGAWATT),
          Quantities.getQuantity(0d, MEGAVAR)
        )
      )

      inside(hpAgent.stateData) {
        case modelBaseStateData: ParticipantModelBaseStateData[_, _, _] =>
          modelBaseStateData.requestValueStore shouldBe ValueStore[
            ApparentPowerAndHeat
          ](
            resolution * 10,
            Map(
              0L -> ApparentPowerAndHeat(
                Quantities.getQuantity(0d, MEGAWATT),
                Quantities.getQuantity(0d, MEGAVAR),
                Quantities.getQuantity(0d, MEGAWATT)
              )
            )
          )
        case _ =>
          fail(
            s"Did not find expected state data $ParticipantModelBaseStateData, but ${hpAgent.stateData}"
          )
      }
    }

    "do correct transitions faced to new data in Idle" in {
      val hpAgent = TestFSMRef(
        new HpAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val initialiseTriggerId = 0
      scheduler.send(
        hpAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPowerAndHeat,
            ParticipantInitializeStateData[
              HpInput,
              HpRuntimeConfig,
              ApparentPowerAndHeat
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = hpInput,
              thermalGrid = thermalGrid,
              modelConfig = modelConfig,
              secondaryDataServices = services,
              simulationStartDate = defaultSimulationStart,
              simulationEndDate = defaultSimulationEnd,
              resolution = simonaConfig.simona.powerflow.resolution.getSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          initialiseTriggerId,
          hpAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(hpAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(hpAgent, RegistrationSuccessfulMessage(Some(0L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(hpAgent.stateName shouldBe Idle)
      /* State data is tested in another test */

      /* Send out new data */
      val weatherData = WeatherData(
        Quantities.getQuantity(0, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(0, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(1.815, CELSIUS),
        Quantities.getQuantity(7.726576, METRE_PER_SECOND)
      )

      weatherService.send(
        hpAgent,
        ProvideWeatherMessage(0L, weatherData, Some(3600L))
      )

      /* Find yourself in corresponding state and state data */
      hpAgent.stateName shouldBe HandleInformation
      hpAgent.stateData match {
        case DataCollectionStateData(
              baseStateData: ParticipantModelBaseStateData[_, _, _],
              expectedSenders,
              isYetTriggered
            ) =>
          /* The next data tick is already registered */
          baseStateData.foreseenDataTicks shouldBe Map(
            weatherService.ref -> Some(3600L)
          )

          /* The yet sent data is also registered */
          expectedSenders shouldBe Map(
            weatherService.ref -> Some(weatherData)
          )

          /* It is not yet triggered */
          isYetTriggered shouldBe false
        case _ =>
          fail(
            s"Did not find expected state data $DataCollectionStateData, but ${hpAgent.stateData}"
          )
      }

      /* Trigger the agent */
      scheduler.send(
        hpAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          1L,
          scheduler.ref
        )
      )

      /* The agent will notice, that all expected information are apparent, switch to Calculate and trigger itself
       * for starting the calculation */
      scheduler.expectMsg(
        CompletionMessage(
          1L,
          Some(
            Seq(ScheduleTriggerMessage(ActivityStartTrigger(3600L), hpAgent))
          )
        )
      )
      hpAgent.stateName shouldBe Idle
      hpAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[_, _, _] =>
          /* The store for calculation relevant data has been extended */
          baseStateData.calcRelevantDateStore match {
            case ValueStore(_, store) =>
              store.get(0L) match {
                case Some(
                      HpRelevantData(
                        HpState(
                          isRunning,
                          lastTimeTick,
                          activePower,
                          qDot,
                          thermalGridState
                        ),
                        currentTimeTick,
                        ambientTemperature
                      )
                    ) =>
                  isRunning shouldBe false
                  lastTimeTick shouldBe 0L
                  activePower should equalWithTolerance(
                    Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)
                  )
                  qDot should equalWithTolerance(
                    Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)
                  )

                  thermalGridState.partState.size shouldBe 1
                  thermalGridState.partState.get(thermalHouse.getUuid) match {
                    case Some(ThermalHouseState(_, innerTemperature, _)) =>
                      innerTemperature should equalWithTolerance(
                        Quantities.getQuantity(
                          20.9999769069444444444444444444444,
                          StandardUnits.TEMPERATURE
                        )
                      )
                    case None =>
                      fail(
                        s"Expected to get a result for thermal house '${inputModel.getUuid}'"
                      )
                  }

                  currentTimeTick shouldBe 0L
                  ambientTemperature should equalWithTolerance(
                    Quantities.getQuantity(1.815, StandardUnits.TEMPERATURE)
                  )
                case None =>
                  fail("Did expect to get hp relevant data for tick 0L")
              }
          }

          /* The store for simulation results has been extended */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              store.size shouldBe 1
              store.getOrElse(
                0L,
                fail("Expected a simulation result for tick 900.")
              ) match {
                case ApparentPowerAndHeat(p, q, qDot) =>
                  QuantityUtil.isEquivalentAbs(
                    p,
                    Quantities.getQuantity(0, MEGAWATT),
                    1e-16
                  ) shouldBe true
                  QuantityUtil.isEquivalentAbs(
                    q,
                    Quantities.getQuantity(0, MEGAVAR),
                    1e-16
                  ) shouldBe true
                  QuantityUtil.isEquivalentAbs(
                    qDot,
                    Quantities.getQuantity(0, MEGAWATT),
                    1e-16
                  ) shouldBe true
              }
          }
        case _ =>
          fail(
            s"Did not found the expected state data $ParticipantModelBaseStateData, but ${hpAgent.stateData}"
          )
      }
    }

    "do correct transitions triggered for activation in idle" in {
      val hpAgent = TestFSMRef(
        new HpAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val initialiseTriggerId = 0
      scheduler.send(
        hpAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPowerAndHeat,
            ParticipantInitializeStateData[
              HpInput,
              HpRuntimeConfig,
              ApparentPowerAndHeat
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = hpInput,
              thermalGrid = thermalGrid,
              modelConfig = modelConfig,
              secondaryDataServices = services,
              simulationStartDate = defaultSimulationStart,
              simulationEndDate = defaultSimulationEnd,
              resolution = simonaConfig.simona.powerflow.resolution.getSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          initialiseTriggerId,
          hpAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(hpAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(hpAgent, RegistrationSuccessfulMessage(Some(0L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(hpAgent.stateName shouldBe Idle)

      /* Send out an activity start trigger */
      scheduler.send(
        hpAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          1L,
          scheduler.ref
        )
      )

      /* Find yourself in appropriate state with state data */
      hpAgent.stateName shouldBe HandleInformation
      hpAgent.stateData match {
        case DataCollectionStateData(
              baseStateData: ParticipantModelBaseStateData[_, _, _],
              expectedSenders,
              isYetTriggered
            ) =>
          /* The next data tick is already registered */
          baseStateData.foreseenDataTicks shouldBe Map(
            weatherService.ref -> Some(0L)
          )

          /* The yet sent data is also registered */
          expectedSenders shouldBe Map(weatherService.ref -> None)

          /* It is yet triggered */
          isYetTriggered shouldBe true
        case _ =>
          fail(
            s"Did not find expected state data $DataCollectionStateData, but ${hpAgent.stateData}"
          )
      }

      /* Providing the awaited data will lead to the foreseen transitions */
      val weatherData = WeatherData(
        Quantities.getQuantity(0, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(0, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(1.815, CELSIUS),
        Quantities.getQuantity(7.726576, METRE_PER_SECOND)
      )

      weatherService.send(
        hpAgent,
        ProvideWeatherMessage(0L, weatherData, Some(3600L))
      )

      /* Expect confirmation */
      scheduler.expectMsg(
        CompletionMessage(
          1L,
          Some(
            Seq(ScheduleTriggerMessage(ActivityStartTrigger(3600L), hpAgent))
          )
        )
      )

      /* Expect the state change to idle with updated base state data */
      hpAgent.stateName shouldBe Idle
      hpAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[_, _, _] =>
          /* The store for calculation relevant data has been extended */
          baseStateData.calcRelevantDateStore match {
            case ValueStore(_, store) =>
              store.get(0L) match {
                case Some(
                      HpRelevantData(
                        HpState(
                          isRunning,
                          lastTimeTick,
                          activePower,
                          qDot,
                          thermalGridState
                        ),
                        currentTimeTick,
                        ambientTemperature
                      )
                    ) =>
                  isRunning shouldBe false
                  lastTimeTick shouldBe 0L
                  activePower should equalWithTolerance(
                    Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)
                  )
                  qDot should equalWithTolerance(
                    Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)
                  )
                  thermalGridState.partState.size shouldBe 1
                  thermalGridState.partState.get(thermalHouse.getUuid) match {
                    case Some(ThermalHouseState(_, innerTemperature, _)) =>
                      innerTemperature should equalWithTolerance(
                        Quantities.getQuantity(
                          20.9999769069444444444444444444444,
                          StandardUnits.TEMPERATURE
                        )
                      )
                    case None =>
                      fail(
                        s"Expected to get a result for thermal house '${inputModel.getUuid}'"
                      )
                  }

                  currentTimeTick shouldBe 0L
                  ambientTemperature should equalWithTolerance(
                    Quantities.getQuantity(1.815, StandardUnits.TEMPERATURE)
                  )
                case None =>
                  fail("Did expect to get hp relevant data for tick 0L")
              }
          }

          /* The store for simulation results has been extended */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              store.size shouldBe 1
              store.getOrElse(
                0L,
                fail("Expected a simulation result for tick 0.")
              ) match {
                case ApparentPowerAndHeat(p, q, qDot) =>
                  QuantityUtil.isEquivalentAbs(
                    p,
                    Quantities.getQuantity(0, MEGAWATT),
                    1e-16
                  ) shouldBe true
                  QuantityUtil.isEquivalentAbs(
                    q,
                    Quantities.getQuantity(0, MEGAVAR),
                    1e-16
                  ) shouldBe true
                  QuantityUtil.isEquivalentAbs(
                    qDot,
                    Quantities.getQuantity(0, MEGAWATT),
                    1e-16
                  ) shouldBe true
              }
          }
        case _ =>
          fail(
            s"Did not found the expected state data $ParticipantModelBaseStateData, but ${hpAgent.stateData}"
          )
      }
    }

    "does not provide power if data is awaited in an earlier tick, but answers it, if all expected data is there" in {
      val hpAgent = TestFSMRef(
        new HpAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      /* Trigger the initialisation */
      scheduler.send(
        hpAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPowerAndHeat,
            ParticipantInitializeStateData[
              HpInput,
              HpRuntimeConfig,
              ApparentPowerAndHeat
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = hpInput,
              thermalGrid = thermalGrid,
              modelConfig = modelConfig,
              secondaryDataServices = services,
              simulationStartDate = defaultSimulationStart,
              simulationEndDate = defaultSimulationEnd,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          0L,
          hpAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(hpAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(hpAgent, RegistrationSuccessfulMessage(Some(3600L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(hpAgent.stateName shouldBe Idle)

      /* Ask the agent for average power in tick 7200 */
      hpAgent ! RequestAssetPowerMessage(
        7200L,
        Quantities.getQuantity(1d, PU),
        Quantities.getQuantity(0d, PU)
      )
      expectNoMessage(noReceiveTimeOut.duration)
      awaitAssert(hpAgent.stateName == Idle)

      /* Send out the expected data and wait for the reply */
      val weatherData = WeatherData(
        Quantities.getQuantity(0, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(0, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(1.815, CELSIUS),
        Quantities.getQuantity(7.726576, METRE_PER_SECOND)
      )
      weatherService.send(
        hpAgent,
        ProvideWeatherMessage(3600L, weatherData, Some(7200L))
      )

      /* Trigger the agent */
      scheduler.send(
        hpAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(3600L),
          1L,
          scheduler.ref
        )
      )

      /* The agent will notice, that all expected information are apparent, switch to Calculate and trigger itself
       * for starting the calculation */
      scheduler.expectMsg(
        CompletionMessage(
          1L,
          Some(
            Seq(ScheduleTriggerMessage(ActivityStartTrigger(7200L), hpAgent))
          )
        )
      )

      /* Appreciate the answer to my previous request */
      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(0d, MEGAWATT),
            testingTolerance
          )
          q should equalWithTolerance(
            Quantities.getQuantity(0d, MEGAVAR),
            testingTolerance
          )
      }
    }

    val hpAgent = TestFSMRef(
      new HpAgent(
        scheduler = scheduler.ref,
        listener = systemListener
      )
    )

    "provide correct average power after three data ticks are available" in {
      /* Trigger the initialisation */
      scheduler.send(
        hpAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPowerAndHeat,
            ParticipantInitializeStateData[
              HpInput,
              HpRuntimeConfig,
              ApparentPowerAndHeat
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = hpInput,
              thermalGrid = thermalGrid,
              modelConfig = modelConfig,
              secondaryDataServices = services,
              simulationStartDate = defaultSimulationStart,
              simulationEndDate = defaultSimulationEnd,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          0L,
          hpAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(hpAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(hpAgent, RegistrationSuccessfulMessage(Some(0L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(hpAgent.stateName shouldBe Idle)

      /* Send out the three data points */
      /* ... for tick 0 */
      weatherService.send(
        hpAgent,
        ProvideWeatherMessage(
          0L,
          WeatherData(
            Quantities.getQuantity(0, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(0, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(1.815, CELSIUS),
            Quantities.getQuantity(7.726576, METRE_PER_SECOND)
          ),
          Some(3600L)
        )
      )
      scheduler.send(
        hpAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          1L,
          scheduler.ref
        )
      )
      scheduler.expectMsg(
        CompletionMessage(
          1L,
          Some(
            Seq(ScheduleTriggerMessage(ActivityStartTrigger(3600L), hpAgent))
          )
        )
      )

      /* ... for tick 3600 */
      weatherService.send(
        hpAgent,
        ProvideWeatherMessage(
          3600L,
          WeatherData(
            Quantities.getQuantity(0, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(0, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(1.815, CELSIUS),
            Quantities.getQuantity(7.726576, METRE_PER_SECOND)
          ),
          Some(7200L)
        )
      )
      scheduler.send(
        hpAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(3600L),
          3L,
          scheduler.ref
        )
      )
      scheduler.expectMsg(
        CompletionMessage(
          3L,
          Some(
            Seq(ScheduleTriggerMessage(ActivityStartTrigger(7200L), hpAgent))
          )
        )
      )

      /* ... for tick 7200 */
      weatherService.send(
        hpAgent,
        ProvideWeatherMessage(
          7200L,
          WeatherData(
            Quantities.getQuantity(0, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(0, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(1.815, CELSIUS),
            Quantities.getQuantity(7.726576, METRE_PER_SECOND)
          ),
          None
        )
      )
      scheduler.send(
        hpAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(7200L),
          5L,
          scheduler.ref
        )
      )
      scheduler.expectMsg(CompletionMessage(5L))

      /* Ask the agent for average power in tick 7500 */
      hpAgent ! RequestAssetPowerMessage(
        7500L,
        Quantities.getQuantity(1d, PU),
        Quantities.getQuantity(0d, PU)
      )

      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(0d, MEGAWATT),
            testingTolerance
          )
          q should equalWithTolerance(
            Quantities.getQuantity(0d, MEGAVAR),
            testingTolerance
          )
        case answer => fail(s"Did not expect to get that answer: $answer")
      }
    }

    "answer unchanged power values after asking a second time with considerably same voltage" in {
      /* Previous request stems from previous test */
      /* Ask again with (nearly) unchanged information */
      hpAgent ! RequestAssetPowerMessage(
        7500L,
        Quantities.getQuantity(1.000000000000001d, PU),
        Quantities.getQuantity(0d, PU)
      )

      /* Expect, that nothing has changed */
      expectMsgType[AssetPowerUnchangedMessage] match {
        case AssetPowerUnchangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(0d, MEGAWATT),
            testingTolerance
          )
          q should equalWithTolerance(
            Quantities.getQuantity(0d, MEGAVAR),
            testingTolerance
          )
      }
    }

    "answer changed power values after asking a second time with different voltage" in {
      /* Ask again with changed information */
      hpAgent ! RequestAssetPowerMessage(
        7500L,
        Quantities.getQuantity(0.98, PU),
        Quantities.getQuantity(0d, PU)
      )

      /* Expect, the correct values (this model has fixed power factor) */
      expectMsgClass(classOf[AssetPowerChangedMessage]) match {
        case AssetPowerChangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(0d, MEGAWATT),
            testingTolerance
          )
          q should equalWithTolerance(
            Quantities.getQuantity(0d, MEGAVAR),
            testingTolerance
          )
      }
    }
  }
}
