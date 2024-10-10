/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestFSMRef
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.datamodel.models.input.system.characteristic.QV
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.pv.PvAgent
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.DataCollectionStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.{
  CollectRegistrationConfirmMessages,
  ParticipantInitializeStateData,
  ParticipantInitializingStateData,
  ParticipantUninitializedStateData
}
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.agent.state.ParticipantAgentState.HandleInformation
import edu.ie3.simona.config.RuntimeConfig.SimpleRuntimeConfig
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.model.participant.PvModel.PvRelevantData
import edu.ie3.simona.model.participant.load.{LoadModelBehaviour, LoadReference}
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
import edu.ie3.simona.test.common.input.PvInputTestData
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.scala.quantities.{
  Megavars,
  ReactivePower,
  Vars,
  WattsPerSquareMeter
}
import org.scalatest.PrivateMethodTester
import squants.{Each, Power}
import squants.energy.{Kilowatts, Megawatts, Watts}
import squants.motion.MetersPerSecond
import squants.thermal.Celsius

import java.util.concurrent.TimeUnit

class PvAgentModelCalculationSpec
    extends ParticipantAgentSpec(
      ActorSystem(
        "PvAgentSpec",
        ConfigFactory
          .parseString("""
            |akka.loggers =["akka.event.slf4j.Slf4jLogger"]
            |akka.loglevel="DEBUG"
        """.stripMargin)
      )
    )
    with PrivateMethodTester
    with PvInputTestData {
  implicit val receiveTimeOut: Timeout = Timeout(10, TimeUnit.SECONDS)
  implicit val noReceiveTimeOut: Timeout = Timeout(1, TimeUnit.SECONDS)

  /* Alter the input model to have a voltage sensitive reactive power calculation */
  val voltageSensitiveInput: PvInput = pvInputModel
    .copy()
    .qCharacteristics(new QV("qV:{(0.95,-0.625),(1.05,0.625)}"))
    .build()

  private val testingTolerance = 1e-6 // Equality on the basis of 1 W

  /* Assign this test to receive the result events from agent */
  override val systemListener: Iterable[ActorRef] = Vector(self)

  private val simonaConfig: SimonaConfig =
    createSimonaConfig(
      LoadModelBehaviour.FIX,
      LoadReference.ActivePower(Kilowatts(0d))
    )
  private val defaultOutputConfig = ParticipantNotifierConfig(
    simonaConfig.output.participant.defaultConfig.simulationResult,
    simonaConfig.output.participant.defaultConfig.powerRequestReply
  )
  private val configUtil = ConfigUtil.ParticipantConfigUtil(
    simonaConfig.runtime.participant
  )
  private val modelConfig = configUtil.getOrDefault[SimpleRuntimeConfig](
    voltageSensitiveInput.getUuid
  )
  private val noServices = None
  private val withServices = Some(
    Vector(
      ActorWeatherService(weatherService.ref)
    )
  )
  private val resolution = simonaConfig.powerflow.resolution.toSeconds

  private implicit val powerTolerance: Power = Watts(0.1)
  private implicit val reactivePowerTolerance: ReactivePower = Vars(0.1)

  "A pv agent with model calculation depending on no secondary data service" should {
    "be instantiated correctly" in {
      val pvAgent = TestFSMRef(
        new PvAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      pvAgent.stateName shouldBe Uninitialized
      // ParticipantUninitializedStateData is an empty class (due to typing). If it contains content one day
      inside(pvAgent.stateData) {
        case _: ParticipantUninitializedStateData[_] => succeed
        case _ =>
          fail(
            s"Expected $ParticipantUninitializedStateData, but got ${pvAgent.stateData}."
          )
      }
    }

    "fail initialisation and stay in uninitialized state" in {
      val pvAgent = TestFSMRef(
        new PvAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val triggerId = 0
      scheduler.send(
        pvAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              PvInput,
              SimpleRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              modelConfig = modelConfig,
              secondaryDataServices = noServices,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          triggerId,
          pvAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(pvAgent, RegistrationFailedMessage)

      scheduler.receiveOne(receiveTimeOut.duration) match {
        case IllegalTriggerMessage(_, _) => logger.debug("Got correct message")
        case m =>
          fail(
            s"Did not fail initialization because of missing weather service. Received: $m"
          )
      }

      /* agent should stay uninitialized */
      pvAgent.stateName shouldBe Uninitialized
      pvAgent.stateData match {
        case _: ParticipantInitializingStateData[_, _, _] => succeed
        case _ => fail("Expected to get initializing state data")
      }
    }
  }

  "A pv agent with model calculation depending on one secondary data service" should {
    "be instantiated correctly" in {
      val pvAgent = TestFSMRef(
        new PvAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      pvAgent.stateName shouldBe Uninitialized
      // ParticipantUninitializedStateData is an empty class (due to typing). If it contains content one day
      inside(pvAgent.stateData) {
        case _: ParticipantUninitializedStateData[_] => succeed
        case _ =>
          fail(
            s"Expected $ParticipantUninitializedStateData, but got ${pvAgent.stateData}."
          )
      }
    }

    "end in correct state with correct state data after initialisation" in {
      val pvAgent = TestFSMRef(
        new PvAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val triggerId = 0
      scheduler.send(
        pvAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              PvInput,
              SimpleRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              modelConfig = modelConfig,
              secondaryDataServices = withServices,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          triggerId,
          pvAgent
        )
      )

      /* Actor should ask for registration with primary service */
      primaryServiceProxy.expectMsg(
        PrimaryServiceRegistrationMessage(voltageSensitiveInput.getUuid)
      )
      /* State should be information handling and having correct state data */
      pvAgent.stateName shouldBe HandleInformation
      pvAgent.stateData match {
        case ParticipantInitializingStateData(
              inputModel,
              modelConfig,
              secondaryDataServices,
              simulationStartDate,
              simulationEndDate,
              resolution,
              requestVoltageDeviationThreshold,
              outputConfig
            ) =>
          inputModel shouldBe voltageSensitiveInput
          modelConfig shouldBe modelConfig
          secondaryDataServices shouldBe withServices
          simulationStartDate shouldBe this.simulationStartDate
          simulationEndDate shouldBe this.simulationEndDate
          resolution shouldBe this.resolution
          requestVoltageDeviationThreshold shouldBe simonaConfig.runtime.participant.requestVoltageDeviationThreshold
          outputConfig shouldBe defaultOutputConfig
        case unsuitableStateData =>
          fail(s"Agent has unsuitable state data '$unsuitableStateData'.")
      }

      /* Refuse registration */
      primaryServiceProxy.send(pvAgent, RegistrationFailedMessage)

      /* Expect a registration message */
      weatherService.expectMsg(
        RegisterForWeatherMessage(52.02083574, 7.40110716)
      )

      /* ... as well as corresponding state and state data */
      pvAgent.stateName shouldBe HandleInformation
      pvAgent.stateData match {
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
          startDate shouldBe simulationStartDate
          endDate shouldBe simulationEndDate
          services shouldBe Some(
            Vector(
              ActorWeatherService(weatherService.ref)
            )
          )
          outputConfig shouldBe ParticipantNotifierConfig(
            simulationResultInfo = false,
            powerRequestReply = false
          )
          additionalActivationTicks shouldBe empty
          foreseenDataTicks shouldBe Map.empty
          voltageValueStore shouldBe ValueStore(
            resolution * 10,
            Map(0L -> Each(1.0))
          )
          resultValueStore shouldBe ValueStore.forResult(resolution, 10)
          requestValueStore shouldBe ValueStore[ApparentPower](resolution * 10)

          /* Additional information */
          awaitRegistrationResponsesFrom shouldBe Vector(weatherService.ref)
          foreseenNextDataTicks shouldBe Map.empty
        case _ =>
          fail(
            s"Did not find expected state data $CollectRegistrationConfirmMessages, but ${pvAgent.stateData}"
          )
      }

      /* Reply, that registration was successful */
      weatherService.send(pvAgent, RegistrationSuccessfulMessage(Some(4711L)))

      /* Expect a completion message */
      scheduler.expectMsg(
        CompletionMessage(
          triggerId,
          Some(
            scala.collection.immutable.Seq(
              ScheduleTriggerMessage(ActivityStartTrigger(4711), pvAgent)
            )
          )
        )
      )

      /* ... as well as corresponding state and state data */
      pvAgent.stateName shouldBe Idle
      pvAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[_, _, _] =>
          /* Only check the awaited next data ticks, as the rest has yet been checked */
          baseStateData.foreseenDataTicks shouldBe Map(
            weatherService.ref -> Some(4711L)
          )
        case _ =>
          fail(
            s"Did not find expected state data $ParticipantModelBaseStateData, but ${pvAgent.stateData}"
          )
      }
    }

    "answer with zero power, if asked directly after initialisation" in {
      val pvAgent = TestFSMRef(
        new PvAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val triggerId = 0
      scheduler.send(
        pvAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              PvInput,
              SimpleRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              modelConfig = modelConfig,
              secondaryDataServices = withServices,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          triggerId,
          pvAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(pvAgent, RegistrationFailedMessage)

      /* Expect a registration message */
      weatherService.expectMsg(
        RegisterForWeatherMessage(52.02083574, 7.40110716)
      )
      weatherService.send(pvAgent, RegistrationSuccessfulMessage(Some(900L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]

      pvAgent.stateName shouldBe Idle
      /* State data has already been tested */

      pvAgent ! RequestAssetPowerMessage(
        0L,
        Each(1d),
        Each(0d)
      )
      expectMsg(
        AssetPowerChangedMessage(
          Megawatts(0d),
          Megavars(0d)
        )
      )

      inside(pvAgent.stateData) {
        case modelBaseStateData: ParticipantModelBaseStateData[_, _, _] =>
          modelBaseStateData.requestValueStore shouldBe ValueStore[
            ApparentPower
          ](
            resolution * 10,
            Map(
              0L -> ApparentPower(
                Megawatts(0d),
                Megavars(0d)
              )
            )
          )
        case _ =>
          fail(
            s"Did not find expected state data $ParticipantModelBaseStateData, but ${pvAgent.stateData}"
          )
      }
    }

    "do correct transitions faced to new data in Idle" in {
      val pvAgent = TestFSMRef(
        new PvAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val initialiseTriggerId = 0
      scheduler.send(
        pvAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              PvInput,
              SimpleRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              modelConfig = modelConfig,
              secondaryDataServices = withServices,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = simonaConfig.powerflow.resolution.toSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          initialiseTriggerId,
          pvAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(pvAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(pvAgent, RegistrationSuccessfulMessage(Some(0L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(pvAgent.stateName shouldBe Idle)
      /* State data is tested in another test */

      /* Send out new data */
      val weatherData = WeatherData(
        WattsPerSquareMeter(0d),
        WattsPerSquareMeter(0d),
        Celsius(1.815d),
        MetersPerSecond(7.726576d)
      )

      weatherService.send(
        pvAgent,
        ProvideWeatherMessage(0L, weatherData, Some(3600L))
      )

      /* Find yourself in corresponding state and state data */
      pvAgent.stateName shouldBe HandleInformation
      pvAgent.stateData match {
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
            s"Did not find expected state data $DataCollectionStateData, but ${pvAgent.stateData}"
          )
      }

      /* Trigger the agent */
      scheduler.send(
        pvAgent,
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
            scala.collection.immutable.Seq(
              ScheduleTriggerMessage(ActivityStartTrigger(3600L), pvAgent)
            )
          )
        )
      )
      pvAgent.stateName shouldBe Idle
      pvAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[_, _, _] =>
          /* The store for calculation relevant data has been extended */
          baseStateData.calcRelevantDateStore match {
            case ValueStore(_, store) =>
              store shouldBe Map(
                0L -> PvRelevantData(
                  0L.toDateTime,
                  3600L,
                  weatherData.diffIrr,
                  weatherData.dirIrr
                )
              )
          }

          /* The store for simulation results has been extended */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              store.size shouldBe 1
              store.getOrElse(
                0L,
                fail("Expected a simulation result for tick 900.")
              ) match {
                case ApparentPower(p, q) =>
                  (p ~= Megawatts(0.0)) shouldBe true
                  (q ~= Megavars(0.0)) shouldBe true
              }
          }
        case _ =>
          fail(
            s"Did not found the expected state data $ParticipantModelBaseStateData, but ${pvAgent.stateData}"
          )
      }
    }

    "do correct transitions triggered for activation in idle" in {
      val pvAgent = TestFSMRef(
        new PvAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val initialiseTriggerId = 0
      scheduler.send(
        pvAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              PvInput,
              SimpleRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              modelConfig = modelConfig,
              secondaryDataServices = withServices,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = simonaConfig.powerflow.resolution.toSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          initialiseTriggerId,
          pvAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(pvAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(pvAgent, RegistrationSuccessfulMessage(Some(0L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(pvAgent.stateName shouldBe Idle)

      /* Send out an activity start trigger */
      scheduler.send(
        pvAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          1L,
          scheduler.ref
        )
      )

      /* Find yourself in appropriate state with state data */
      pvAgent.stateName shouldBe HandleInformation
      pvAgent.stateData match {
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
            s"Did not find expected state data $DataCollectionStateData, but ${pvAgent.stateData}"
          )
      }

      /* Providing the awaited data will lead to the foreseen transitions */
      val weatherData = WeatherData(
        WattsPerSquareMeter(0d),
        WattsPerSquareMeter(0d),
        Celsius(1.815d),
        MetersPerSecond(7.726576d)
      )

      weatherService.send(
        pvAgent,
        ProvideWeatherMessage(0L, weatherData, Some(3600L))
      )

      /* Expect confirmation */
      scheduler.expectMsg(
        CompletionMessage(
          1L,
          Some(
            scala.collection.immutable.Seq(
              ScheduleTriggerMessage(ActivityStartTrigger(3600L), pvAgent)
            )
          )
        )
      )

      /* Expect the state change to idle with updated base state data */
      pvAgent.stateName shouldBe Idle
      pvAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[_, _, _] =>
          /* The store for calculation relevant data has been extended */
          baseStateData.calcRelevantDateStore match {
            case ValueStore(_, store) =>
              store shouldBe Map(
                0L -> PvRelevantData(
                  0L.toDateTime,
                  3600L,
                  weatherData.diffIrr,
                  weatherData.dirIrr
                )
              )
          }

          /* The store for simulation results has been extended */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              store.size shouldBe 1
              store.getOrElse(
                0L,
                fail("Expected a simulation result for tick 0.")
              ) match {
                case ApparentPower(p, q) =>
                  (p ~= Megawatts(0.0)) shouldBe true
                  (q ~= Megavars(0.0)) shouldBe true
              }
          }
        case _ =>
          fail(
            s"Did not found the expected state data $ParticipantModelBaseStateData, but ${pvAgent.stateData}"
          )
      }
    }

    "does not provide power if data is awaited in an earlier tick, but answers it, if all expected data is there" in {
      val pvAgent = TestFSMRef(
        new PvAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      /* Trigger the initialisation */
      scheduler.send(
        pvAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              PvInput,
              SimpleRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              modelConfig = modelConfig,
              secondaryDataServices = withServices,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          0L,
          pvAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(pvAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(pvAgent, RegistrationSuccessfulMessage(Some(3600L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(pvAgent.stateName shouldBe Idle)

      /* Ask the agent for average power in tick 7200 */
      pvAgent ! RequestAssetPowerMessage(
        7200L,
        Each(1d),
        Each(0d)
      )
      expectNoMessage(noReceiveTimeOut.duration)
      awaitAssert(pvAgent.stateName == Idle)

      /* Send out the expected data and wait for the reply */
      val weatherData = WeatherData(
        WattsPerSquareMeter(0d),
        WattsPerSquareMeter(0d),
        Celsius(1.815d),
        MetersPerSecond(7.726576d)
      )
      weatherService.send(
        pvAgent,
        ProvideWeatherMessage(3600L, weatherData, Some(7200L))
      )

      /* Trigger the agent */
      scheduler.send(
        pvAgent,
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
            scala.collection.immutable.Seq(
              ScheduleTriggerMessage(ActivityStartTrigger(7200L), pvAgent)
            )
          )
        )
      )

      /* Appreciate the answer to my previous request */
      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          (p ~= Megawatts(0.0)) shouldBe true
          (q ~= Megavars(0.0)) shouldBe true
      }
    }

    val pvAgent = TestFSMRef(
      new PvAgent(
        scheduler = scheduler.ref,
        listener = systemListener
      )
    )

    "provide correct average power after three data ticks are available" in {
      /* Trigger the initialisation */
      scheduler.send(
        pvAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              PvInput,
              SimpleRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              modelConfig = modelConfig,
              secondaryDataServices = withServices,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          0L,
          pvAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(pvAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(pvAgent, RegistrationSuccessfulMessage(Some(0L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(pvAgent.stateName shouldBe Idle)

      /* Send out the three data points */
      /* ... for tick 0 */
      weatherService.send(
        pvAgent,
        ProvideWeatherMessage(
          0L,
          WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(1.815d),
            MetersPerSecond(7.726576d)
          ),
          Some(3600L)
        )
      )
      scheduler.send(
        pvAgent,
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
            scala.collection.immutable.Seq(
              ScheduleTriggerMessage(ActivityStartTrigger(3600L), pvAgent)
            )
          )
        )
      )

      /* ... for tick 3600 */
      weatherService.send(
        pvAgent,
        ProvideWeatherMessage(
          3600L,
          WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(1.815d),
            MetersPerSecond(7.726576d)
          ),
          Some(7200L)
        )
      )
      scheduler.send(
        pvAgent,
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
            scala.collection.immutable.Seq(
              ScheduleTriggerMessage(ActivityStartTrigger(7200L), pvAgent)
            )
          )
        )
      )

      /* ... for tick 7200 */
      weatherService.send(
        pvAgent,
        ProvideWeatherMessage(
          7200L,
          WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(1.815d),
            MetersPerSecond(7.726576d)
          ),
          None
        )
      )
      scheduler.send(
        pvAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(7200L),
          5L,
          scheduler.ref
        )
      )
      scheduler.expectMsg(CompletionMessage(5L))

      /* Ask the agent for average power in tick 7500 */
      pvAgent ! RequestAssetPowerMessage(
        7500L,
        Each(1d),
        Each(0d)
      )

      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          (p ~= Megawatts(0.0)) shouldBe true
          (q ~= Megavars(0.0)) shouldBe true
        case answer => fail(s"Did not expect to get that answer: $answer")
      }
    }

    "answer unchanged power values after asking a second time with considerably same voltage" in {
      /* Previous request stems from previous test */
      /* Ask again with (nearly) unchanged information */
      pvAgent ! RequestAssetPowerMessage(
        7500L,
        Each(1.000000000000001d),
        Each(0d)
      )

      /* Expect, that nothing has changed */
      expectMsgType[AssetPowerUnchangedMessage] match {
        case AssetPowerUnchangedMessage(p, q) =>
          (p ~= Megawatts(0.0)) shouldBe true
          (q ~= Megavars(0.0)) shouldBe true
      }
    }

    "answer changed power values after asking a second time with different voltage" in {
      /* Ask again with changed information */
      pvAgent ! RequestAssetPowerMessage(
        7500L,
        Each(0.98),
        Each(0d)
      )

      /* Expect, the correct values (this model has fixed power factor) */
      expectMsgClass(classOf[AssetPowerChangedMessage]) match {
        case AssetPowerChangedMessage(p, q) =>
          (p ~= Megawatts(0.0)) shouldBe true
          (q ~= Megavars(-780.6e-6)) shouldBe true
      }
    }
  }
}
