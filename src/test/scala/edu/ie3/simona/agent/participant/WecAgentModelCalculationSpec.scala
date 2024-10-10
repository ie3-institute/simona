/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestFSMRef
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.input.system.WecInput
import edu.ie3.datamodel.models.input.system.characteristic.QV
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.DataCollectionStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.{
  CollectRegistrationConfirmMessages,
  ParticipantInitializeStateData,
  ParticipantInitializingStateData,
  ParticipantUninitializedStateData
}
import edu.ie3.simona.agent.participant.wec.WecAgent
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.agent.state.ParticipantAgentState.HandleInformation
import edu.ie3.simona.config.RuntimeConfig.SimpleRuntimeConfig
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.model.participant.WecModel
import edu.ie3.simona.model.participant.WecModel.WecRelevantData
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
import edu.ie3.simona.test.common.input.WecInputTestData
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.quantities.{
  Megavars,
  ReactivePower,
  Vars,
  WattsPerSquareMeter
}
import org.scalatest.PrivateMethodTester
import squants.Each
import squants.energy.{Kilowatts, Megawatts, Watts}
import squants.motion.MetersPerSecond
import squants.thermal.Celsius

import java.time.ZonedDateTime
import java.util.concurrent.TimeUnit
import scala.collection._

class WecAgentModelCalculationSpec
    extends ParticipantAgentSpec(
      ActorSystem(
        "WecAgentSpec",
        ConfigFactory
          .parseString("""
            |akka.loggers =["akka.event.slf4j.Slf4jLogger"]
            |akka.loglevel="DEBUG"
        """.stripMargin)
      )
    )
    with PrivateMethodTester
    with WecInputTestData {

  protected implicit val simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01 00:00:00")
  protected val simulationEndDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01 01:00:00")
  implicit val receiveTimeOut: Timeout = Timeout(10, TimeUnit.SECONDS)
  implicit val noReceiveTimeOut: Timeout = Timeout(1, TimeUnit.SECONDS)

  /* Alter the input model to have a voltage sensitive reactive power calculation */
  val voltageSensitiveInput: WecInput = wecInputModel
    .copy()
    .qCharacteristics(new QV("qV:{(0.95,-0.625),(1.05,0.625)}"))
    .build()

  /* Assign this test to receive the result events from agent */
  override val systemListener: Iterable[ActorRef] = Vector(self)

  private val simonaConfig: SimonaConfig =
    createSimonaConfig(
      LoadModelBehaviour.FIX,
      LoadReference.ActivePower(Kilowatts(0d))
    )
  private val configUtil = ConfigUtil.ParticipantConfigUtil(
    simonaConfig.runtime.participant
  )
  private val modelConfig =
    configUtil.getOrDefault[SimpleRuntimeConfig](
      voltageSensitiveInput.getUuid
    )

  private val withServices = Some(
    Vector(ActorWeatherService(weatherService.ref))
  )

  private val resolution = simonaConfig.powerflow.resolution.toSeconds

  private implicit val powerTolerance: squants.Power = Watts(0.1)
  private implicit val reactivePowerTolerance: ReactivePower = Vars(0.1)

  "A wec agent with model calculation depending on no second data service" should {
    "be instantiated correctly" in {
      val wecAgent = TestFSMRef(
        new WecAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      wecAgent.stateName shouldBe Uninitialized
      // ParticipantUninitializedStateData is an empty class (due to typing). If it contains content one day
      inside(wecAgent.stateData) {
        case _: ParticipantUninitializedStateData[_] => succeed
        case _ =>
          fail(
            s"Expected $ParticipantUninitializedStateData, but got ${wecAgent.stateData}."
          )
      }
    }

    "fail initialisation and stay in uninitialized state" in {
      val wecAgent = TestFSMRef(
        new WecAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val triggerId = 0
      scheduler.send(
        wecAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              WecInput,
              SimpleRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = simonaConfig.powerflow.resolution.toSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.runtime.participant.requestVoltageDeviationThreshold,
              modelConfig = modelConfig,
              primaryServiceProxy = primaryServiceProxy.ref,
              secondaryDataServices = None,
              outputConfig = ParticipantNotifierConfig(
                simulationResultInfo = false,
                powerRequestReply = false
              )
            )
          ),
          triggerId,
          wecAgent
        )
      )

      /* Agent attempts to register with primary data service -- refuse this */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(wecAgent, RegistrationFailedMessage)

      /* Agent announces, that it has received an illegal trigger */
      scheduler.receiveOne(receiveTimeOut.duration) match {
        case IllegalTriggerMessage(_, _) => logger.debug("Got correct message")
        case null => fail("Did not receive an IllegalTriggerMessage.")
        case m =>
          fail(
            s"Did not fail initialization because of missing weather service. Received: '$m'"
          )
      }

      /* agent should stay uninitialized */
      wecAgent.stateName shouldBe Uninitialized
      // ParticipantUninitializedStateData is an empty class (due to typing). If it contains content one day
      inside(wecAgent.stateData) {
        case _: ParticipantInitializingStateData[_, _, _] => succeed
        case _ =>
          fail(
            s"Expected $ParticipantUninitializedStateData, but got ${wecAgent.stateData}."
          )
      }
    }
  }

  "A wec agent with model calculation depending on one secondary data service" should {
    "be instantiated correctly" in {
      val wecAgent = TestFSMRef(
        new WecAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      wecAgent.stateName shouldBe Uninitialized
      // ParticipantUninitializedStateData is an empty class (due to typing). If it contains content one day
      inside(wecAgent.stateData) {
        case _: ParticipantUninitializedStateData[_] => succeed
        case _ =>
          fail(
            s"Expected $ParticipantUninitializedStateData, but got ${wecAgent.stateData}."
          )
      }
    }

    "end in correct state with correct state data after initialisation" in {
      val wecAgent = TestFSMRef(
        new WecAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val triggerId = 0
      scheduler.send(
        wecAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              WecInput,
              SimpleRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              modelConfig = modelConfig,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.runtime.participant.requestVoltageDeviationThreshold,
              primaryServiceProxy = primaryServiceProxy.ref,
              secondaryDataServices = withServices,
              outputConfig = ParticipantNotifierConfig(
                simulationResultInfo = false,
                powerRequestReply = false
              )
            )
          ),
          triggerId,
          wecAgent
        )
      )

      /* Agent attempts to register with primary data service -- refuse this */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(wecAgent, RegistrationFailedMessage)

      /* Expect a registration message */
      weatherService.expectMsg(RegisterForWeatherMessage(51.4843281, 7.4116482))

      /* ... as well as corresponding state and state data */
      wecAgent.stateName shouldBe HandleInformation
      wecAgent.stateData match {
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
          services shouldBe withServices
          outputConfig shouldBe ParticipantNotifierConfig(
            simulationResultInfo = false,
            powerRequestReply = false
          )
          additionalActivationTicks shouldBe empty
          foreseenDataTicks shouldBe Map.empty
          voltageValueStore shouldBe ValueStore(
            resolution * 10,
            immutable.Map(0L -> Each(1.0))
          )
          resultValueStore shouldBe ValueStore.forResult(resolution, 10)
          requestValueStore shouldBe ValueStore[ApparentPower](resolution * 10)

          /* Additional information */
          awaitRegistrationResponsesFrom shouldBe Vector(weatherService.ref)
          foreseenNextDataTicks shouldBe Map.empty
        case _ =>
          fail(
            s"Did not find expected state data $CollectRegistrationConfirmMessages, but ${wecAgent.stateData}"
          )
      }

      /* Reply, that registration was successful */
      weatherService.send(wecAgent, RegistrationSuccessfulMessage(Some(4711L)))

      /* Expect a completion message */
      scheduler.expectMsg(
        CompletionMessage(
          triggerId,
          Some(
            immutable.Seq(
              ScheduleTriggerMessage(ActivityStartTrigger(4711), wecAgent)
            )
          )
        )
      )

      /* ... as well as corresponding state and state data */
      wecAgent.stateName shouldBe Idle
      wecAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[
              ApparentPower,
              WecRelevantData,
              WecModel
            ] =>
          /* Only check the awaited next data ticks, as the rest has yet been checked */
          baseStateData.foreseenDataTicks shouldBe Map(
            weatherService.ref -> Some(4711L)
          )
        case _ =>
          fail(
            s"Did not find expected state data $ParticipantModelBaseStateData, but ${wecAgent.stateData}"
          )
      }
    }

    "answer with zero power, if asked directly after initialisation" in {
      val wecAgent = TestFSMRef(
        new WecAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val triggerId = 0
      scheduler.send(
        wecAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              WecInput,
              SimpleRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              modelConfig = modelConfig,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.runtime.participant.requestVoltageDeviationThreshold,
              primaryServiceProxy = primaryServiceProxy.ref,
              secondaryDataServices = withServices,
              outputConfig = ParticipantNotifierConfig(
                simulationResultInfo = false,
                powerRequestReply = false
              )
            )
          ),
          triggerId,
          wecAgent
        )
      )

      /* Agent attempts to register with primary data service -- refuse this */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(wecAgent, RegistrationFailedMessage)

      /* Expect a registration message */
      weatherService.expectMsg(RegisterForWeatherMessage(51.4843281, 7.4116482))
      weatherService.send(wecAgent, RegistrationSuccessfulMessage(Some(900L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]

      wecAgent.stateName shouldBe Idle
      /* State data has already been tested */

      wecAgent ! RequestAssetPowerMessage(
        0L,
        Each(1.0),
        Each(0.0)
      )
      expectMsg(
        AssetPowerChangedMessage(
          Megawatts(0.0),
          Megavars(0.0)
        )
      )

      inside(wecAgent.stateData) {
        case modelBaseStateData: ParticipantModelBaseStateData[
              ApparentPower,
              WecRelevantData,
              WecModel
            ] =>
          modelBaseStateData.requestValueStore shouldBe ValueStore[
            ApparentPower
          ](
            resolution * 10,
            immutable.Map(
              0L -> ApparentPower(
                Megawatts(0d),
                Megavars(0d)
              )
            )
          )
        case _ =>
          fail(
            s"Did not find expected state data $ParticipantModelBaseStateData, but ${wecAgent.stateData}"
          )
      }
    }

    "do correct transitions faced to new data in Idle" in {
      val wecAgent = TestFSMRef(
        new WecAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val initialiseTriggerId = 0
      scheduler.send(
        wecAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              WecInput,
              SimpleRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              modelConfig = modelConfig,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = simonaConfig.powerflow.resolution.toSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.runtime.participant.requestVoltageDeviationThreshold,
              primaryServiceProxy = primaryServiceProxy.ref,
              secondaryDataServices = withServices,
              outputConfig = ParticipantNotifierConfig(
                simulationResultInfo = false,
                powerRequestReply = false
              )
            )
          ),
          initialiseTriggerId,
          wecAgent
        )
      )

      /* Agent attempts to register with primary data service -- refuse this */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(wecAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(wecAgent, RegistrationSuccessfulMessage(Some(900L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(wecAgent.stateName shouldBe Idle)
      /* State data is tested in another test */

      /* Send out new data */
      val weatherData = WeatherData(
        WattsPerSquareMeter(50d),
        WattsPerSquareMeter(100d),
        Celsius(0d),
        MetersPerSecond(0d)
      )

      weatherService.send(
        wecAgent,
        ProvideWeatherMessage(900L, weatherData, Some(1800L))
      )

      /* Find yourself in corresponding state and state data */
      wecAgent.stateName shouldBe HandleInformation
      wecAgent.stateData match {
        case DataCollectionStateData(
              baseStateData: ParticipantModelBaseStateData[
                ApparentPower,
                WecRelevantData,
                WecModel
              ],
              expectedSenders,
              isYetTriggered
            ) =>
          /* The next data tick is already registered */
          baseStateData.foreseenDataTicks shouldBe Map(
            weatherService.ref -> Some(1800L)
          )

          /* The yet sent data is also registered */
          expectedSenders shouldBe Map(
            weatherService.ref -> Some(weatherData)
          )

          /* It is not yet triggered */
          isYetTriggered shouldBe false
        case _ =>
          fail(
            s"Did not find expected state data $DataCollectionStateData, but ${wecAgent.stateData}"
          )
      }

      /* Trigger the agent */
      scheduler.send(
        wecAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(900L),
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
            immutable.Seq(
              ScheduleTriggerMessage(ActivityStartTrigger(1800L), wecAgent)
            )
          )
        )
      )
      wecAgent.stateName shouldBe Idle
      wecAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[
              ApparentPower,
              WecRelevantData,
              WecModel
            ] =>
          /* The store for calculation relevant data has been extended */
          baseStateData.calcRelevantDateStore match {
            case ValueStore(_, store) =>
              store shouldBe Map(
                900L -> WecRelevantData(
                  weatherData.windVel,
                  weatherData.temp,
                  None
                )
              )
          }

          /* The store for simulation results has been extended */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              store.size shouldBe 1
              store.getOrElse(
                900L,
                fail("Expected a simulation result for tick 900.")
              ) match {
                case ApparentPower(p, q) =>
                  (p ~= Megawatts(0.0)) shouldBe true
                  (q ~= Megavars(0.0)) shouldBe true
              }
          }
        case _ =>
          fail(
            s"Did not found the expected state data $ParticipantModelBaseStateData, but ${wecAgent.stateData}"
          )
      }
    }

    "do correct transitions triggered for activation in idle" in {
      val wecAgent = TestFSMRef(
        new WecAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val initialiseTriggerId = 0
      scheduler.send(
        wecAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              WecInput,
              SimpleRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              modelConfig = modelConfig,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = simonaConfig.powerflow.resolution.toSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.runtime.participant.requestVoltageDeviationThreshold,
              primaryServiceProxy = primaryServiceProxy.ref,
              secondaryDataServices = withServices,
              outputConfig = ParticipantNotifierConfig(
                simulationResultInfo = false,
                powerRequestReply = false
              )
            )
          ),
          initialiseTriggerId,
          wecAgent
        )
      )

      /* Agent attempts to register with primary data service -- refuse this */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(wecAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(wecAgent, RegistrationSuccessfulMessage(Some(900L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(wecAgent.stateName shouldBe Idle)

      /* Send out an activity start trigger */
      scheduler.send(
        wecAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(900L),
          1L,
          scheduler.ref
        )
      )

      /* Find yourself in appropriate state with state data */
      wecAgent.stateName shouldBe HandleInformation
      wecAgent.stateData match {
        case DataCollectionStateData(
              baseStateData: ParticipantModelBaseStateData[
                ApparentPower,
                WecRelevantData,
                WecModel
              ],
              expectedSenders,
              isYetTriggered
            ) =>
          /* The next data tick is already registered */
          baseStateData.foreseenDataTicks shouldBe Map(
            weatherService.ref -> Some(900L)
          )

          /* The yet sent data is also registered */
          expectedSenders shouldBe Map(weatherService.ref -> None)

          /* It is yet triggered */
          isYetTriggered shouldBe true
        case _ =>
          fail(
            s"Did not find expected state data $DataCollectionStateData, but ${wecAgent.stateData}"
          )
      }

      /* Providing the awaited data will lead to the foreseen transitions */
      val weatherData = WeatherData(
        WattsPerSquareMeter(50d),
        WattsPerSquareMeter(100d),
        Celsius(0d),
        MetersPerSecond(0d)
      )

      weatherService.send(
        wecAgent,
        ProvideWeatherMessage(900L, weatherData, Some(1800L))
      )

      /* Expect confirmation */
      scheduler.expectMsg(
        CompletionMessage(
          1L,
          Some(
            immutable.Seq(
              ScheduleTriggerMessage(ActivityStartTrigger(1800L), wecAgent)
            )
          )
        )
      )

      /* Expect the state change to idle with updated base state data */
      wecAgent.stateName shouldBe Idle
      wecAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[
              ApparentPower,
              WecRelevantData,
              WecModel
            ] =>
          /* The store for calculation relevant data has been extended */
          baseStateData.calcRelevantDateStore match {
            case ValueStore(_, store) =>
              store shouldBe Map(
                900L -> WecRelevantData(
                  weatherData.windVel,
                  weatherData.temp,
                  None
                )
              )
          }

          /* The store for simulation results has been extended */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              store.size shouldBe 1
              store.getOrElse(
                900L,
                fail("Expected a simulation result for tick 900.")
              ) match {
                case ApparentPower(p, q) =>
                  (p ~= Megawatts(0.0)) shouldBe true
                  (q ~= Megavars(0.0)) shouldBe true
              }
          }
        case _ =>
          fail(
            s"Did not found the expected state data $ParticipantModelBaseStateData, but ${wecAgent.stateData}"
          )
      }
    }

    "does not provide power if data is awaited in an earlier tick, but answers it, if all expected data is there" in {
      val wecAgent = TestFSMRef(
        new WecAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      /* Trigger the initialisation */
      scheduler.send(
        wecAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              WecInput,
              SimpleRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              modelConfig = modelConfig,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.runtime.participant.requestVoltageDeviationThreshold,
              primaryServiceProxy = primaryServiceProxy.ref,
              secondaryDataServices = withServices,
              outputConfig = ParticipantNotifierConfig(
                simulationResultInfo = false,
                powerRequestReply = false
              )
            )
          ),
          0L,
          wecAgent
        )
      )

      /* Agent attempts to register with primary data service -- refuse this */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(wecAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(wecAgent, RegistrationSuccessfulMessage(Some(900L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(wecAgent.stateName shouldBe Idle)

      /* Ask the agent for average power in tick 1800 */
      wecAgent ! RequestAssetPowerMessage(
        1800L,
        Each(1.0),
        Each(0.0)
      )
      expectNoMessage(noReceiveTimeOut.duration)
      awaitAssert(wecAgent.stateName == Idle)

      /* Send out the expected data and wait for the reply */
      val weatherData = WeatherData(
        WattsPerSquareMeter(50d),
        WattsPerSquareMeter(100d),
        Celsius(0d),
        MetersPerSecond(0d)
      )
      weatherService.send(
        wecAgent,
        ProvideWeatherMessage(900L, weatherData, Some(1800L))
      )

      /* Trigger the agent */
      scheduler.send(
        wecAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(900L),
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
            immutable.Seq(
              ScheduleTriggerMessage(ActivityStartTrigger(1800L), wecAgent)
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

    val wecAgent = TestFSMRef(
      new WecAgent(
        scheduler = scheduler.ref,
        listener = systemListener
      )
    )

    "provide correct average power after three data ticks are available" in {
      /* Trigger the initialisation */
      scheduler.send(
        wecAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              WecInput,
              SimpleRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              modelConfig = modelConfig,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.runtime.participant.requestVoltageDeviationThreshold,
              primaryServiceProxy = primaryServiceProxy.ref,
              secondaryDataServices = withServices,
              outputConfig = ParticipantNotifierConfig(
                simulationResultInfo = false,
                powerRequestReply = false
              )
            )
          ),
          0L,
          wecAgent
        )
      )

      /* Agent attempts to register with primary data service -- refuse this */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(wecAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(wecAgent, RegistrationSuccessfulMessage(Some(900L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(wecAgent.stateName shouldBe Idle)

      /* Send out the three data points */
      /* ... for tick 900 */
      weatherService.send(
        wecAgent,
        ProvideWeatherMessage(
          900L,
          WeatherData(
            WattsPerSquareMeter(50d),
            WattsPerSquareMeter(100d),
            Celsius(0d),
            MetersPerSecond(0d)
          ),
          Some(1800L)
        )
      )
      scheduler.send(
        wecAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(900L),
          1L,
          scheduler.ref
        )
      )
      scheduler.expectMsg(
        CompletionMessage(
          1L,
          Some(
            immutable.Seq(
              ScheduleTriggerMessage(ActivityStartTrigger(1800L), wecAgent)
            )
          )
        )
      )

      /* ... for tick 1800 */
      weatherService.send(
        wecAgent,
        ProvideWeatherMessage(
          1800L,
          WeatherData(
            WattsPerSquareMeter(50d),
            WattsPerSquareMeter(100d),
            Celsius(0d),
            MetersPerSecond(0d)
          ),
          Some(2700L)
        )
      )
      scheduler.send(
        wecAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(1800L),
          3L,
          scheduler.ref
        )
      )
      scheduler.expectMsg(
        CompletionMessage(
          3L,
          Some(
            immutable.Seq(
              ScheduleTriggerMessage(ActivityStartTrigger(2700L), wecAgent)
            )
          )
        )
      )

      /* ... for tick 2700 */
      weatherService.send(
        wecAgent,
        ProvideWeatherMessage(
          2700L,
          WeatherData(
            WattsPerSquareMeter(50d),
            WattsPerSquareMeter(100d),
            Celsius(0d),
            MetersPerSecond(0d)
          ),
          None
        )
      )
      scheduler.send(
        wecAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(2700L),
          5L,
          scheduler.ref
        )
      )
      scheduler.expectMsg(CompletionMessage(5L))

      /* Ask the agent for average power in tick 3000 */
      wecAgent ! RequestAssetPowerMessage(
        3000L,
        Each(1.0),
        Each(0.0)
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
      wecAgent ! RequestAssetPowerMessage(
        3000L,
        Each(1.000000000000001d),
        Each(0.0)
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
      wecAgent ! RequestAssetPowerMessage(
        3000L,
        Each(0.98d),
        Each(0.0)
      )

      /* Expect, the correct values (this model has fixed power factor) */
      expectMsgClass(classOf[AssetPowerChangedMessage]) match {
        case AssetPowerChangedMessage(p, q) =>
          (p ~= Megawatts(0.0)) shouldBe true
          (q ~= Megavars(-156.1249e-3)) shouldBe true
      }
    }
  }
}
