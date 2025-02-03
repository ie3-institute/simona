/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.input.system.WecInput
import edu.ie3.datamodel.models.input.system.characteristic.QV
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.grid.GridAgentMessages.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
}
import edu.ie3.simona.agent.participant.ParticipantAgent.RequestAssetPowerMessage
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPower
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.DataCollectionStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.{
  CollectRegistrationConfirmMessages,
  ParticipantInitializeStateData,
  ParticipantUninitializedStateData,
}
import edu.ie3.simona.agent.participant.wec.WecAgent
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.agent.state.ParticipantAgentState.HandleInformation
import edu.ie3.simona.config.RuntimeConfig.SimpleRuntimeConfig
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.WecModel
import edu.ie3.simona.model.participant.WecModel.WecRelevantData
import edu.ie3.simona.model.participant.load.{LoadModelBehaviour, LoadReference}
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.PowerMessage.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
  RequestAssetPowerMessage,
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  IllegalTriggerMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage,
}
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
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeParticipantAgentTrigger,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.{
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.ontology.messages.services.WeatherMessage.{
  ProvideWeatherMessage,
  RegisterForWeatherMessage,
  WeatherData,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.{
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.ontology.messages.services.WeatherMessage.{
  ProvideWeatherMessage,
  RegisterForWeatherMessage,
  WeatherData,
}
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeParticipantAgentTrigger,
}
import edu.ie3.simona.test.ParticipantAgentSpec
import edu.ie3.simona.test.common.input.WecInputTestData
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.quantities.{
  Megavars,
  ReactivePower,
  Vars,
  WattsPerSquareMeter,
}
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.actor.{ActorRef, ActorSystem}
import org.apache.pekko.testkit.{TestFSMRef, TestProbe}
import org.apache.pekko.util.Timeout
import org.scalatest.PrivateMethodTester
import squants.Each
import squants.energy.{Kilowatts, Megawatts, Watts}
import squants.motion.MetersPerSecond
import squants.thermal.Celsius

import java.time.ZonedDateTime
import java.util.concurrent.TimeUnit
import scala.collection.SortedMap

class WecAgentModelCalculationSpec
    extends ParticipantAgentSpec(
      ActorSystem(
        "WecAgentSpec",
        ConfigFactory
          .parseString("""
            |pekko.loggers =["org.apache.pekko.event.slf4j.Slf4jLogger"]
            |pekko.loglevel="DEBUG"
        """.stripMargin),
      )
    )
    with PrivateMethodTester
    with WecInputTestData {

  protected implicit val simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z")
  protected val simulationEndDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T01:00:00Z")
  implicit val receiveTimeOut: Timeout = Timeout(10, TimeUnit.SECONDS)
  implicit val noReceiveTimeOut: Timeout = Timeout(1, TimeUnit.SECONDS)

  /* Alter the input model to have a voltage sensitive reactive power calculation */
  val voltageSensitiveInput: WecInput = wecInputModel
    .copy()
    .qCharacteristics(new QV("qV:{(0.95,-0.625),(1.05,0.625)}"))
    .build()

  /* Assign this test to receive the result events from agent */
  override val systemListener: Iterable[ActorRef] = Iterable(self)

  private val simonaConfig: SimonaConfig =
    createSimonaConfig(
      LoadModelBehaviour.FIX,
      LoadReference.ActivePower(Kilowatts(0d)),
    )
  private val configUtil = ConfigUtil.ParticipantConfigUtil(
    simonaConfig.runtime.participant
  )
  private val modelConfig =
    configUtil.getOrDefault[SimpleRuntimeConfig](
      voltageSensitiveInput.getUuid
    )

  private val withServices = Iterable(ActorWeatherService(weatherService.ref))

  private val resolution = simonaConfig.powerflow.resolution.toSeconds

  private implicit val powerTolerance: squants.Power = Watts(0.1)
  private implicit val reactivePowerTolerance: ReactivePower = Vars(0.1)

  "A wec agent with model calculation depending on no second data service" should {
    val initStateData = ParticipantInitializeStateData[
      WecInput,
      WecRuntimeConfig,
      ComplexPower,
    ](
      inputModel = voltageSensitiveInput,
      simulationStartDate = simulationStartDate,
      simulationEndDate = simulationEndDate,
      resolution = simonaConfig.simona.powerflow.resolution.getSeconds,
      requestVoltageDeviationThreshold =
        simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
      modelConfig = modelConfig,
      primaryServiceProxy = primaryServiceProxy.ref,
      secondaryDataServices = Iterable.empty,
      outputConfig = NotifierConfig(
        simulationResultInfo = false,
        powerRequestReply = false,
        flexResult = false,
      ),
    )

    "be instantiated correctly" in {
      val wecAgent = TestFSMRef(
        new WecAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = systemListener,
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

    "fail initialisation and stop agent" in {
      val deathProbe = TestProbe("deathProbe")

      val wecAgent = TestFSMRef(
        new WecAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      scheduler.send(wecAgent, Activation(INIT_SIM_TICK))

      deathProbe.watch(wecAgent.ref)

      /* Agent attempts to register with primary data service -- refuse this */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        wecAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      deathProbe.expectTerminated(wecAgent.ref)
    }
  }

  "A wec agent with model calculation depending on one secondary data service" should {
    val initStateData = ParticipantInitializeStateData[
      WecInput,
      WecRuntimeConfig,
      ComplexPower,
    ](
      inputModel = voltageSensitiveInput,
      modelConfig = modelConfig,
      simulationStartDate = simulationStartDate,
      simulationEndDate = simulationEndDate,
      resolution = resolution,
      requestVoltageDeviationThreshold =
        simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
      primaryServiceProxy = primaryServiceProxy.ref,
      secondaryDataServices = withServices,
      outputConfig = NotifierConfig(
        simulationResultInfo = false,
        powerRequestReply = false,
        flexResult = false,
      ),
    )

    "be instantiated correctly" in {
      val wecAgent = TestFSMRef(
        new WecAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = systemListener,
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
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      scheduler.send(wecAgent, Activation(INIT_SIM_TICK))

      /* Agent attempts to register with primary data service -- refuse this */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        wecAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

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
                _,
                _,
                _,
              ),
              awaitRegistrationResponsesFrom,
              foreseenNextDataTicks,
            ) =>
          /* Base state data */
          startDate shouldBe simulationStartDate
          endDate shouldBe simulationEndDate
          services shouldBe withServices
          outputConfig shouldBe NotifierConfig(
            simulationResultInfo = false,
            powerRequestReply = false,
            flexResult = false,
          )
          additionalActivationTicks shouldBe empty
          foreseenDataTicks shouldBe Map.empty
          voltageValueStore shouldBe ValueStore(
            resolution,
            SortedMap(0L -> Each(1.0)),
          )
          resultValueStore shouldBe ValueStore(resolution)
          requestValueStore shouldBe ValueStore[ComplexPower](resolution)

          /* Additional information */
          awaitRegistrationResponsesFrom shouldBe Iterable(weatherService.ref)
          foreseenNextDataTicks shouldBe Map.empty
        case _ =>
          fail(
            s"Did not find expected state data $CollectRegistrationConfirmMessages, but ${wecAgent.stateData}"
          )
      }

      /* Reply, that registration was successful */
      weatherService.send(
        wecAgent,
        RegistrationSuccessfulMessage(weatherService.ref, Some(4711L)),
      )

      /* Expect a completion message */
      scheduler.expectMsg(Completion(wecAgent.toTyped, Some(4711L)))

      /* ... as well as corresponding state and state data */
      wecAgent.stateName shouldBe Idle
      wecAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[
              ComplexPower,
              WecRelevantData,
              ConstantState.type,
              WecModel,
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
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      scheduler.send(wecAgent, Activation(INIT_SIM_TICK))

      /* Agent attempts to register with primary data service -- refuse this */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        wecAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* Expect a registration message */
      weatherService.expectMsg(RegisterForWeatherMessage(51.4843281, 7.4116482))
      weatherService.send(
        wecAgent,
        RegistrationSuccessfulMessage(weatherService.ref, Some(900L)),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]

      wecAgent.stateName shouldBe Idle
      /* State data has already been tested */

      wecAgent ! RequestAssetPowerMessage(
        0L,
        Each(1.0),
        Each(0.0),
      )
      expectMsg(
        AssetPowerChangedMessage(
          Megawatts(0.0),
          Megavars(0.0),
        )
      )

      inside(wecAgent.stateData) {
        case modelBaseStateData: ParticipantModelBaseStateData[
              ComplexPower,
              WecRelevantData,
              ConstantState.type,
              WecModel,
            ] =>
          modelBaseStateData.requestValueStore shouldBe ValueStore[
            ComplexPower
          ](
            resolution,
            SortedMap(
              0L -> ComplexPower(
                Megawatts(0d),
                Megavars(0d),
              )
            ),
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
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      scheduler.send(wecAgent, Activation(INIT_SIM_TICK))

      /* Agent attempts to register with primary data service -- refuse this */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        wecAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(
        wecAgent,
        RegistrationSuccessfulMessage(weatherService.ref, Some(900L)),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]
      awaitAssert(wecAgent.stateName shouldBe Idle)
      /* State data is tested in another test */

      /* Send out new data */
      val weatherData = WeatherData(
        WattsPerSquareMeter(50d),
        WattsPerSquareMeter(100d),
        Celsius(0d),
        MetersPerSecond(0d),
      )

      weatherService.send(
        wecAgent,
        ProvideWeatherMessage(
          900L,
          weatherService.ref,
          weatherData,
          Some(1800L),
        ),
      )

      /* Find yourself in corresponding state and state data */
      wecAgent.stateName shouldBe HandleInformation
      wecAgent.stateData match {
        case DataCollectionStateData(
              baseStateData: ParticipantModelBaseStateData[
                ComplexPower,
                WecRelevantData,
                ConstantState.type,
                WecModel,
              ],
              expectedSenders,
              isYetTriggered,
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
      scheduler.send(wecAgent, Activation(900))

      /* The agent will notice, that all expected information are apparent, switch to Calculate and trigger itself
       * for starting the calculation */
      scheduler.expectMsg(Completion(wecAgent.toTyped, Some(1800)))

      wecAgent.stateName shouldBe Idle
      wecAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[
              ComplexPower,
              WecRelevantData,
              ConstantState.type,
              WecModel,
            ] =>
          /* The store for calculation relevant data has been extended */
          baseStateData.receivedSecondaryDataStore match {
            case ValueStore(_, store) =>
              store shouldBe Map(
                900L -> Map(weatherService.ref -> weatherData)
              )
          }

          /* The store for simulation results has been extended */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              store.size shouldBe 1
              store.getOrElse(
                900L,
                fail("Expected a simulation result for tick 900."),
              ) match {
                case ComplexPower(p, q) =>
                  p should approximate(Megawatts(0.0))
                  q should approximate(Megavars(0.0))
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
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      scheduler.send(wecAgent, Activation(INIT_SIM_TICK))

      /* Agent attempts to register with primary data service -- refuse this */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        wecAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(
        wecAgent,
        RegistrationSuccessfulMessage(weatherService.ref, Some(900L)),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]
      awaitAssert(wecAgent.stateName shouldBe Idle)

      /* Send out an activation */
      scheduler.send(wecAgent, Activation(900))

      /* Find yourself in appropriate state with state data */
      wecAgent.stateName shouldBe HandleInformation
      wecAgent.stateData match {
        case DataCollectionStateData(
              baseStateData: ParticipantModelBaseStateData[
                ComplexPower,
                WecRelevantData,
                ConstantState.type,
                WecModel,
              ],
              expectedSenders,
              isYetTriggered,
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
        MetersPerSecond(0d),
      )

      weatherService.send(
        wecAgent,
        ProvideWeatherMessage(
          900L,
          weatherService.ref,
          weatherData,
          Some(1800L),
        ),
      )

      /* Expect confirmation */
      scheduler.expectMsg(Completion(wecAgent.toTyped, Some(1800)))

      /* Expect the state change to idle with updated base state data */
      wecAgent.stateName shouldBe Idle
      wecAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[
              ComplexPower,
              WecRelevantData,
              ConstantState.type,
              WecModel,
            ] =>
          /* The store for calculation relevant data has been extended */
          baseStateData.receivedSecondaryDataStore match {
            case ValueStore(_, store) =>
              store shouldBe Map(
                900L -> Map(weatherService.ref -> weatherData)
              )
          }

          /* The store for simulation results has been extended */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              store.size shouldBe 1
              store.getOrElse(
                900L,
                fail("Expected a simulation result for tick 900."),
              ) match {
                case ComplexPower(p, q) =>
                  p should approximate(Megawatts(0.0))
                  q should approximate(Megavars(0.0))
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
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      /* Trigger the initialisation */
      scheduler.send(wecAgent, Activation(INIT_SIM_TICK))

      /* Agent attempts to register with primary data service -- refuse this */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        wecAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(
        wecAgent,
        RegistrationSuccessfulMessage(weatherService.ref, Some(900L)),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]
      awaitAssert(wecAgent.stateName shouldBe Idle)

      /* Ask the agent for average power in tick 1800 */
      wecAgent ! RequestAssetPowerMessage(
        1800L,
        Each(1.0),
        Each(0.0),
      )
      expectNoMessage(noReceiveTimeOut.duration)
      awaitAssert(wecAgent.stateName == Idle)

      /* Send out the expected data and wait for the reply */
      val weatherData = WeatherData(
        WattsPerSquareMeter(50d),
        WattsPerSquareMeter(100d),
        Celsius(0d),
        MetersPerSecond(0d),
      )
      weatherService.send(
        wecAgent,
        ProvideWeatherMessage(
          900L,
          weatherService.ref,
          weatherData,
          Some(1800L),
        ),
      )

      /* Trigger the agent */
      scheduler.send(wecAgent, Activation(900))

      /* The agent will notice, that all expected information are apparent, switch to Calculate and trigger itself
       * for starting the calculation */
      scheduler.expectMsg(Completion(wecAgent.toTyped, Some(1800)))

      /* Appreciate the answer to my previous request */
      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should approximate(Megawatts(0.0))
          q should approximate(Megavars(0.0))
      }
    }

    val wecAgent = TestFSMRef(
      new WecAgent(
        scheduler = scheduler.ref,
        initStateData = initStateData,
        listener = systemListener,
      )
    )

    "provide correct average power after three data ticks are available" in {
      /* Trigger the initialisation */
      scheduler.send(wecAgent, Activation(INIT_SIM_TICK))

      /* Agent attempts to register with primary data service -- refuse this */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        wecAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(
        wecAgent,
        RegistrationSuccessfulMessage(weatherService.ref, Some(900L)),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]
      awaitAssert(wecAgent.stateName shouldBe Idle)

      /* Send out the three data points */
      /* ... for tick 900 */
      weatherService.send(
        wecAgent,
        ProvideWeatherMessage(
          900L,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(50d),
            WattsPerSquareMeter(100d),
            Celsius(0d),
            MetersPerSecond(0d),
          ),
          Some(1800L),
        ),
      )
      scheduler.send(wecAgent, Activation(900))
      scheduler.expectMsg(Completion(wecAgent.toTyped, Some(1800)))

      /* ... for tick 1800 */
      weatherService.send(
        wecAgent,
        ProvideWeatherMessage(
          1800L,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(50d),
            WattsPerSquareMeter(100d),
            Celsius(0d),
            MetersPerSecond(0d),
          ),
          Some(2700L),
        ),
      )
      scheduler.send(wecAgent, Activation(1800))
      scheduler.expectMsg(Completion(wecAgent.toTyped, Some(2700)))

      /* ... for tick 2700 */
      weatherService.send(
        wecAgent,
        ProvideWeatherMessage(
          2700L,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(50d),
            WattsPerSquareMeter(100d),
            Celsius(0d),
            MetersPerSecond(0d),
          ),
          None,
        ),
      )
      scheduler.send(wecAgent, Activation(2700))
      scheduler.expectMsg(Completion(wecAgent.toTyped))

      /* Ask the agent for average power in tick 3000 */
      wecAgent ! RequestAssetPowerMessage(
        3000L,
        Each(1.0),
        Each(0.0),
      )

      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should approximate(Megawatts(0.0))
          q should approximate(Megavars(0.0))
        case answer => fail(s"Did not expect to get that answer: $answer")
      }
    }

    "answer unchanged power values after asking a second time with considerably same voltage" in {
      /* Previous request stems from previous test */
      /* Ask again with (nearly) unchanged information */
      wecAgent ! RequestAssetPowerMessage(
        3000L,
        Each(1.000000000000001d),
        Each(0.0),
      )

      /* Expect, that nothing has changed */
      expectMsgType[AssetPowerUnchangedMessage] match {
        case AssetPowerUnchangedMessage(p, q) =>
          p should approximate(Megawatts(0.0))
          q should approximate(Megavars(0.0))
      }
    }

    "answer changed power values after asking a second time with different voltage" in {
      /* Ask again with changed information */
      wecAgent ! RequestAssetPowerMessage(
        3000L,
        Each(0.98d),
        Each(0.0),
      )

      /* Expect, the correct values (this model has fixed power factor) */
      expectMsgClass(classOf[AssetPowerChangedMessage]) match {
        case AssetPowerChangedMessage(p, q) =>
          p should approximate(Megawatts(0.0))
          q should approximate(Megavars(-156.1249e-3))
      }
    }
  }
}
