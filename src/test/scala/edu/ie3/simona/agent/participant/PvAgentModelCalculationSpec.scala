/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.datamodel.models.input.system.characteristic.QV
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.grid.GridAgentMessages.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPower
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.pv.PvAgent
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.DataCollectionStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData._
import edu.ie3.simona.agent.participant2.ParticipantAgent.{
  DataProvision,
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
  RequestAssetPowerMessage,
}
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.agent.state.ParticipantAgentState.HandleInformation
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.RuntimeConfig.PvRuntimeConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.WeatherMessage.{
  RegisterForWeatherMessage,
  WeatherData,
}
import edu.ie3.simona.test.ParticipantAgentSpec
import edu.ie3.simona.test.common.input.PvInputTestData
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
import squants.energy.{Kilowatts, Megawatts, Watts}
import squants.motion.MetersPerSecond
import squants.thermal.Celsius
import squants.{Each, Power}

import java.time.ZonedDateTime
import java.util.concurrent.TimeUnit
import scala.collection.SortedMap

class PvAgentModelCalculationSpec
    extends ParticipantAgentSpec(
      ActorSystem(
        "PvAgentSpec",
        ConfigFactory
          .parseString("""
            |pekko.loggers =["org.apache.pekko.event.slf4j.Slf4jLogger"]
            |pekko.loglevel="DEBUG"
        """.stripMargin),
      )
    )
    with PvInputTestData {

  private implicit val simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z")
  private val simulationEndDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T01:00:00Z")

  implicit val receiveTimeOut: Timeout = Timeout(10, TimeUnit.SECONDS)
  implicit val noReceiveTimeOut: Timeout = Timeout(1, TimeUnit.SECONDS)

  /* Alter the input model to have a voltage sensitive reactive power calculation */
  val voltageSensitiveInput: PvInput = pvInput
    .copy()
    .qCharacteristics(new QV("qV:{(0.95,-0.625),(1.05,0.625)}"))
    .build()

  /* Assign this test to receive the result events from agent */
  override val systemListener: Iterable[ActorRef] = Iterable(self)

  private val simonaConfig: SimonaConfig = createSimonaConfig()
  private val defaultOutputConfig = NotifierConfig(
    simonaConfig.simona.output.participant.defaultConfig.simulationResult,
    simonaConfig.simona.output.participant.defaultConfig.powerRequestReply,
    simonaConfig.simona.output.participant.defaultConfig.flexResult,
  )
  private val configUtil = ConfigUtil.ParticipantConfigUtil(
    simonaConfig.simona.runtime.participant
  )
  private val modelConfig = configUtil.getOrDefault[PvRuntimeConfig](
    voltageSensitiveInput.getUuid
  )
  private val noServices = Iterable.empty
  private val withServices = Iterable(
    ActorWeatherService(weatherService.ref)
  )
  private val resolution = simonaConfig.simona.powerflow.resolution.getSeconds

  private implicit val powerTolerance: Power = Watts(0.1)
  private implicit val reactivePowerTolerance: ReactivePower = Vars(0.1)

  "A pv agent with model calculation depending on no secondary data service" should {
    val initStateData = ParticipantInitializeStateData[
      PvInput,
      PvRuntimeConfig,
      ComplexPower,
    ](
      inputModel = voltageSensitiveInput,
      modelConfig = modelConfig,
      secondaryDataServices = noServices,
      simulationStartDate = simulationStartDate,
      simulationEndDate = simulationEndDate,
      resolution = resolution,
      requestVoltageDeviationThreshold =
        simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
      outputConfig = defaultOutputConfig,
      primaryServiceProxy = primaryServiceProxy.ref,
    )

    "be instantiated correctly" in {
      val pvAgent = TestFSMRef(
        new PvAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = systemListener,
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

    "fail initialisation and stop agent" in {
      val deathProbe = TestProbe("deathProbe")

      val pvAgent = TestFSMRef(
        new PvAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      scheduler.send(pvAgent, Activation(INIT_SIM_TICK))

      deathProbe.watch(pvAgent.ref)

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        pvAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      deathProbe.expectTerminated(pvAgent.ref)
    }
  }

  "A pv agent with model calculation depending on one secondary data service" should {
    val initStateData = ParticipantInitializeStateData[
      PvInput,
      PvRuntimeConfig,
      ComplexPower,
    ](
      inputModel = voltageSensitiveInput,
      modelConfig = modelConfig,
      secondaryDataServices = withServices,
      simulationStartDate = simulationStartDate,
      simulationEndDate = simulationEndDate,
      resolution = resolution,
      requestVoltageDeviationThreshold =
        simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
      outputConfig = defaultOutputConfig,
      primaryServiceProxy = primaryServiceProxy.ref,
    )

    "be instantiated correctly" in {
      val pvAgent = TestFSMRef(
        new PvAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = systemListener,
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
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      scheduler.send(pvAgent, Activation(INIT_SIM_TICK))

      /* Actor should ask for registration with primary service */
      primaryServiceProxy.expectMsg(
        PrimaryServiceRegistrationMessage(
          pvAgent.ref,
          voltageSensitiveInput.getUuid,
        )
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
              outputConfig,
              maybeEmAgent,
            ) =>
          inputModel shouldBe SimpleInputContainer(voltageSensitiveInput)
          modelConfig shouldBe modelConfig
          secondaryDataServices shouldBe withServices
          simulationStartDate shouldBe this.simulationStartDate
          simulationEndDate shouldBe this.simulationEndDate
          resolution shouldBe this.resolution
          requestVoltageDeviationThreshold shouldBe simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold
          outputConfig shouldBe defaultOutputConfig
          maybeEmAgent shouldBe None
        case unsuitableStateData =>
          fail(s"Agent has unsuitable state data '$unsuitableStateData'.")
      }

      /* Refuse registration */
      primaryServiceProxy.send(
        pvAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

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
          services shouldBe Iterable(
            ActorWeatherService(weatherService.ref)
          )
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
            s"Did not find expected state data $CollectRegistrationConfirmMessages, but ${pvAgent.stateData}"
          )
      }

      /* Reply, that registration was successful */
      weatherService.send(
        pvAgent,
        RegistrationSuccessfulMessage(weatherService.ref, 4711L),
      )

      /* Expect a completion message */
      scheduler.expectMsg(Completion(pvAgent.toTyped, Some(4711L)))

      /* ... as well as corresponding state and state data */
      pvAgent.stateName shouldBe Idle
      pvAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[_, _, _, _] =>
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
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      scheduler.send(pvAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        pvAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* Expect a registration message */
      weatherService.expectMsg(
        RegisterForWeatherMessage(52.02083574, 7.40110716)
      )
      weatherService.send(
        pvAgent,
        RegistrationSuccessfulMessage(weatherService.ref, 900L),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]

      pvAgent.stateName shouldBe Idle
      /* State data has already been tested */

      pvAgent ! RequestAssetPowerMessage(
        0L,
        Each(1d),
        Each(0d),
        self.toTyped,
      )
      expectMsg(
        AssetPowerChangedMessage(
          Megawatts(0d),
          Megavars(0d),
        )
      )

      inside(pvAgent.stateData) {
        case baseStateData: ParticipantModelBaseStateData[_, _, _, _] =>
          baseStateData.requestValueStore shouldBe ValueStore[
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
            s"Did not find expected state data $ParticipantModelBaseStateData, but ${pvAgent.stateData}"
          )
      }
    }

    "do correct transitions faced to new data in Idle" in {
      val pvAgent = TestFSMRef(
        new PvAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      scheduler.send(pvAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        pvAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(
        pvAgent,
        RegistrationSuccessfulMessage(weatherService.ref, 0L),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]
      awaitAssert(pvAgent.stateName shouldBe Idle)
      /* State data is tested in another test */

      /* Send out new data */
      val weatherData = WeatherData(
        WattsPerSquareMeter(0d),
        WattsPerSquareMeter(0d),
        Celsius(1.815d),
        MetersPerSecond(7.726576d),
      )

      weatherService.send(
        pvAgent,
        DataProvision(0L, weatherService.ref, weatherData, Some(3600L)),
      )

      /* Find yourself in corresponding state and state data */
      pvAgent.stateName shouldBe HandleInformation
      pvAgent.stateData match {
        case DataCollectionStateData(
              baseStateData: ParticipantModelBaseStateData[_, _, _, _],
              expectedSenders,
              isYetTriggered,
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
      scheduler.send(pvAgent, Activation(0))

      /* The agent will notice, that all expected information are apparent, switch to Calculate and trigger itself
       * for starting the calculation */
      scheduler.expectMsg(Completion(pvAgent.toTyped, Some(3600)))

      pvAgent.stateName shouldBe Idle
      pvAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[_, _, _, _] =>
          /* The store for calculation relevant data has been extended */
          baseStateData.receivedSecondaryDataStore match {
            case ValueStore(_, store) =>
              store shouldBe Map(
                0L -> Map(weatherService.ref -> weatherData)
              )
          }

          /* The store for simulation results has been extended */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              store.size shouldBe 1
              store.getOrElse(
                0L,
                fail("Expected a simulation result for tick 900."),
              ) match {
                case ComplexPower(p, q) =>
                  p should approximate(Megawatts(0.0))
                  q should approximate(Megavars(0.0))
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
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      scheduler.send(pvAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        pvAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(
        pvAgent,
        RegistrationSuccessfulMessage(weatherService.ref, 0),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]
      awaitAssert(pvAgent.stateName shouldBe Idle)

      /* Send out an activity start trigger */
      scheduler.send(pvAgent, Activation(0))

      /* Find yourself in appropriate state with state data */
      pvAgent.stateName shouldBe HandleInformation
      pvAgent.stateData match {
        case DataCollectionStateData(
              baseStateData: ParticipantModelBaseStateData[_, _, _, _],
              expectedSenders,
              isYetTriggered,
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
        MetersPerSecond(7.726576d),
      )

      weatherService.send(
        pvAgent,
        DataProvision(0L, weatherService.ref, weatherData, Some(3600L)),
      )

      /* Expect confirmation */
      scheduler.expectMsg(Completion(pvAgent.toTyped, Some(3600)))

      /* Expect the state change to idle with updated base state data */
      pvAgent.stateName shouldBe Idle
      pvAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[_, _, _, _] =>
          /* The store for calculation relevant data has been extended */
          baseStateData.receivedSecondaryDataStore match {
            case ValueStore(_, store) =>
              store shouldBe Map(
                0L -> Map(weatherService.ref -> weatherData)
              )
          }

          /* The store for simulation results has been extended */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              store.size shouldBe 1
              store.getOrElse(
                0L,
                fail("Expected a simulation result for tick 0."),
              ) match {
                case ComplexPower(p, q) =>
                  p should approximate(Megawatts(0.0))
                  q should approximate(Megavars(0.0))
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
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      /* Trigger the initialisation */
      scheduler.send(pvAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        pvAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(
        pvAgent,
        RegistrationSuccessfulMessage(weatherService.ref, 3600L),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]
      awaitAssert(pvAgent.stateName shouldBe Idle)

      /* Ask the agent for average power in tick 7200 */
      pvAgent ! RequestAssetPowerMessage(
        7200L,
        Each(1d),
        Each(0d),
        self.toTyped,
      )
      expectNoMessage(noReceiveTimeOut.duration)
      awaitAssert(pvAgent.stateName == Idle)

      /* Send out the expected data and wait for the reply */
      val weatherData = WeatherData(
        WattsPerSquareMeter(0d),
        WattsPerSquareMeter(0d),
        Celsius(1.815d),
        MetersPerSecond(7.726576d),
      )
      weatherService.send(
        pvAgent,
        DataProvision(
          3600L,
          weatherService.ref,
          weatherData,
          Some(7200L),
        ),
      )

      /* Trigger the agent */
      scheduler.send(pvAgent, Activation(3600))

      /* The agent will notice, that all expected information are apparent, switch to Calculate and trigger itself
       * for starting the calculation */
      scheduler.expectMsg(Completion(pvAgent.toTyped, Some(7200)))

      /* Appreciate the answer to my previous request */
      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should approximate(Megawatts(0.0))
          q should approximate(Megavars(0.0))
      }
    }

    val pvAgent = TestFSMRef(
      new PvAgent(
        scheduler = scheduler.ref,
        initStateData = initStateData,
        listener = systemListener,
      )
    )

    "provide correct average power after three data ticks are available" in {
      /* Trigger the initialisation */
      scheduler.send(pvAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        pvAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(
        pvAgent,
        RegistrationSuccessfulMessage(weatherService.ref, 0L),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]
      awaitAssert(pvAgent.stateName shouldBe Idle)

      /* Send out the three data points */
      /* ... for tick 0 */
      weatherService.send(
        pvAgent,
        DataProvision(
          0L,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(1.815d),
            MetersPerSecond(7.726576d),
          ),
          Some(3600L),
        ),
      )
      scheduler.send(pvAgent, Activation(0))
      scheduler.expectMsg(Completion(pvAgent.toTyped, Some(3600)))

      /* ... for tick 3600 */
      weatherService.send(
        pvAgent,
        DataProvision(
          3600L,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(1.815d),
            MetersPerSecond(7.726576d),
          ),
          Some(7200L),
        ),
      )
      scheduler.send(pvAgent, Activation(3600))
      scheduler.expectMsg(Completion(pvAgent.toTyped, Some(7200)))

      /* ... for tick 7200 */
      weatherService.send(
        pvAgent,
        DataProvision(
          7200L,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(1.815d),
            MetersPerSecond(7.726576d),
          ),
          None,
        ),
      )
      scheduler.send(pvAgent, Activation(7200))
      scheduler.expectMsg(Completion(pvAgent.toTyped))

      /* Ask the agent for average power in tick 7500 */
      pvAgent ! RequestAssetPowerMessage(
        7500L,
        Each(1d),
        Each(0d),
        self.toTyped,
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
      pvAgent ! RequestAssetPowerMessage(
        7500L,
        Each(1.000000000000001d),
        Each(0d),
        self.toTyped,
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
      pvAgent ! RequestAssetPowerMessage(
        7500L,
        Each(0.98),
        Each(0d),
        self.toTyped,
      )

      /* Expect, the correct values (this model has fixed power factor) */
      expectMsgClass(classOf[AssetPowerChangedMessage]) match {
        case AssetPowerChangedMessage(p, q) =>
          p should approximate(Megawatts(0.0))
          q should approximate(Megavars(-780.6e-6))
      }
    }
  }
}
