/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.input.system.HpInput
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.grid.GridAgentMessages.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPowerAndHeat
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.hp.HpAgent
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
import edu.ie3.simona.config.RuntimeConfig.HpRuntimeConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.integration.common.IntegrationSpecCommon
import edu.ie3.simona.model.participant.HpModel.HpState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.WeatherMessage.{
  RegisterForWeatherMessage,
  WeatherData,
}
import edu.ie3.simona.test.ParticipantAgentSpec
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.simona.test.common.input.HpInputTestData
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.scala.quantities.{
  Megavars,
  ReactivePower,
  Vars,
  WattsPerSquareMeter,
}
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.testkit.{TestFSMRef, TestProbe}
import org.apache.pekko.util.Timeout
import org.scalatest.PrivateMethodTester
import squants.energy.{Kilowatts, Megawatts, Watts}
import squants.motion.MetersPerSecond
import squants.thermal.Celsius
import squants.{Dimensionless, Each}

import java.io.File
import java.util.concurrent.TimeUnit
import scala.collection.SortedMap

class HpAgentModelCalculationSpec
    extends ParticipantAgentSpec(
      ActorSystem(
        "HpAgentSpec",
        ConfigFactory
          .parseString("""
            |pekko.loggers =["org.apache.pekko.event.slf4j.Slf4jLogger"]
            |pekko.loglevel="DEBUG"
        """.stripMargin),
      )
    )
    with HpInputTestData
    with IntegrationSpecCommon
    with PrivateMethodTester
    with DefaultTestData {

  implicit val receiveTimeOut: Timeout = Timeout(10, TimeUnit.SECONDS)
  implicit val noReceiveTimeOut: Timeout = Timeout(1, TimeUnit.SECONDS)

  private implicit val powerTolerance: squants.Power = Watts(0.1)
  private implicit val reactivePowerTolerance: ReactivePower = Vars(0.1)
  private implicit val temperatureTolerance: squants.Temperature = Celsius(
    1e-10
  )

  /* Alter the input model to have a voltage sensitive reactive power calculation */
  val hpInput: HpInput = hpInputModel

  private val simonaConfig: SimonaConfig = SimonaConfig(
    ConfigFactory
      .empty()
      .withFallback(ConfigFactory.parseFile(new File(configFile)))
      .resolve()
  )
  private val defaultOutputConfig = NotifierConfig(
    simonaConfig.simona.output.participant.defaultConfig.simulationResult,
    simonaConfig.simona.output.participant.defaultConfig.powerRequestReply,
    simonaConfig.simona.output.participant.defaultConfig.flexResult,
  )
  private val participantConfigUtil = ConfigUtil.ParticipantConfigUtil(
    simonaConfig.simona.runtime.participant
  )
  private val modelConfig =
    participantConfigUtil.getOrDefault[HpRuntimeConfig](
      hpInput.getUuid
    )
  private val noServices = Iterable.empty
  private val services = Iterable(
    ActorWeatherService(weatherService.ref)
  )
  private val resolution = simonaConfig.simona.powerflow.resolution.getSeconds

  "A heat pump agent depending on no services" should {
    val initStateData = ParticipantInitializeStateData[
      HpInput,
      HpRuntimeConfig,
      ComplexPowerAndHeat,
    ](
      inputModel = hpInput,
      modelConfig = modelConfig,
      secondaryDataServices = noServices,
      simulationStartDate = defaultSimulationStart,
      simulationEndDate = defaultSimulationEnd,
      resolution = resolution,
      requestVoltageDeviationThreshold =
        simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
      outputConfig = defaultOutputConfig,
      primaryServiceProxy = primaryServiceProxy.ref,
      maybeEmAgent = None,
    )

    "be instantiated correctly" in {
      val hpAgent = TestFSMRef(
        new HpAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = systemListener,
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
      val deathProbe = TestProbe("deathProbe")

      val hpAgent = TestFSMRef(
        new HpAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      scheduler.send(hpAgent, Activation(INIT_SIM_TICK))

      deathProbe.watch(hpAgent)

      /* Agent attempts to register with primary data service -- refuse this */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        hpAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      deathProbe.expectTerminated(hpAgent)
    }
  }

  "A heat pump agent depending on one secondary data service" should {
    val initStateData = ParticipantInitializeStateData[
      HpInput,
      HpRuntimeConfig,
      ComplexPowerAndHeat,
    ](
      inputModel = hpInput,
      thermalGrid = defaultThermalGrid,
      modelConfig = modelConfig,
      secondaryDataServices = services,
      simulationStartDate = defaultSimulationStart,
      simulationEndDate = defaultSimulationEnd,
      resolution = resolution,
      requestVoltageDeviationThreshold =
        simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
      outputConfig = defaultOutputConfig,
      primaryServiceProxy = primaryServiceProxy.ref,
      maybeEmAgent = None,
    )

    "be instantiated correctly" in {
      val hpAgent = TestFSMRef(
        new HpAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = systemListener,
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
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      scheduler.send(hpAgent, Activation(INIT_SIM_TICK))

      /* Actor should ask for registration with primary service */
      primaryServiceProxy.expectMsg(
        PrimaryServiceRegistrationMessage(hpAgent.ref, hpInput.getUuid)
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
              outputConfig,
              _,
            ) =>
          inputModel shouldBe WithHeatInputContainer(
            hpInput,
            defaultThermalGrid,
          )
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
      primaryServiceProxy.send(
        hpAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* Expect a registration message */
      weatherService.expectMsg(
        RegisterForWeatherMessage(52.02083574, 7.40110716)
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
                _,
                _,
                _,
              ),
              awaitRegistrationResponsesFrom,
              foreseenNextDataTicks,
            ) =>
          /* Base state data */
          startDate shouldBe defaultSimulationStart
          endDate shouldBe defaultSimulationEnd
          services shouldBe Iterable(
            ActorWeatherService(weatherService.ref)
          )
          outputConfig shouldBe NotifierConfig(
            simulationResultInfo = true,
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
          requestValueStore shouldBe ValueStore[ComplexPowerAndHeat](
            resolution
          )

          /* Additional information */
          awaitRegistrationResponsesFrom shouldBe Iterable(weatherService.ref)
          foreseenNextDataTicks shouldBe Map.empty
        case _ =>
          fail(
            s"Did not find expected state data $CollectRegistrationConfirmMessages, but ${hpAgent.stateData}"
          )
      }

      /* Reply, that registration was successful */
      weatherService.send(
        hpAgent,
        RegistrationSuccessfulMessage(weatherService.ref, 4711L),
      )

      /* Expect a completion message */
      scheduler.expectMsg(Completion(hpAgent.toTyped, Some(4711L)))

      /* ... as well as corresponding state and state data */
      hpAgent.stateName shouldBe Idle
      hpAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[_, _, _, _] =>
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
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      scheduler.send(hpAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        hpAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* Expect a registration message */
      weatherService.expectMsg(
        RegisterForWeatherMessage(52.02083574, 7.40110716)
      )
      weatherService.send(
        hpAgent,
        RegistrationSuccessfulMessage(weatherService.ref, 900L),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]

      hpAgent.stateName shouldBe Idle
      /* State data has already been tested */

      hpAgent ! RequestAssetPowerMessage(
        0L,
        Dimensionless.primaryUnit(1.0),
        Dimensionless.primaryUnit(0.0),
        self.toTyped,
      )
      expectMsg(
        AssetPowerChangedMessage(
          Megawatts(0.0),
          Megavars(0.0),
        )
      )

      inside(hpAgent.stateData) {
        case modelBaseStateData: ParticipantModelBaseStateData[_, _, _, _] =>
          modelBaseStateData.requestValueStore shouldBe ValueStore[
            ComplexPowerAndHeat
          ](
            resolution,
            SortedMap(
              0L -> ComplexPowerAndHeat(
                Megawatts(0.0),
                Megavars(0.0),
                Megawatts(0.0),
              )
            ),
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
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      scheduler.send(hpAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        hpAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(
        hpAgent,
        RegistrationSuccessfulMessage(weatherService.ref, 0L),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]
      awaitAssert(hpAgent.stateName shouldBe Idle)
      /* State data is tested in another test */

      /* Send out new data */
      val weatherData = WeatherData(
        WattsPerSquareMeter(0),
        WattsPerSquareMeter(0),
        Celsius(1.815),
        MetersPerSecond(7.726576),
      )

      weatherService.send(
        hpAgent,
        DataProvision(0L, weatherService.ref, weatherData, Some(3600L)),
      )

      /* Find yourself in corresponding state and state data */
      hpAgent.stateName shouldBe HandleInformation
      hpAgent.stateData match {
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
            s"Did not find expected state data $DataCollectionStateData, but ${hpAgent.stateData}"
          )
      }

      /* Trigger the agent */
      scheduler.send(hpAgent, Activation(0))

      /* The agent will notice, that all expected information are apparent, switch to Calculate and trigger itself
       * for starting the calculation */
      scheduler.expectMsg(Completion(hpAgent.toTyped, Some(3600)))

      hpAgent.stateName shouldBe Idle
      hpAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[_, _, _, _] =>
          baseStateData.stateDataStore.last(0L) match {
            case Some(
                  (
                    _,
                    HpState(
                      isRunning,
                      tick,
                      _,
                      activePower,
                      qDot,
                      thermalGridState,
                      _,
                    ),
                  )
                ) =>
              isRunning shouldBe false
              tick shouldBe 0L
              activePower should approximate(Kilowatts(0.0))
              qDot should approximate(Kilowatts(0.0))

              thermalGridState.houseState match {
                case Some(ThermalHouseState(_, innerTemperature, _)) =>
                  innerTemperature should approximate(Celsius(20.999976906944))
                case None =>
                  fail(
                    s"Expected to get a result for thermal house '${hpInputModel.getUuid}'"
                  )
              }
            case None => fail("Expected to get a model state")
          }

          /* The store for simulation results has been extended */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              store.size shouldBe 1
              store.getOrElse(
                0L,
                fail("Expected a simulation result for tick 900."),
              ) match {
                case ComplexPowerAndHeat(p, q, qDot) =>
                  p should approximate(Megawatts(0d))
                  q should approximate(Megavars(0d))
                  qDot should approximate(Megawatts(0d))
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
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      scheduler.send(hpAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        hpAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(
        hpAgent,
        RegistrationSuccessfulMessage(weatherService.ref, 0L),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]
      awaitAssert(hpAgent.stateName shouldBe Idle)

      /* Send out an activation*/
      scheduler.send(hpAgent, Activation(0))

      /* Find yourself in appropriate state with state data */
      hpAgent.stateName shouldBe HandleInformation
      hpAgent.stateData match {
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
            s"Did not find expected state data $DataCollectionStateData, but ${hpAgent.stateData}"
          )
      }

      /* Providing the awaited data will lead to the foreseen transitions */
      val weatherData = WeatherData(
        WattsPerSquareMeter(0),
        WattsPerSquareMeter(0),
        Celsius(1.815),
        MetersPerSecond(7.726576),
      )

      weatherService.send(
        hpAgent,
        DataProvision(0L, weatherService.ref, weatherData, Some(3600L)),
      )

      /* Expect confirmation */
      scheduler.expectMsg(Completion(hpAgent.toTyped, Some(3600)))

      /* Expect the state change to idle with updated base state data */
      hpAgent.stateName shouldBe Idle
      hpAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[_, _, _, _] =>
          baseStateData.stateDataStore.last(0L) match {
            case Some(
                  (
                    _,
                    HpState(
                      isRunning,
                      tick,
                      _,
                      activePower,
                      qDot,
                      thermalGridState,
                      _,
                    ),
                  )
                ) =>
              isRunning shouldBe false
              tick shouldBe 0L
              activePower should approximate(Kilowatts(0d))
              qDot should approximate(Kilowatts(0d))

              thermalGridState.houseState match {
                case Some(ThermalHouseState(_, innerTemperature, _)) =>
                  innerTemperature should approximate(Celsius(20.999976906944))
                case None =>
                  fail(
                    s"Expected to get a result for thermal house '${hpInputModel.getUuid}'"
                  )
              }
            case None => fail("Expected to get a model state.")
          }

          /* The store for simulation results has been extended */
          baseStateData.resultValueStore match {
            case ValueStore(_, store) =>
              store.size shouldBe 1
              store.getOrElse(
                0L,
                fail("Expected a simulation result for tick 0."),
              ) match {
                case ComplexPowerAndHeat(p, q, qDot) =>
                  p should approximate(Megawatts(0d))
                  q should approximate(Megavars(0d))
                  qDot should approximate(Megawatts(0d))
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
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      /* Trigger the initialisation */
      scheduler.send(hpAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        hpAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(
        hpAgent,
        RegistrationSuccessfulMessage(weatherService.ref, 3600L),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]
      awaitAssert(hpAgent.stateName shouldBe Idle)

      /* Ask the agent for average power in tick 7200 */
      hpAgent ! RequestAssetPowerMessage(
        7200L,
        Each(1.0),
        Each(0.0),
        self.toTyped,
      )
      expectNoMessage(noReceiveTimeOut.duration)
      awaitAssert(hpAgent.stateName == Idle)

      /* Send out the expected data and wait for the reply */
      val weatherData = WeatherData(
        WattsPerSquareMeter(0),
        WattsPerSquareMeter(0),
        Celsius(1.815),
        MetersPerSecond(7.726576),
      )
      weatherService.send(
        hpAgent,
        DataProvision(
          3600L,
          weatherService.ref,
          weatherData,
          Some(7200L),
        ),
      )

      /* Trigger the agent */
      scheduler.send(hpAgent, Activation(3600))

      /* The agent will notice, that all expected information are apparent, switch to Calculate and trigger itself
       * for starting the calculation */
      scheduler.expectMsg(Completion(hpAgent.toTyped, Some(7200)))

      /* Appreciate the answer to my previous request */
      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should approximate(Megawatts(0d))
          q should approximate(Megavars(0d))
      }
    }

    val hpAgent = TestFSMRef(
      new HpAgent(
        scheduler = scheduler.ref,
        initStateData = initStateData,
        listener = systemListener,
      )
    )

    "provide correct average power after three data ticks are available" in {
      /* Trigger the initialisation */
      scheduler.send(hpAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        hpAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(
        hpAgent,
        RegistrationSuccessfulMessage(weatherService.ref, 0L),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]
      awaitAssert(hpAgent.stateName shouldBe Idle)

      /* Send out the three data points */
      /* ... for tick 0 */
      weatherService.send(
        hpAgent,
        DataProvision(
          0L,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(0),
            WattsPerSquareMeter(0),
            Celsius(1.815),
            MetersPerSecond(7.726576),
          ),
          Some(3600L),
        ),
      )
      scheduler.send(hpAgent, Activation(0))
      scheduler.expectMsg(Completion(hpAgent.toTyped, Some(3600)))

      /* ... for tick 3600 */
      weatherService.send(
        hpAgent,
        DataProvision(
          3600L,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(0),
            WattsPerSquareMeter(0),
            Celsius(1.815),
            MetersPerSecond(7.726576),
          ),
          Some(7200L),
        ),
      )
      scheduler.send(hpAgent, Activation(3600))
      scheduler.expectMsg(Completion(hpAgent.toTyped, Some(7200)))

      /* ... for tick 7200 */
      weatherService.send(
        hpAgent,
        DataProvision(
          7200L,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(0),
            WattsPerSquareMeter(0),
            Celsius(1.815),
            MetersPerSecond(7.726576),
          ),
          None,
        ),
      )
      scheduler.send(hpAgent, Activation(7200))
      scheduler.expectMsg(Completion(hpAgent.toTyped))

      /* Ask the agent for average power in tick 7500 */
      hpAgent ! RequestAssetPowerMessage(
        7500L,
        Each(1.0),
        Each(0.0),
        self.toTyped,
      )

      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should approximate(Megawatts(0d))
          q should approximate(Megavars(0d))
        case answer => fail(s"Did not expect to get that answer: $answer")
      }
    }

    "answer unchanged power values after asking a second time with considerably same voltage" in {
      /* Previous request stems from previous test */
      /* Ask again with (nearly) unchanged information */
      hpAgent ! RequestAssetPowerMessage(
        7500L,
        Each(1.000000000000001d),
        Each(0.0),
        self.toTyped,
      )

      /* Expect, that nothing has changed */
      expectMsgType[AssetPowerUnchangedMessage] match {
        case AssetPowerUnchangedMessage(p, q) =>
          p should approximate(Megawatts(0d))
          q should approximate(Megavars(0d))
      }
    }

    "answer changed power values after asking a second time with different voltage" in {
      /* Ask again with changed information */
      hpAgent ! RequestAssetPowerMessage(
        7500L,
        Each(0.98),
        Each(0.0),
        self.toTyped,
      )

      /* Expect, the correct values (this model has fixed power factor) */
      expectMsgClass(classOf[AssetPowerChangedMessage]) match {
        case AssetPowerChangedMessage(p, q) =>
          p should approximate(Megawatts(0d))
          q should approximate(Megavars(0d))
      }
    }
  }
}
