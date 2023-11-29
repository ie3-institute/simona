/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.testkit.{TestFSMRef, TestProbe}
import org.apache.pekko.util.Timeout
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.input.system.HpInput
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPowerAndHeat
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
import edu.ie3.simona.ontology.messages.PowerMessage.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
  RequestAssetPowerMessage
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
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
import edu.ie3.simona.test.ParticipantAgentSpec
import edu.ie3.simona.test.common.model.participant.HpTestData
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.scala.quantities.{
  Megavars,
  ReactivePower,
  Vars,
  WattsPerSquareMeter
}
import org.scalatest.PrivateMethodTester
import squants.energy.{Kilowatts, Megawatts, Watts}
import squants.motion.MetersPerSecond
import squants.thermal.Celsius
import squants.{Dimensionless, Each, Power, Temperature}

import java.io.File
import java.time.ZonedDateTime
import java.util.concurrent.TimeUnit

class HpAgentModelCalculationSpec
    extends ParticipantAgentSpec(
      ActorSystem(
        "HpAgentSpec",
        ConfigFactory
          .parseString("""
            |pekko.loggers =["org.apache.pekko.event.slf4j.Slf4jLogger"]
            |pekko.loglevel="DEBUG"
        """.stripMargin)
      )
    )
    with HpTestData
    with IntegrationSpecCommon
    with PrivateMethodTester {
  implicit val simulationStart: ZonedDateTime = defaultSimulationStart
  implicit val receiveTimeOut: Timeout = Timeout(10, TimeUnit.SECONDS)
  implicit val noReceiveTimeOut: Timeout = Timeout(1, TimeUnit.SECONDS)
  implicit val powerTolerance: Power = Watts(1e-3)
  implicit val reactivepowerTolerance: ReactivePower = Vars(1e-3)
  implicit val temperatureTolerance: Temperature = Celsius(1e-10)
  implicit val dimensionlessTolerance: Dimensionless = Each(1e-10)
  /* Alter the input model to have a voltage sensitive reactive power calculation */
  val hpInput: HpInput = inputModel

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
    val initStateData = ParticipantInitializeStateData[
      HpInput,
      HpRuntimeConfig,
      ApparentPowerAndHeat
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
      primaryServiceProxy = primaryServiceProxy.ref
    )

    "be instantiated correctly" in {
      val hpAgent = TestFSMRef(
        new HpAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
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
      val deathProbe = TestProbe("deathProbe")

      val hpAgent = TestFSMRef(
        new HpAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = systemListener
        )
      )

      scheduler.send(hpAgent, Activation(INIT_SIM_TICK))

      deathProbe.watch(hpAgent)

      /* Agent attempts to register with primary data service -- refuse this */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(hpAgent, RegistrationFailedMessage)

      deathProbe.expectTerminated(hpAgent)
    }
  }

  "A heat pump agent depending on one secondary data service" should {
    val initStateData = ParticipantInitializeStateData[
      HpInput,
      HpRuntimeConfig,
      ApparentPowerAndHeat
    ](
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

    "be instantiated correctly" in {
      val hpAgent = TestFSMRef(
        new HpAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
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
          initStateData = initStateData,
          listener = systemListener
        )
      )

      scheduler.send(hpAgent, Activation(INIT_SIM_TICK))

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
          additionalActivationTicks shouldBe empty
          foreseenDataTicks shouldBe Map.empty
          voltageValueStore shouldBe ValueStore(
            resolution,
            Map(0L -> Each(1d))
          )
          resultValueStore shouldBe ValueStore(resolution)
          requestValueStore shouldBe ValueStore[ApparentPowerAndHeat](
            resolution
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
      scheduler.expectMsg(Completion(hpAgent.toTyped, Some(4711L)))

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
          initStateData = initStateData,
          listener = systemListener
        )
      )

      scheduler.send(hpAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(hpAgent, RegistrationFailedMessage)

      /* Expect a registration message */
      weatherService.expectMsg(
        RegisterForWeatherMessage(51.4843281, 7.4116482)
      )
      weatherService.send(hpAgent, RegistrationSuccessfulMessage(Some(900L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[Completion]

      hpAgent.stateName shouldBe Idle
      /* State data has already been tested */

      hpAgent ! RequestAssetPowerMessage(
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

      inside(hpAgent.stateData) {
        case modelBaseStateData: ParticipantModelBaseStateData[_, _, _] =>
          modelBaseStateData.requestValueStore shouldBe ValueStore[
            ApparentPowerAndHeat
          ](
            resolution,
            Map(
              0L -> ApparentPowerAndHeat(
                Megawatts(0d),
                Megavars(0d),
                Megawatts(0d)
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
          initStateData = initStateData,
          listener = systemListener
        )
      )

      scheduler.send(hpAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(hpAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(hpAgent, RegistrationSuccessfulMessage(Some(0L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[Completion]
      awaitAssert(hpAgent.stateName shouldBe Idle)
      /* State data is tested in another test */

      /* Send out new data */
      val weatherData = WeatherData(
        WattsPerSquareMeter(0d),
        WattsPerSquareMeter(0d),
        Celsius(1.815d),
        MetersPerSecond(7.726576d)
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
      scheduler.send(hpAgent, Activation(0))

      /* The agent will notice, that all expected information are apparent, switch to Calculate and trigger itself
       * for starting the calculation */
      scheduler.expectMsg(Completion(hpAgent.toTyped, Some(3600)))

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
                  (activePower =~ Kilowatts(0d)) shouldBe true

                  (qDot =~
                    Kilowatts(0d)) shouldBe true

                  thermalGridState.houseState match {
                    case Some(ThermalHouseState(_, innerTemperature, _)) =>
                      (innerTemperature =~
                        Celsius(
                          20.9999769069444444444444444444444
                        )) shouldBe true
                    case None =>
                      fail(
                        s"Expected to get a result for thermal house '${inputModel.getUuid}'"
                      )
                  }

                  currentTimeTick shouldBe 0L
                  (ambientTemperature =~ Celsius(1.815d)) shouldBe true
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
                  (p =~ Megawatts(0d)) shouldBe true
                  q =~ Megavars(0d) shouldBe true
                  qDot =~ Megawatts(0d) shouldBe true
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
          listener = systemListener
        )
      )

      scheduler.send(hpAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(hpAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(hpAgent, RegistrationSuccessfulMessage(Some(0L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[Completion]
      awaitAssert(hpAgent.stateName shouldBe Idle)

      /* Send out an activation*/
      scheduler.send(hpAgent, Activation(0))

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
        WattsPerSquareMeter(0d),
        WattsPerSquareMeter(0d),
        Celsius(1.815d),
        MetersPerSecond(7.726576d)
      )

      weatherService.send(
        hpAgent,
        ProvideWeatherMessage(0L, weatherData, Some(3600L))
      )

      /* Expect confirmation */
      scheduler.expectMsg(Completion(hpAgent.toTyped, Some(3600)))

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
                  (activePower =~ Kilowatts(0d)) shouldBe true

                  (qDot =~ Kilowatts(0d)) shouldBe true

                  thermalGridState.houseState match {
                    case Some(ThermalHouseState(_, innerTemperature, _)) =>
                      (innerTemperature =~ Celsius(
                        20.9999769069444444444444444444444
                      )) shouldBe true
                    case None =>
                      fail(
                        s"Expected to get a result for thermal house '${inputModel.getUuid}'"
                      )
                  }

                  currentTimeTick shouldBe 0L
                  (ambientTemperature =~
                    Celsius(1.815d)) shouldBe true
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
                  (p =~ Megawatts(0d)) shouldBe true
                  (q =~ Megavars(0d)) shouldBe true
                  (
                    qDot =~ Megawatts(0d)
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
          initStateData = initStateData,
          listener = systemListener
        )
      )

      /* Trigger the initialisation */
      scheduler.send(hpAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(hpAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(hpAgent, RegistrationSuccessfulMessage(Some(3600L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[Completion]
      awaitAssert(hpAgent.stateName shouldBe Idle)

      /* Ask the agent for average power in tick 7200 */
      hpAgent ! RequestAssetPowerMessage(
        7200L,
        Each(1d),
        Each(0d)
      )
      expectNoMessage(noReceiveTimeOut.duration)
      awaitAssert(hpAgent.stateName == Idle)

      /* Send out the expected data and wait for the reply */
      val weatherData = WeatherData(
        WattsPerSquareMeter(0d),
        WattsPerSquareMeter(0d),
        Celsius(1.815d),
        MetersPerSecond(7.726576d)
      )
      weatherService.send(
        hpAgent,
        ProvideWeatherMessage(3600L, weatherData, Some(7200L))
      )

      /* Trigger the agent */
      scheduler.send(hpAgent, Activation(3600))

      /* The agent will notice, that all expected information are apparent, switch to Calculate and trigger itself
       * for starting the calculation */
      scheduler.expectMsg(Completion(hpAgent.toTyped, Some(7200)))

      /* Appreciate the answer to my previous request */
      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          (p =~ Megawatts(0d)) shouldBe true
          (q =~ Megavars(0d)) shouldBe true
      }
    }

    val hpAgent = TestFSMRef(
      new HpAgent(
        scheduler = scheduler.ref,
        initStateData = initStateData,
        listener = systemListener
      )
    )

    "provide correct average power after three data ticks are available" in {
      /* Trigger the initialisation */
      scheduler.send(hpAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(hpAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the RegistrationMessage */
      weatherService.expectMsgType[RegisterForWeatherMessage]
      weatherService.send(hpAgent, RegistrationSuccessfulMessage(Some(0L)))

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[Completion]
      awaitAssert(hpAgent.stateName shouldBe Idle)

      /* Send out the three data points */
      /* ... for tick 0 */
      weatherService.send(
        hpAgent,
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
      scheduler.send(hpAgent, Activation(0))
      scheduler.expectMsg(Completion(hpAgent.toTyped, Some(3600)))

      /* ... for tick 3600 */
      weatherService.send(
        hpAgent,
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
      scheduler.send(hpAgent, Activation(3600))
      scheduler.expectMsg(Completion(hpAgent.toTyped, Some(7200)))

      /* ... for tick 7200 */
      weatherService.send(
        hpAgent,
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
      scheduler.send(hpAgent, Activation(7200))
      scheduler.expectMsg(Completion(hpAgent.toTyped))

      /* Ask the agent for average power in tick 7500 */
      hpAgent ! RequestAssetPowerMessage(
        7500L,
        Each(1d),
        Each(0d)
      )

      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          (p =~ Megawatts(0d)) shouldBe true
          (q =~ Megavars(0d)) shouldBe true

        case answer => fail(s"Did not expect to get that answer: $answer")
      }
    }

    "answer unchanged power values after asking a second time with considerably same voltage" in {
      /* Previous request stems from previous test */
      /* Ask again with (nearly) unchanged information */
      hpAgent ! RequestAssetPowerMessage(
        7500L,
        Each(1.000000000000001d),
        Each(0d)
      )

      /* Expect, that nothing has changed */
      expectMsgType[AssetPowerUnchangedMessage] match {
        case AssetPowerUnchangedMessage(p, q) =>
          (p =~ Megawatts(0d)) shouldBe true
          (q =~ Megavars(0d)) shouldBe true
      }
    }

    "answer changed power values after asking a second time with different voltage" in {
      /* Ask again with changed information */
      hpAgent ! RequestAssetPowerMessage(
        7500L,
        Each(0.98d),
        Each(0d)
      )

      /* Expect, the correct values (this model has fixed power factor) */
      expectMsgClass(classOf[AssetPowerChangedMessage]) match {
        case AssetPowerChangedMessage(p, q) =>
          (p =~ Megawatts(0d)) shouldBe true
          (q =~ Megavars(0d)) shouldBe true

      }
    }
  }
}
