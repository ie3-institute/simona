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
import edu.ie3.datamodel.models.StandardUnits
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
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.WecRuntimeConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.model.participant.ModelState.ConstantState
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
import edu.ie3.util.quantities.PowerSystemUnits.{
  KILOWATT,
  MEGAVAR,
  MEGAWATT,
  PU
}
import edu.ie3.util.quantities.QuantityUtil
import org.scalatest.PrivateMethodTester
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.{CELSIUS, METRE_PER_SECOND}

import java.time.ZonedDateTime
import java.util.concurrent.TimeUnit
import scala.collection.SortedMap

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

  private val testingTolerance = 1e-6 // Equality on the basis of 1 W

  /* Assign this test to receive the result events from agent */
  override val systemListener: Iterable[ActorRef] = Vector(self)

  private val simonaConfig: SimonaConfig =
    createSimonaConfig(
      LoadModelBehaviour.FIX,
      LoadReference.ActivePower(Quantities.getQuantity(0d, KILOWATT))
    )
  private val configUtil = ConfigUtil.ParticipantConfigUtil(
    simonaConfig.simona.runtime.participant
  )
  private val modelConfig =
    configUtil.getOrDefault[WecRuntimeConfig](
      voltageSensitiveInput.getUuid
    )

  private val withServices = Some(
    Vector(ActorWeatherService(weatherService.ref))
  )

  private val resolution = simonaConfig.simona.powerflow.resolution.getSeconds

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
              WecRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = simonaConfig.simona.powerflow.resolution.getSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              modelConfig = modelConfig,
              primaryServiceProxy = primaryServiceProxy.ref,
              secondaryDataServices = None,
              outputConfig = NotifierConfig(
                simulationResultInfo = false,
                powerRequestReply = false
              ),
              scheduleTriggerFunc = scheduleTriggerFunc(wecAgent)
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
              WecRuntimeConfig,
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
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              primaryServiceProxy = primaryServiceProxy.ref,
              secondaryDataServices = withServices,
              outputConfig = NotifierConfig(
                simulationResultInfo = false,
                powerRequestReply = false
              ),
              scheduleTriggerFunc = scheduleTriggerFunc(wecAgent)
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
                _,
                _,
                _
              ),
              awaitRegistrationResponsesFrom,
              foreseenNextDataTicks
            ) =>
          /* Base state data */
          startDate shouldBe simulationStartDate
          endDate shouldBe simulationEndDate
          services shouldBe withServices
          outputConfig shouldBe NotifierConfig(
            simulationResultInfo = false,
            powerRequestReply = false
          )
          additionalActivationTicks shouldBe Array.emptyLongArray
          foreseenDataTicks shouldBe Map.empty
          voltageValueStore shouldBe ValueStore(
            resolution,
            SortedMap(0L -> Quantities.getQuantity(1d, PU))
          )
          resultValueStore shouldBe ValueStore(resolution)
          requestValueStore shouldBe ValueStore[ApparentPower](resolution)

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
            Seq(
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
              ConstantState.type,
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
              WecRuntimeConfig,
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
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              primaryServiceProxy = primaryServiceProxy.ref,
              secondaryDataServices = withServices,
              outputConfig = NotifierConfig(
                simulationResultInfo = false,
                powerRequestReply = false
              ),
              scheduleTriggerFunc = scheduleTriggerFunc(wecAgent)
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
        Quantities.getQuantity(1d, PU),
        Quantities.getQuantity(0d, PU)
      )
      expectMsg(
        AssetPowerChangedMessage(
          Quantities.getQuantity(0d, MEGAWATT),
          Quantities.getQuantity(0d, MEGAVAR)
        )
      )

      inside(wecAgent.stateData) {
        case modelBaseStateData: ParticipantModelBaseStateData[
              ApparentPower,
              WecRelevantData,
              ConstantState.type,
              WecModel
            ] =>
          modelBaseStateData.requestValueStore shouldBe ValueStore[
            ApparentPower
          ](
            resolution,
            SortedMap(
              0L -> ApparentPower(
                Quantities.getQuantity(0d, MEGAWATT),
                Quantities.getQuantity(0d, MEGAVAR)
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
              WecRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              modelConfig = modelConfig,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = simonaConfig.simona.powerflow.resolution.getSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              primaryServiceProxy = primaryServiceProxy.ref,
              secondaryDataServices = withServices,
              outputConfig = NotifierConfig(
                simulationResultInfo = false,
                powerRequestReply = false
              ),
              scheduleTriggerFunc = scheduleTriggerFunc(wecAgent)
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
        Quantities.getQuantity(50, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(100, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(0, CELSIUS),
        Quantities.getQuantity(0, METRE_PER_SECOND)
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
                ConstantState.type,
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
            Seq(ScheduleTriggerMessage(ActivityStartTrigger(1800L), wecAgent))
          )
        )
      )
      wecAgent.stateName shouldBe Idle
      wecAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[
              ApparentPower,
              WecRelevantData,
              ConstantState.type,
              WecModel
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
                fail("Expected a simulation result for tick 900.")
              ) match {
                case ApparentPower(p, q) =>
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
              WecRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              modelConfig = modelConfig,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = simonaConfig.simona.powerflow.resolution.getSeconds,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              primaryServiceProxy = primaryServiceProxy.ref,
              secondaryDataServices = withServices,
              outputConfig = NotifierConfig(
                simulationResultInfo = false,
                powerRequestReply = false
              ),
              scheduleTriggerFunc = scheduleTriggerFunc(wecAgent)
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
                ConstantState.type,
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
        Quantities.getQuantity(50, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(100, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(0, CELSIUS),
        Quantities.getQuantity(0, METRE_PER_SECOND)
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
            Seq(ScheduleTriggerMessage(ActivityStartTrigger(1800L), wecAgent))
          )
        )
      )

      /* Expect the state change to idle with updated base state data */
      wecAgent.stateName shouldBe Idle
      wecAgent.stateData match {
        case baseStateData: ParticipantModelBaseStateData[
              ApparentPower,
              WecRelevantData,
              ConstantState.type,
              WecModel
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
                fail("Expected a simulation result for tick 900.")
              ) match {
                case ApparentPower(p, q) =>
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
              WecRuntimeConfig,
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
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              primaryServiceProxy = primaryServiceProxy.ref,
              secondaryDataServices = withServices,
              outputConfig = NotifierConfig(
                simulationResultInfo = false,
                powerRequestReply = false
              ),
              scheduleTriggerFunc = scheduleTriggerFunc(wecAgent)
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
        Quantities.getQuantity(1d, PU),
        Quantities.getQuantity(0d, PU)
      )
      expectNoMessage(noReceiveTimeOut.duration)
      awaitAssert(wecAgent.stateName == Idle)

      /* Send out the expected data and wait for the reply */
      val weatherData = WeatherData(
        Quantities.getQuantity(50, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(100, StandardUnits.SOLAR_IRRADIANCE),
        Quantities.getQuantity(0, CELSIUS),
        Quantities.getQuantity(0, METRE_PER_SECOND)
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
            Seq(ScheduleTriggerMessage(ActivityStartTrigger(1800L), wecAgent))
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
              WecRuntimeConfig,
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
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              primaryServiceProxy = primaryServiceProxy.ref,
              secondaryDataServices = withServices,
              outputConfig = NotifierConfig(
                simulationResultInfo = false,
                powerRequestReply = false
              ),
              scheduleTriggerFunc = scheduleTriggerFunc(wecAgent)
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
            Quantities.getQuantity(50, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(100, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(0, CELSIUS),
            Quantities.getQuantity(0, METRE_PER_SECOND)
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
            Seq(ScheduleTriggerMessage(ActivityStartTrigger(1800L), wecAgent))
          )
        )
      )

      /* ... for tick 1800 */
      weatherService.send(
        wecAgent,
        ProvideWeatherMessage(
          1800L,
          WeatherData(
            Quantities.getQuantity(50, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(100, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(0, CELSIUS),
            Quantities.getQuantity(0, METRE_PER_SECOND)
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
            Seq(ScheduleTriggerMessage(ActivityStartTrigger(2700L), wecAgent))
          )
        )
      )

      /* ... for tick 2700 */
      weatherService.send(
        wecAgent,
        ProvideWeatherMessage(
          2700L,
          WeatherData(
            Quantities.getQuantity(50, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(100, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(0, CELSIUS),
            Quantities.getQuantity(0, METRE_PER_SECOND)
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
      wecAgent ! RequestAssetPowerMessage(
        3000L,
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
      wecAgent ! RequestAssetPowerMessage(
        3000L,
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
            Quantities.getQuantity(-156.1249e-3, MEGAVAR),
            testingTolerance
          )
      }
    }
  }
}
