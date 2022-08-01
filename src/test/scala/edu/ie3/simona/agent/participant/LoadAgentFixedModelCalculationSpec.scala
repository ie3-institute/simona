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
import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.input.system.characteristic.QV
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.load.LoadAgent.FixedLoadAgent
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.{
  ParticipantInitializeStateData,
  ParticipantInitializingStateData,
  ParticipantUninitializedStateData
}
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.agent.state.ParticipantAgentState.HandleInformation
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.LoadRuntimeConfig
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.model.participant.load.{LoadModelBehaviour, LoadReference}
import edu.ie3.simona.ontology.messages.PowerMessage.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
  RequestAssetPowerMessage
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationFailedMessage
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeParticipantAgentTrigger
}
import edu.ie3.simona.test.ParticipantAgentSpec
import edu.ie3.simona.test.common.model.participant.LoadTestData
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.util.quantities.PowerSystemUnits._
import org.scalatest.PrivateMethodTester
import tech.units.indriya.quantity.Quantities

import java.util.concurrent.TimeUnit

class LoadAgentFixedModelCalculationSpec
    extends ParticipantAgentSpec(
      ActorSystem(
        "LoadAgentSpec",
        ConfigFactory
          .parseString("""
            |akka.loggers =["akka.event.slf4j.Slf4jLogger"]
            |akka.loglevel="DEBUG"
        """.stripMargin)
      )
    )
    with LoadTestData
    with PrivateMethodTester {
  implicit val receiveTimeOut: Timeout = Timeout(10, TimeUnit.SECONDS)
  implicit val noReceiveTimeOut: Timeout = Timeout(1, TimeUnit.SECONDS)

  /* Alter the input model to have a voltage sensitive reactive power calculation */
  val voltageSensitiveInput: LoadInput = loadInput
    .copy()
    .qCharacteristics(new QV("qV:{(0.95,-0.625),(1.05,0.625)}"))
    .build()

  private val testingTolerance = 1e-6 // Equality on the basis of 1 W
  private val simonaConfig: SimonaConfig =
    createSimonaConfig(
      LoadModelBehaviour.FIX,
      LoadReference.ActivePower(Quantities.getQuantity(0d, KILOWATT))
    )
  private val defaultOutputConfig = ParticipantNotifierConfig(
    simonaConfig.simona.output.participant.defaultConfig.simulationResult,
    simonaConfig.simona.output.participant.defaultConfig.powerRequestReply
  )
  private val loadConfigUtil = ConfigUtil.ParticipantConfigUtil(
    simonaConfig.simona.runtime.participant
  )
  private val modelConfig =
    loadConfigUtil.getOrDefault[LoadRuntimeConfig](
      voltageSensitiveInput.getUuid
    )
  private val services = None
  private val resolution = simonaConfig.simona.powerflow.resolution.getSeconds

  "A load agent with fixed model calculation depending on no secondary data service" should {
    "be instantiated correctly" in {
      val loadAgent = TestFSMRef(
        new FixedLoadAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      loadAgent.stateName shouldBe Uninitialized
      // ParticipantUninitializedStateData is an empty class (due to typing). If it contains content one day
      inside(loadAgent.stateData) {
        case _: ParticipantUninitializedStateData[_] => succeed
        case _ =>
          fail(
            s"Expected $ParticipantUninitializedStateData, but got ${loadAgent.stateData}."
          )
      }
    }

    "end in correct state with correct state data after initialisation" in {
      val loadAgent = TestFSMRef(
        new FixedLoadAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val triggerId = 0
      scheduler.send(
        loadAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              LoadInput,
              LoadRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              modelConfig = modelConfig,
              secondaryDataServices = services,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          triggerId,
          loadAgent
        )
      )

      /* Actor should ask for registration with primary service */
      primaryServiceProxy.expectMsg(
        PrimaryServiceRegistrationMessage(voltageSensitiveInput.getUuid)
      )
      /* State should be information handling and having correct state data */
      loadAgent.stateName shouldBe HandleInformation
      loadAgent.stateData match {
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
          secondaryDataServices shouldBe services
          simulationStartDate shouldBe this.simulationStartDate
          simulationEndDate shouldBe this.simulationEndDate
          resolution shouldBe this.resolution
          requestVoltageDeviationThreshold shouldBe simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold
          outputConfig shouldBe defaultOutputConfig
        case unsuitableStateData =>
          fail(s"Agent has unsuitable state data '$unsuitableStateData'.")
      }

      /* Refuse registration */
      primaryServiceProxy.send(loadAgent, RegistrationFailedMessage)

      /* Expect a completion notification */
      scheduler.expectMsg(
        CompletionMessage(
          triggerId,
          Some(
            List(
              ScheduleTriggerMessage(ActivityStartTrigger(0L), loadAgent)
            )
          )
        )
      )

      /* ... as well as corresponding state and state data */
      loadAgent.stateName shouldBe Idle
      loadAgent.stateData match {
        case ParticipantModelBaseStateData(
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
            ) =>
          /* Base state data */
          startDate shouldBe simulationStartDate
          endDate shouldBe simulationEndDate
          services shouldBe None
          outputConfig shouldBe defaultOutputConfig
          additionalActivationTicks shouldBe Array.emptyLongArray
          foreseenDataTicks shouldBe Map.empty
          voltageValueStore shouldBe ValueStore(
            resolution,
            Map(0L -> Quantities.getQuantity(1d, PU))
          )
          resultValueStore shouldBe ValueStore.forResult(
            resolution,
            2
          )
          requestValueStore shouldBe ValueStore[ApparentPower](
            resolution
          )
        case _ =>
          fail(
            s"Did not find expected state data $ParticipantModelBaseStateData, but ${loadAgent.stateData}"
          )
      }
    }

    "answer with zero power, if asked directly after initialisation" in {
      val loadAgent = TestFSMRef(
        new FixedLoadAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val triggerId = 0
      scheduler.send(
        loadAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              LoadInput,
              LoadRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              modelConfig = modelConfig,
              secondaryDataServices = services,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          triggerId,
          loadAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(loadAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]

      loadAgent.stateName shouldBe Idle
      /* State data has already been tested */

      loadAgent ! RequestAssetPowerMessage(
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

      inside(loadAgent.stateData) {
        case modelBaseStateData: ParticipantModelBaseStateData[_, _, _] =>
          modelBaseStateData.requestValueStore shouldBe ValueStore[
            ApparentPower
          ](
            resolution,
            Map(
              0L -> ApparentPower(
                Quantities.getQuantity(0d, MEGAWATT),
                Quantities.getQuantity(0d, MEGAVAR)
              )
            )
          )
        case _ =>
          fail(
            s"Did not find expected state data $ParticipantModelBaseStateData, but ${loadAgent.stateData}"
          )
      }
    }

    "do correct transitions when triggered in Idle" in {
      val loadAgent = TestFSMRef(
        new FixedLoadAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val initialiseTriggerId = 0
      scheduler.send(
        loadAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              LoadInput,
              LoadRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              modelConfig = modelConfig,
              secondaryDataServices = services,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          initialiseTriggerId,
          loadAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(loadAgent, RegistrationFailedMessage)

      /* I am not interested in the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(loadAgent.stateName shouldBe Idle)
      /* State data is tested in another test */

      val activityStartTriggerId = 1
      scheduler.send(
        loadAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          activityStartTriggerId,
          loadAgent
        )
      )

      /* Expect confirmation */
      scheduler.expectMsg(CompletionMessage(activityStartTriggerId, None))

      /* Intermediate transitions and states cannot be tested, as the agents triggers itself
       * too fast */

      awaitAssert(loadAgent.stateName shouldBe Idle)
      inside(loadAgent.stateData) {
        case participantModelBaseStateData: ParticipantModelBaseStateData[
              _,
              _,
              _
            ] =>
          participantModelBaseStateData.resultValueStore.last(0L) match {
            case Some((tick, entry)) =>
              tick shouldBe 0L
              inside(entry) { case ApparentPower(p, q) =>
                p should equalWithTolerance(
                  Quantities.getQuantity(268.603e-6, MEGAWATT),
                  testingTolerance
                )
                q should equalWithTolerance(
                  Quantities.getQuantity(0d, MEGAVAR),
                  testingTolerance
                )
              }
            case None =>
              fail("Result value store does not contain entry for tick 0.")
          }
        case _ =>
          fail(
            s"Did not find the expected state data $ParticipantModelBaseStateData, but ${loadAgent.stateData}"
          )
      }
    }

    "provide the correct average power after one data tick is available" in {
      val loadAgent = TestFSMRef(
        new FixedLoadAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      /* Trigger the initialisation */
      scheduler.send(
        loadAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              LoadInput,
              LoadRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = voltageSensitiveInput,
              modelConfig = modelConfig,
              secondaryDataServices = services,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          0L,
          loadAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(loadAgent, RegistrationFailedMessage)

      /* Trigger the data generation in tick 0 */
      scheduler.send(
        loadAgent,
        TriggerWithIdMessage(ActivityStartTrigger(0L), 1L, loadAgent)
      )

      /* Appreciate the existence of two CompletionMessages */
      val completedTriggerIds = scheduler.receiveWhile(messages = 2) {
        case msg: CompletionMessage => msg.triggerId
      }
      logger.debug(
        s"Received CompletionMessages for the following trigger ids: $completedTriggerIds"
      )
      awaitAssert(loadAgent.stateName shouldBe Idle)

      /* Ask the agent for average power in tick 3000 */
      loadAgent ! RequestAssetPowerMessage(
        3000L,
        Quantities.getQuantity(1d, PU),
        Quantities.getQuantity(0d, PU)
      )

      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(268.603e-6, MEGAWATT),
            testingTolerance
          )
          q should equalWithTolerance(
            Quantities.getQuantity(0d, MEGAVAR),
            testingTolerance
          )
      }
    }

    val loadAgent = TestFSMRef(
      new FixedLoadAgent(
        scheduler = scheduler.ref,
        listener = systemListener
      )
    )

    "does answer unchanged power values after asking a second time with considerably same voltage" in {

      /* Trigger the initialisation */
      scheduler.send(
        loadAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              LoadInput,
              LoadRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              inputModel = voltageSensitiveInput,
              modelConfig = modelConfig,
              secondaryDataServices = services,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref
            )
          ),
          0L,
          loadAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(loadAgent, RegistrationFailedMessage)

      /* Trigger the data generation in tick 0 */
      scheduler.send(
        loadAgent,
        TriggerWithIdMessage(ActivityStartTrigger(0L), 1, loadAgent)
      )

      /* Appreciate the existence of two CompletionMessages */
      val completedTriggerIds = scheduler.receiveWhile(messages = 2) {
        case msg: CompletionMessage => msg.triggerId
      }
      logger.debug(
        s"Received CompletionMessages for the following trigger ids: $completedTriggerIds"
      )

      /* Ask the agent for average power in tick 3000 */
      loadAgent ! RequestAssetPowerMessage(
        3000L,
        Quantities.getQuantity(1d, PU),
        Quantities.getQuantity(0d, PU)
      )

      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(268.603e-6, MEGAWATT),
            testingTolerance
          )
          q should equalWithTolerance(
            Quantities.getQuantity(0d, MEGAVAR),
            testingTolerance
          )
        case answer => fail(s"Did not expect to get that answer: $answer")
      }
    }

    "reply unchanged power values after asking a second time with considerably same voltage" in {
      /* Previous request stems from previous test */
      /* Ask again with unchanged information */
      loadAgent ! RequestAssetPowerMessage(
        3000L,
        Quantities.getQuantity(1.000000000000001d, PU),
        Quantities.getQuantity(0d, PU)
      )

      /* Expect, that nothing has changed */
      expectMsgType[AssetPowerUnchangedMessage] match {
        case AssetPowerUnchangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(268.603e-6, MEGAWATT),
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
      loadAgent ! RequestAssetPowerMessage(
        3000L,
        Quantities.getQuantity(0.98, PU),
        Quantities.getQuantity(0d, PU)
      )

      /* Expect, the correct values (this model has fixed power factor) */
      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(268.603e-6, MEGAWATT),
            testingTolerance
          )
          q should equalWithTolerance(
            Quantities.getQuantity(-22.07138e-6, MEGAVAR),
            testingTolerance
          )
      }
    }
  }
}
