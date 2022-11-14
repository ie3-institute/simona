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
import edu.ie3.datamodel.models.input.system.FixedFeedInInput
import edu.ie3.datamodel.models.input.system.characteristic.QV
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.fixedfeedin.FixedFeedInAgent
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.{
  ParticipantInitializeStateData,
  ParticipantInitializingStateData,
  ParticipantUninitializedStateData,
  SimpleInputContainer
}
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.agent.state.ParticipantAgentState.HandleInformation
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.FixedFeedInRuntimeConfig
import edu.ie3.simona.event.notifier.NotifierConfig
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
import edu.ie3.simona.test.common.input.FixedFeedInputTestData
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.PowerSystemUnits.{
  KILOWATT,
  MEGAVAR,
  MEGAWATT,
  PU
}
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.concurrent.TimeUnit

class FixedFeedInAgentModelCalculationSpec
    extends ParticipantAgentSpec(
      ActorSystem(
        "FixedFeedInAgentModelCalculationSpec",
        ConfigFactory
          .parseString("""
            |akka.loggers =["akka.event.slf4j.Slf4jLogger"]
            |akka.loglevel="DEBUG"
        """.stripMargin)
      )
    )
    with FixedFeedInputTestData {
  implicit val receiveTimeOut: Timeout = Timeout(10, TimeUnit.SECONDS)
  implicit val noReceiveTimeOut: Timeout = Timeout(1, TimeUnit.SECONDS)

  /* Alter the input model to have a voltage sensitive reactive power calculation */
  val voltageSensitiveInput: FixedFeedInInput = fixedFeedInput
    .copy()
    .qCharacteristics(new QV("qV:{(0.95,-0.625),(1.05,0.625)}"))
    .build()

  protected val simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01 00:00:00")
  protected val simulationEndDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01 01:00:00")

  private val testingTolerance = 1e-6 // Equality on the basis of 1 W
  private val simonaConfig: SimonaConfig =
    createSimonaConfig(
      LoadModelBehaviour.FIX,
      LoadReference.ActivePower(Quantities.getQuantity(0d, KILOWATT))
    )
  private val defaultOutputConfig = NotifierConfig(
    simonaConfig.simona.output.participant.defaultConfig.simulationResult,
    simonaConfig.simona.output.participant.defaultConfig.powerRequestReply
  )

  private val fixedFeedConfigUtil = ConfigUtil.ParticipantConfigUtil(
    simonaConfig.simona.runtime.participant
  )
  private val modelConfig =
    fixedFeedConfigUtil.getOrDefault[FixedFeedInRuntimeConfig](
      voltageSensitiveInput.getUuid
    )
  private val services = None
  private val resolution = simonaConfig.simona.powerflow.resolution.getSeconds

  "A fixed feed in agent with model calculation " should {
    "be instantiated correctly" in {
      val fixedFeedAgent = TestFSMRef(
        new FixedFeedInAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      fixedFeedAgent.stateName shouldBe Uninitialized
      // ParticipantUninitializedStateData is an empty class (due to typing). If it contains content one day
      inside(fixedFeedAgent.stateData) {
        case _: ParticipantUninitializedStateData[_] => succeed
        case _ =>
          fail(
            s"Expected $ParticipantUninitializedStateData, but got ${fixedFeedAgent.stateData}."
          )
      }
    }

    "end in correct state with correct state data after initialisation" in {
      val fixedFeedAgent = TestFSMRef(
        new FixedFeedInAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val triggerId = 0
      scheduler.send(
        fixedFeedAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              FixedFeedInInput,
              FixedFeedInRuntimeConfig,
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
              primaryServiceProxy = primaryServiceProxy.ref,
              maybeEmAgent = None
            )
          ),
          triggerId,
          fixedFeedAgent
        )
      )

      /* Actor should ask for registration with primary service */
      primaryServiceProxy.expectMsg(
        PrimaryServiceRegistrationMessage(voltageSensitiveInput.getUuid)
      )
      /* State should be information handling and having correct state data */
      fixedFeedAgent.stateName shouldBe HandleInformation
      fixedFeedAgent.stateData match {
        case ParticipantInitializingStateData(
              inputModel,
              modelConfig,
              secondaryDataServices,
              simulationStartDate,
              simulationEndDate,
              resolution,
              requestVoltageDeviationThreshold,
              outputConfig,
              maybeEmAgent
            ) =>
          inputModel shouldBe SimpleInputContainer(voltageSensitiveInput)
          modelConfig shouldBe modelConfig
          secondaryDataServices shouldBe services
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
      primaryServiceProxy.send(fixedFeedAgent, RegistrationFailedMessage)

      /* Expect a completion notification */
      scheduler.expectMsg(
        CompletionMessage(
          triggerId,
          Some(
            List(
              ScheduleTriggerMessage(ActivityStartTrigger(0L), fixedFeedAgent)
            )
          )
        )
      )

      /* ... as well as corresponding state and state data */
      fixedFeedAgent.stateName shouldBe Idle
      fixedFeedAgent.stateData match {
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
              _,
              _,
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
          resultValueStore shouldBe ValueStore(resolution)
          requestValueStore shouldBe ValueStore[ApparentPower](
            resolution
          )
        case _ =>
          fail(
            s"Did not find expected state data $ParticipantModelBaseStateData, but ${fixedFeedAgent.stateData}"
          )
      }
    }

    "answer with zero power, if asked directly after initialisation" in {
      val fixedFeedAgent = TestFSMRef(
        new FixedFeedInAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val triggerId = 0
      scheduler.send(
        fixedFeedAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              FixedFeedInInput,
              FixedFeedInRuntimeConfig,
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
              primaryServiceProxy = primaryServiceProxy.ref,
              maybeEmAgent = None
            )
          ),
          triggerId,
          fixedFeedAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(fixedFeedAgent, RegistrationFailedMessage)

      /* I'm not interested in the content of the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]

      fixedFeedAgent.stateName shouldBe Idle
      /* State data has already been tested */

      fixedFeedAgent ! RequestAssetPowerMessage(
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

      inside(fixedFeedAgent.stateData) {
        case baseStateData: ParticipantModelBaseStateData[_, _, _, _] =>
          baseStateData.requestValueStore shouldBe ValueStore[
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
            s"Did not found the expected state data $ParticipantModelBaseStateData, but ${fixedFeedAgent.stateData}"
          )
      }
    }

    "do correct transitions when triggered in Idle" in {
      val fixedFeedAgent = TestFSMRef(
        new FixedFeedInAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val initialiseTriggerId = 0
      scheduler.send(
        fixedFeedAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              FixedFeedInInput,
              FixedFeedInRuntimeConfig,
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
              primaryServiceProxy = primaryServiceProxy.ref,
              maybeEmAgent = None
            )
          ),
          initialiseTriggerId,
          fixedFeedAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(fixedFeedAgent, RegistrationFailedMessage)

      /* I am not interested in the CompletionMessage */
      scheduler.expectMsgType[CompletionMessage]
      awaitAssert(fixedFeedAgent.stateName shouldBe Idle)
      /* State data is tested in another test */

      val activityStartTriggerId = 1
      scheduler.send(
        fixedFeedAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          activityStartTriggerId,
          fixedFeedAgent
        )
      )

      /* Expect confirmation */
      scheduler.expectMsg(CompletionMessage(activityStartTriggerId, None))

      /* Intermediate transitions and states cannot be tested, as the agents triggers itself
       * too fast */

      awaitAssert(fixedFeedAgent.stateName shouldBe Idle)
      inside(fixedFeedAgent.stateData) {
        case baseStateData: ParticipantModelBaseStateData[_, _, _, _] =>
          baseStateData.resultValueStore.last(0L) match {
            case Some((tick, entry)) =>
              tick shouldBe 0L
              inside(entry) { case ApparentPower(p, q) =>
                p should equalWithTolerance(
                  Quantities.getQuantity(-268.603e-6, MEGAWATT),
                  testingTolerance
                )
                q should equalWithTolerance(
                  Quantities.getQuantity(0d, MEGAVAR),
                  testingTolerance
                )
              }
            case None =>
              fail("Result value store does not contain entry for tick 900.")
          }
        case _ =>
          fail(
            s"Did not find the expected state data $ParticipantModelBaseStateData, but ${fixedFeedAgent.stateData}"
          )
      }
    }

    "provide the correct average power after one data tick is available" in {
      val fixedFeedAgent = TestFSMRef(
        new FixedFeedInAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      /* Trigger the initialisation */
      scheduler.send(
        fixedFeedAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              FixedFeedInInput,
              FixedFeedInRuntimeConfig,
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
              primaryServiceProxy = primaryServiceProxy.ref,
              maybeEmAgent = None
            )
          ),
          0L,
          fixedFeedAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(fixedFeedAgent, RegistrationFailedMessage)

      /* Trigger the data generation in tick 0 */
      scheduler.send(
        fixedFeedAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          1,
          fixedFeedAgent
        )
      )

      /* Appreciate the existence of two CompletionMessages */
      val completedTriggerIds = scheduler.receiveWhile(messages = 2) {
        case msg: CompletionMessage => msg.triggerId
      }
      logger.debug(
        s"Received CompletionMessages for the following trigger ids: $completedTriggerIds"
      )
      awaitAssert(fixedFeedAgent.stateName shouldBe Idle)

      /* Ask the agent for average power in tick 3000 */
      fixedFeedAgent ! RequestAssetPowerMessage(
        3000L,
        Quantities.getQuantity(1d, PU),
        Quantities.getQuantity(0d, PU)
      )

      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(-268.603e-6, MEGAWATT),
            testingTolerance
          )
          q should equalWithTolerance(
            Quantities.getQuantity(0d, MEGAVAR),
            testingTolerance
          )
      }
    }

    val fixedFeedAgent = TestFSMRef(
      new FixedFeedInAgent(
        scheduler = scheduler.ref,
        listener = systemListener
      )
    )

    "does answer unchanged power values after asking a second time with considerably same voltage" in {

      /* Trigger the initialisation */
      scheduler.send(
        fixedFeedAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              FixedFeedInInput,
              FixedFeedInRuntimeConfig,
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
              primaryServiceProxy = primaryServiceProxy.ref,
              maybeEmAgent = None
            )
          ),
          0,
          fixedFeedAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(fixedFeedAgent, RegistrationFailedMessage)

      /* Trigger the data generation in tick 0 */
      scheduler.send(
        fixedFeedAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          1,
          fixedFeedAgent
        )
      )

      /* Appreciate the existence of two CompletionMessages */
      val completedTriggerIds = scheduler.receiveWhile(messages = 2) {
        case msg: CompletionMessage => msg.triggerId
      }
      logger.debug(
        s"Received CompletionMessages for the following trigger ids: $completedTriggerIds"
      )

      /* Ask the agent for average power in tick 3000 */
      fixedFeedAgent ! RequestAssetPowerMessage(
        3000L,
        Quantities.getQuantity(1d, PU),
        Quantities.getQuantity(0d, PU)
      )

      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(-268.603e-6, MEGAWATT),
            testingTolerance
          )
          q should equalWithTolerance(
            Quantities.getQuantity(0d, MEGAVAR),
            testingTolerance
          )
        case answer => fail(s"Did not expect to get that answer: $answer")
      }
    }

    "replies unchanged power values after asking a second time" in {
      /* Previous request stems from previous test */
      /* Ask again with unchanged information */
      fixedFeedAgent ! RequestAssetPowerMessage(
        3000L,
        Quantities.getQuantity(1.000000000000001d, PU),
        Quantities.getQuantity(0d, PU)
      )

      /* Expect, that nothing has changed */
      expectMsgType[AssetPowerUnchangedMessage] match {
        case AssetPowerUnchangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(-268.603e-6, PowerSystemUnits.MEGAWATT),
            testingTolerance
          )
          q should equalWithTolerance(
            Quantities
              .getQuantity(0d, PowerSystemUnits.MEGAVAR),
            testingTolerance
          )
      }
    }

    "does answer changed power values after asking a second time with different voltage" in {
      /* Ask again with changed information */
      fixedFeedAgent ! RequestAssetPowerMessage(
        3000L,
        Quantities.getQuantity(0.98, PU),
        Quantities.getQuantity(0d, PU)
      )

      /* Expect, the correct values (this model has fixed power factor) */
      expectMsgClass(classOf[AssetPowerChangedMessage]) match {
        case AssetPowerChangedMessage(p, q) =>
          p should equalWithTolerance(
            Quantities.getQuantity(-0.000268603, MEGAWATT),
            testingTolerance
          )
          q should equalWithTolerance(
            Quantities.getQuantity(-22.07138418e-6, MEGAVAR),
            testingTolerance
          )
      }
    }
  }
}
