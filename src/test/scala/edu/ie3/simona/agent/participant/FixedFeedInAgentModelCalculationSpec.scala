/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.input.system.FixedFeedInInput
import edu.ie3.datamodel.models.input.system.characteristic.QV
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.grid.GridAgentMessages.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPower
import edu.ie3.simona.agent.participant.fixedfeedin.FixedFeedInAgent
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.{
  ParticipantInitializeStateData,
  ParticipantInitializingStateData,
  ParticipantUninitializedStateData,
  SimpleInputContainer,
}
import edu.ie3.simona.agent.participant2.ParticipantAgent.{
  RegistrationFailedMessage,
  RequestAssetPowerMessage,
}
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.agent.state.ParticipantAgentState.HandleInformation
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.RuntimeConfig.FixedFeedInRuntimeConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.model.participant.load.{LoadModelBehaviour, LoadReference}
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.test.ParticipantAgentSpec
import edu.ie3.simona.test.common.input.FixedFeedInputTestData
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.quantities.{Megavars, ReactivePower, Vars}
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.testkit.TestFSMRef
import org.apache.pekko.util.Timeout
import squants.Each
import squants.energy.{Kilowatts, Megawatts, Watts}

import java.time.ZonedDateTime
import java.util.concurrent.TimeUnit
import scala.collection.SortedMap

class FixedFeedInAgentModelCalculationSpec
    extends ParticipantAgentSpec(
      ActorSystem(
        "FixedFeedInAgentModelCalculationSpec",
        ConfigFactory
          .parseString("""
            |pekko.loggers =["org.apache.pekko.event.slf4j.Slf4jLogger"]
            |pekko.loglevel="DEBUG"
        """.stripMargin),
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
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z")
  protected val simulationEndDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T01:00:00Z")

  private implicit val powerTolerance: squants.Power = Watts(0.1)
  private implicit val reactivePowerTolerance: ReactivePower = Vars(0.1)

  private val simonaConfig: SimonaConfig =
    createSimonaConfig(
      LoadModelBehaviour.FIX,
      LoadReference.ActivePower(Kilowatts(0d)),
    )
  private val defaultOutputConfig = NotifierConfig(
    simonaConfig.simona.output.participant.defaultConfig.simulationResult,
    simonaConfig.simona.output.participant.defaultConfig.powerRequestReply,
    simonaConfig.simona.output.participant.defaultConfig.flexResult,
  )

  private val fixedFeedConfigUtil = ConfigUtil.ParticipantConfigUtil(
    simonaConfig.simona.runtime.participant
  )
  private val modelConfig =
    fixedFeedConfigUtil.getOrDefault[FixedFeedInRuntimeConfig](
      voltageSensitiveInput.getUuid
    )
  private val services = Iterable.empty
  private val resolution = simonaConfig.simona.powerflow.resolution.getSeconds

  "A fixed feed in agent with model calculation " should {
    val initStateData = ParticipantInitializeStateData[
      FixedFeedInInput,
      FixedFeedInRuntimeConfig,
      ComplexPower,
    ](
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
    )

    "be instantiated correctly" in {
      val fixedFeedAgent = TestFSMRef(
        new FixedFeedInAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = systemListener,
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
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      scheduler.send(fixedFeedAgent, Activation(INIT_SIM_TICK))

      /* Actor should ask for registration with primary service */
      primaryServiceProxy.expectMsg(
        PrimaryServiceRegistrationMessage(
          fixedFeedAgent.ref,
          voltageSensitiveInput.getUuid,
        )
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
              maybeEmAgent,
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
      primaryServiceProxy.send(
        fixedFeedAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* Expect a completion notification */
      scheduler.expectMsg(Completion(fixedFeedAgent.toTyped, Some(0)))

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
              _,
            ) =>
          /* Base state data */
          startDate shouldBe simulationStartDate
          endDate shouldBe simulationEndDate
          services shouldBe Iterable.empty
          outputConfig shouldBe defaultOutputConfig
          additionalActivationTicks shouldBe empty
          foreseenDataTicks shouldBe Map.empty
          voltageValueStore shouldBe ValueStore(
            resolution,
            SortedMap(0L -> Each(1.0)),
          )
          resultValueStore shouldBe ValueStore(resolution)
          requestValueStore shouldBe ValueStore[ComplexPower](
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
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      scheduler.send(fixedFeedAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        fixedFeedAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]

      fixedFeedAgent.stateName shouldBe Idle
      /* State data has already been tested */

      fixedFeedAgent ! RequestAssetPowerMessage(
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

      inside(fixedFeedAgent.stateData) {
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
            s"Did not found the expected state data $ParticipantModelBaseStateData, but ${fixedFeedAgent.stateData}"
          )
      }
    }

    "do correct transitions when triggered in Idle" in {
      val fixedFeedAgent = TestFSMRef(
        new FixedFeedInAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      scheduler.send(fixedFeedAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        fixedFeedAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* I am not interested in the Completion */
      scheduler.expectMsgType[Completion]
      awaitAssert(fixedFeedAgent.stateName shouldBe Idle)
      /* State data is tested in another test */

      scheduler.send(fixedFeedAgent, Activation(0))

      /* Expect confirmation */
      scheduler.expectMsg(Completion(fixedFeedAgent.toTyped, None))

      /* Intermediate transitions and states cannot be tested, as the agents triggers itself
       * too fast */

      awaitAssert(fixedFeedAgent.stateName shouldBe Idle)
      inside(fixedFeedAgent.stateData) {
        case baseStateData: ParticipantModelBaseStateData[_, _, _, _] =>
          baseStateData.resultValueStore.last(0L) match {
            case Some((tick, entry)) =>
              tick shouldBe 0L
              inside(entry) { case ComplexPower(p, q) =>
                p should approximate(Megawatts(-268.603e-6))
                q should approximate(Megavars(0.0))
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
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      /* Trigger the initialisation */
      scheduler.send(fixedFeedAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        fixedFeedAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      scheduler.expectMsg(Completion(fixedFeedAgent.toTyped, Some(0)))

      /* Trigger the data generation in tick 0 */
      scheduler.send(fixedFeedAgent, Activation(0))

      scheduler.expectMsg(Completion(fixedFeedAgent.toTyped))

      awaitAssert(fixedFeedAgent.stateName shouldBe Idle)

      /* Ask the agent for average power in tick 3000 */
      fixedFeedAgent ! RequestAssetPowerMessage(
        3000L,
        Each(1d),
        Each(0d),
        self.toTyped,
      )

      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should approximate(Megawatts(-268.603e-6))
          q should approximate(Megavars(0.0))
      }
    }

    val fixedFeedAgent = TestFSMRef(
      new FixedFeedInAgent(
        scheduler = scheduler.ref,
        initStateData = initStateData,
        listener = systemListener,
      )
    )

    "does answer unchanged power values after asking a second time with considerably same voltage" in {

      /* Trigger the initialisation */
      scheduler.send(fixedFeedAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        fixedFeedAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      scheduler.expectMsg(Completion(fixedFeedAgent.toTyped, Some(0)))

      /* Trigger the data generation in tick 0 */
      scheduler.send(fixedFeedAgent, Activation(0))

      scheduler.expectMsg(Completion(fixedFeedAgent.toTyped))

      /* Ask the agent for average power in tick 3000 */
      fixedFeedAgent ! RequestAssetPowerMessage(
        3000L,
        Each(1d),
        Each(0d),
        self.toTyped,
      )

      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should approximate(Megawatts(-268.603e-6))
          q should approximate(Megavars(0.0))
        case answer => fail(s"Did not expect to get that answer: $answer")
      }
    }

    "replies unchanged power values after asking a second time" in {
      /* Previous request stems from previous test */
      /* Ask again with unchanged information */
      fixedFeedAgent ! RequestAssetPowerMessage(
        3000L,
        Each(1.000000000000001d),
        Each(0d),
        self.toTyped,
      )

      /* Expect, that nothing has changed */
      expectMsgType[AssetPowerUnchangedMessage] match {
        case AssetPowerUnchangedMessage(p, q) =>
          p should approximate(Megawatts(-268.603e-6))
          q should approximate(Megavars(0.0))
      }
    }

    "does answer changed power values after asking a second time with different voltage" in {
      /* Ask again with changed information */
      fixedFeedAgent ! RequestAssetPowerMessage(
        3000L,
        Each(0.98),
        Each(0d),
        self.toTyped,
      )

      /* Expect, the correct values (this model has fixed power factor) */
      expectMsgClass(classOf[AssetPowerChangedMessage]) match {
        case AssetPowerChangedMessage(p, q) =>
          p should approximate(Megawatts(-0.000268603))
          q should approximate(Megavars(-22.07138418e-6))
      }
    }
  }
}
