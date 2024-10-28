/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.input.system.characteristic.QV
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.grid.GridAgentMessages.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
}
import edu.ie3.simona.agent.participant.ParticipantAgent.RequestAssetPowerMessage
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.load.LoadAgent.ProfileLoadAgent
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.{ParticipantInitializeStateData, ParticipantInitializingStateData, ParticipantUninitializedStateData,
  SimpleInputContainer,
}
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.agent.state.ParticipantAgentState.HandleInformation
import edu.ie3.simona.config.RuntimeConfig.LoadRuntimeConfig
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.model.participant.load.{LoadModelBehaviour, LoadReference}
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationFailedMessage
import edu.ie3.simona.ontology.trigger.Trigger.{ActivityStartTrigger, InitializeParticipantAgentTrigger}
import edu.ie3.simona.test.ParticipantAgentSpec
import edu.ie3.simona.test.common.model.participant.LoadTestData
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.scala.quantities.{Megavars, ReactivePower, Vars}
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.testkit.TestFSMRef
import org.apache.pekko.util.Timeout
import org.scalatest.PrivateMethodTester
import squants.Each
import squants.energy.{Kilowatts, Megawatts, Watts}

import java.util.concurrent.TimeUnit
import scala.collection.SortedMap

class LoadAgentProfileModelCalculationSpec
    extends ParticipantAgentSpec(
      ActorSystem(
        "LoadAgentSpec",
        ConfigFactory
          .parseString("""
            |pekko.loggers =["org.apache.pekko.event.slf4j.Slf4jLogger"]
            |pekko.loglevel="DEBUG"
        """.stripMargin),
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

  private val simonaConfig: SimonaConfig =
    createSimonaConfig(
      LoadModelBehaviour.PROFILE,
      LoadReference.ActivePower(Kilowatts(0d)),
    )
  private val defaultOutputConfig = NotifierConfig(
    simonaConfig.output.participant.defaultConfig.simulationResult,
    simonaConfig.output.participant.defaultConfig.powerRequestReply,
    simonaConfig.simona.output.participant.defaultConfig.flexResult,
  )
  private val loadConfigUtil = ConfigUtil.ParticipantConfigUtil(
    simonaConfig.runtime.participant
  )
  private val modelConfig =
    loadConfigUtil.getOrDefault[LoadRuntimeConfig](
      voltageSensitiveInput.getUuid
    )
  private val services = Iterable.empty
  private val resolution = simonaConfig.powerflow.resolution.toSeconds

  private implicit val powerTolerance: squants.Power = Watts(0.1)
  private implicit val reactivePowerTolerance: ReactivePower = Vars(0.1)

  "A load agent with profile model calculation depending on no secondary data service" should {
    val initStateData = ParticipantInitializeStateData[
      LoadInput,
      LoadRuntimeConfig,
      ApparentPower,
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
      val loadAgent = TestFSMRef(
        new ProfileLoadAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = systemListener,
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
        new ProfileLoadAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      scheduler.send(loadAgent, Activation(INIT_SIM_TICK))

      /* Actor should ask for registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]

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
              outputConfig,
              maybeEmAgent,
            ) =>
          inputModel shouldBe SimpleInputContainer(voltageSensitiveInput)
          modelConfig shouldBe modelConfig
          secondaryDataServices shouldBe services
          simulationStartDate shouldBe this.simulationStartDate
          simulationEndDate shouldBe this.simulationEndDate
          resolution shouldBe this.resolution
          requestVoltageDeviationThreshold shouldBe simonaConfig.runtime.participant.requestVoltageDeviationThreshold
          outputConfig shouldBe defaultOutputConfig
          maybeEmAgent shouldBe None
        case unsuitableStateData =>
          fail(s"Agent has unsuitable state data '$unsuitableStateData'.")
      }

      /* Refuse registration */
      primaryServiceProxy.send(
        loadAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* Expect a completion notification */
      scheduler.expectMsg(Completion(loadAgent.toTyped, Some(0)))

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
              _,
              _,
              _,
            ) =>
          /* Base state data */
          startDate shouldBe simulationStartDate
          endDate shouldBe simulationEndDate
          services shouldBe Iterable.empty
          outputConfig shouldBe defaultOutputConfig
          additionalActivationTicks
            .corresponds(Seq(900L, 1800L, 2700L, 3600L))(_ == _) shouldBe true
          foreseenDataTicks shouldBe Map.empty
          voltageValueStore shouldBe ValueStore(
            resolution,
            SortedMap(0L -> Each(1.0)),
          )
          resultValueStore shouldBe ValueStore(resolution)
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
        new ProfileLoadAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      scheduler.send(loadAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        loadAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]

      loadAgent.stateName shouldBe Idle
      /* State data has already been tested */

      loadAgent ! RequestAssetPowerMessage(
        0L,
        Each(1d),
        Each(0d),
      )
      expectMsg(
        AssetPowerChangedMessage(
          Megawatts(0d),
          Megavars(0d),
        )
      )

      inside(loadAgent.stateData) {
        case baseStateData: ParticipantModelBaseStateData[_, _, _, _] =>
          baseStateData.requestValueStore shouldBe ValueStore[
            ApparentPower
          ](
            resolution,
            SortedMap(
              0L -> ApparentPower(
                Megawatts(0d),
                Megavars(0d),
              )
            ),
          )
        case _ =>
          fail(
            s"Did not find expected state data $ParticipantModelBaseStateData, but ${loadAgent.stateData}"
          )
      }
    }

    "do correct transitions when triggered in Idle" in {
      val loadAgent = TestFSMRef(
        new ProfileLoadAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = systemListener,
        )
      )

      scheduler.send(loadAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        loadAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      /* I am not interested in the Completion */
      scheduler.expectMsgType[Completion]
      awaitAssert(loadAgent.stateName shouldBe Idle)
      /* State data is tested in another test */

      scheduler.send(loadAgent, Activation(0))

      /* Expect confirmation */
      scheduler.expectMsg(Completion(loadAgent.toTyped, Some(900)))

      /* Intermediate transitions and states cannot be tested, as the agents triggers itself
       * too fast */

      awaitAssert(loadAgent.stateName shouldBe Idle)
      inside(loadAgent.stateData) {
        case baseStateData: ParticipantModelBaseStateData[_, _, _, _] =>
          baseStateData.resultValueStore.last(0L) match {
            case Some((tick, entry)) =>
              tick shouldBe 0L
              inside(entry) { case ApparentPower(p, q) =>
                p should approximate(Megawatts(84.000938e-6))
                q should approximate(Megavars(0.0))
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

    val loadAgent = TestFSMRef(
      new ProfileLoadAgent(
        scheduler = scheduler.ref,
        initStateData = initStateData,
        listener = systemListener,
      )
    )

    "provide the correct average power after three data ticks are available" in {
      /* Trigger the initialisation */
      scheduler.send(loadAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        loadAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref),
      )

      scheduler.expectMsg(Completion(loadAgent.toTyped, Some(0)))

      /* Trigger the data generation in tick 0, 900, 1800 */
      scheduler.send(loadAgent, Activation(0))
      scheduler.expectMsg(Completion(loadAgent.toTyped, Some(900)))

      scheduler.send(loadAgent, Activation(900))
      scheduler.expectMsg(Completion(loadAgent.toTyped, Some(1800)))

      awaitAssert(loadAgent.stateName shouldBe Idle)

      /* Ask the agent for average power in tick 1800 */
      loadAgent ! RequestAssetPowerMessage(
        1800L,
        Each(1d),
        Each(0d),
      )

      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should approximate(Megawatts(79.750890e-6))
          q should approximate(Megavars(0.0))
      }
    }

    "answer unchanged power values after asking a second time with considerably same voltage" in {
      /* Previous request stems from previous test */
      /* Ask again with unchanged information */
      loadAgent ! RequestAssetPowerMessage(
        1800L,
        Each(1.000000000000001d),
        Each(0d),
      )

      /* Expect, that nothing has changed */
      expectMsgType[AssetPowerUnchangedMessage] match {
        case AssetPowerUnchangedMessage(p, q) =>
          p should approximate(Megawatts(79.750890e-6))
          q should approximate(Megavars(0.0))
      }
    }

    "answer changed power values after asking a second time with different voltage" in {
      /* Ask again with changed information */
      loadAgent ! RequestAssetPowerMessage(
        1800L,
        Each(0.98),
        Each(0),
      )

      /* Expect, the correct values (this model has fixed power factor) */
      expectMsgType[AssetPowerChangedMessage] match {
        case AssetPowerChangedMessage(p, q) =>
          p should approximate(Megawatts(79.750890e-6))
          q should approximate(Megavars(-22.0714e-6))
      }
    }
  }
}
