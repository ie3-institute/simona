/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestFSMRef, TestProbe}
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.input.system.StorageInput
import edu.ie3.datamodel.models.input.system.characteristic.QV
import edu.ie3.datamodel.models.result.system.StorageResult
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.{
  ParticipantInitializeStateData,
  ParticipantInitializingStateData,
  SimpleInputContainer
}
import edu.ie3.simona.agent.participant.storage.StorageAgent
import edu.ie3.simona.agent.state.AgentState.Idle
import edu.ie3.simona.agent.state.ParticipantAgentState.HandleInformation
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.StorageRuntimeConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.event.ResultEvent.{
  FlexOptionsResultEvent,
  ParticipantResultEvent
}
import edu.ie3.simona.model.participant.load.{LoadModelBehaviour, LoadReference}
import edu.ie3.simona.ontology.messages.FlexibilityMessage.{
  FlexCtrlCompletion,
  IssuePowerCtrl,
  ProvideFlexOptions,
  ProvideMinMaxFlexOptions,
  RequestFlexOptions
}
import edu.ie3.simona.ontology.messages.PowerMessage.{
  AssetPowerChangedMessage,
  RequestAssetPowerMessage
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationFailedMessage
import edu.ie3.simona.ontology.trigger.Trigger.InitializeParticipantAgentTrigger
import edu.ie3.simona.test.ParticipantAgentSpec
import edu.ie3.simona.test.common.input.StorageInputTestData
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.Megavars
import squants.Each
import squants.energy.{Kilowatts, Megawatts, Watts}

import java.time.ZonedDateTime
import scala.collection.SortedMap

class StorageAgentModelCalculationSpec
    extends ParticipantAgentSpec(
      ActorSystem(
        "StorageAgentModelCalculationSpec",
        ConfigFactory
          .parseString("""
          |akka.loggers =["akka.event.slf4j.Slf4jLogger"]
          |akka.loglevel="DEBUG"
    """.stripMargin)
      )
    )
    with StorageInputTestData {

  protected implicit val simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01 00:00:00")
  protected val simulationEndDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01 01:00:00")

  /* Alter the input model to have a voltage sensitive reactive power calculation */
  private val voltageSensitiveInput = storageInput
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
  private val outputConfig = NotifierConfig(
    simulationResultInfo = true,
    powerRequestReply = false,
    flexResult = true
  )
  private val configUtil = ConfigUtil.ParticipantConfigUtil(
    simonaConfig.simona.runtime.participant
  )
  private val modelConfig = configUtil.getOrDefault[StorageRuntimeConfig](
    voltageSensitiveInput.getUuid
  )
  private val services = None
  private val resolution = simonaConfig.simona.powerflow.resolution.getSeconds

  private implicit val powerTolerance: squants.Power = Watts(0.1)

  "A storage agent with model calculation depending on no secondary data service" should {

    "end in correct state with correct state data after initialisation" in {
      val emAgent = TestProbe("EmAgentProbe")

      val storageAgent = TestFSMRef(
        new StorageAgent(
          scheduler = emAgent.ref,
          listener = Iterable.empty
        )
      )

      val triggerId = 0
      emAgent.send(
        storageAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              StorageInput,
              StorageRuntimeConfig,
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
              outputConfig = outputConfig,
              primaryServiceProxy = primaryServiceProxy.ref,
              maybeEmAgent = Some(emAgent.ref),
              scheduleTriggerFunc =
                scheduleTriggerEmFunc(storageAgent, emAgent.ref)
            )
          ),
          triggerId,
          storageAgent
        )
      )

      /* Actor should ask for registration with primary service */
      primaryServiceProxy.expectMsg(
        PrimaryServiceRegistrationMessage(voltageSensitiveInput.getUuid)
      )
      /* State should be information handling and having correct state data */
      storageAgent.stateName shouldBe HandleInformation
      storageAgent.stateData match {
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
              _
            ) =>
          inputModel shouldBe SimpleInputContainer(voltageSensitiveInput)
          modelConfig shouldBe modelConfig
          secondaryDataServices shouldBe services
          simulationStartDate shouldBe simulationStartDate
          simulationEndDate shouldBe simulationEndDate
          resolution shouldBe resolution
          requestVoltageDeviationThreshold shouldBe simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold
          outputConfig shouldBe outputConfig
          maybeEmAgent shouldBe Some(emAgent.ref)
        case unsuitableStateData =>
          fail(s"Agent has unsuitable state data '$unsuitableStateData'.")
      }

      /* Refuse registration */
      primaryServiceProxy.send(storageAgent, RegistrationFailedMessage)

      emAgent.expectMsg(
        ScheduleTriggerMessage(
          RequestFlexOptions(0L),
          storageAgent
        )
      )

      emAgent.expectMsg(
        CompletionMessage(
          triggerId,
          None
        )
      )

      /* ... as well as corresponding state and state data */
      storageAgent.stateName shouldBe Idle
      storageAgent.stateData match {
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
          outputConfig shouldBe outputConfig
          additionalActivationTicks shouldBe empty
          foreseenDataTicks shouldBe Map.empty
          voltageValueStore shouldBe ValueStore(
            resolution,
            SortedMap(0L -> Each(1.0))
          )
          resultValueStore shouldBe ValueStore(
            resolution
          )
          requestValueStore shouldBe ValueStore[ApparentPower](
            resolution
          )
        case unrecognized =>
          fail(
            s"Did not find expected state data $ParticipantModelBaseStateData, but $unrecognized"
          )
      }
    }

    "answer with zero power, if asked directly after initialisation" in {
      val emAgent = TestProbe("EmAgentProbe")

      val storageAgent = TestFSMRef(
        new StorageAgent(
          scheduler = emAgent.ref,
          listener = Iterable.empty
        )
      )

      val triggerId = 0
      emAgent.send(
        storageAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              StorageInput,
              StorageRuntimeConfig,
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
              outputConfig = outputConfig,
              primaryServiceProxy = primaryServiceProxy.ref,
              maybeEmAgent = Some(emAgent.ref),
              scheduleTriggerFunc =
                scheduleTriggerEmFunc(storageAgent, emAgent.ref)
            )
          ),
          triggerId,
          storageAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(storageAgent, RegistrationFailedMessage)

      emAgent.expectMsg(
        ScheduleTriggerMessage(
          RequestFlexOptions(0L),
          storageAgent
        )
      )

      /* I'm not interested in the content of the CompletionMessage */
      emAgent.expectMsgType[CompletionMessage]

      storageAgent.stateName shouldBe Idle
      /* State data has already been tested */

      storageAgent ! RequestAssetPowerMessage(
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

      inside(storageAgent.stateData) {
        case modelBaseStateData: ParticipantModelBaseStateData[_, _, _, _] =>
          modelBaseStateData.requestValueStore shouldBe ValueStore[
            ApparentPower
          ](
            resolution,
            SortedMap(
              0L -> ApparentPower(
                Megawatts(0d),
                Megavars(0d)
              )
            )
          )
        case _ =>
          fail(
            s"Did not find expected state data $ParticipantModelBaseStateData, but ${storageAgent.stateData}"
          )
      }
    }

    "provide correct flex options when in Idle" in {
      val emAgent = TestProbe("EmAgentProbe")
      val resultListener = TestProbe("ResultListener")

      val storageAgent = TestFSMRef(
        new StorageAgent(
          scheduler = emAgent.ref,
          listener = Iterable(resultListener.ref)
        )
      )

      val triggerId = 0
      emAgent.send(
        storageAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            ParticipantInitializeStateData[
              StorageInput,
              StorageRuntimeConfig,
              ApparentPower
            ]
          ](
            ParticipantInitializeStateData(
              inputModel = SimpleInputContainer(voltageSensitiveInput),
              modelConfig = modelConfig,
              secondaryDataServices = services,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = outputConfig,
              primaryServiceProxy = primaryServiceProxy.ref,
              maybeEmAgent = Some(emAgent.ref),
              scheduleTriggerFunc =
                scheduleTriggerEmFunc(storageAgent, emAgent.ref)
            )
          ),
          triggerId,
          storageAgent
        )
      )

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(storageAgent, RegistrationFailedMessage)

      emAgent.expectMsg(
        ScheduleTriggerMessage(
          RequestFlexOptions(0L),
          storageAgent
        )
      )

      /* I am not interested in the CompletionMessage */
      emAgent.expectMsgType[CompletionMessage]
      awaitAssert(storageAgent.stateName shouldBe Idle)
      /* State data is tested in another test */

      val pMax = Kilowatts(
        storageInput.getType.getpMax
          .to(PowerSystemUnits.KILOWATT)
          .getValue
          .doubleValue
      )

      /* TICK 0 (expected activation)
         - charging with pMax (12.961 kW)
         - expecting changing flex options indicator (charging from empty)
       */

      emAgent.send(storageAgent, RequestFlexOptions(0L))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              refPower,
              minPower,
              maxPower
            ) =>
          modelUuid shouldBe storageInput.getUuid
          (refPower ~= Kilowatts(0.0)) shouldBe true
          (minPower ~= Kilowatts(0.0)) shouldBe true
          (maxPower ~= pMax) shouldBe true
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe storageInput.getUuid
        flexResult.getTime shouldBe 0L.toDateTime(simulationStartDate)
        flexResult.getpRef should beEquivalentTo(0d.asKiloWatt)
        flexResult.getpMin should beEquivalentTo(0d.asKiloWatt)
        flexResult.getpMax should beEquivalentTo(storageInput.getType.getpMax)
      }

      emAgent.send(
        storageAgent,
        IssuePowerCtrl(
          0L,
          Kilowatts(storageInput.getType.getpMax().getValue.doubleValue())
        )
      )

      emAgent.expectMsgPF() {
        case ParticipantResultEvent(result: StorageResult) =>
          result.getP should beEquivalentTo(storageInput.getType.getpMax())
          result.getQ should beEquivalentTo(0.asMegaVar)
      }

      // next potential activation at fully charged battery:
      // net power = 12.961kW * 0.92 = 11.92412kW
      // time to charge fully ~= 16.7727262054h = 60382 ticks (rounded)
      emAgent.expectMsg(
        FlexCtrlCompletion(
          modelUuid = storageInput.getUuid,
          requestAtNextActivation = true,
          requestAtTick = Some(60382L)
        )
      )

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: StorageResult) =>
          result.getInputModel shouldBe storageInput.getUuid
          result.getTime shouldBe 0L.toDateTime(simulationStartDate)
          result.getP should beEquivalentTo(storageInput.getType.getpMax)
          result.getQ should beEquivalentTo(0d.asMegaVar)
          result.getSoc should beEquivalentTo(0d.asPercent)
      }

      /* TICK 28800 (unplanned activation)
         - charging with 9 kW
         - expecting trigger revoke
       */

      // Re-request flex options, since we've been asked to
      emAgent.send(storageAgent, RequestFlexOptions(28800L))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              refPower,
              minPower,
              maxPower
            ) =>
          modelUuid shouldBe storageInput.getUuid
          (refPower ~= Kilowatts(0.0)) shouldBe true
          (minPower ~= pMax * -1) shouldBe true
          (maxPower ~= pMax) shouldBe true
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe storageInput.getUuid
        flexResult.getTime shouldBe 28800L.toDateTime(simulationStartDate)
        flexResult.getpRef should beEquivalentTo(0d.asKiloWatt)
        flexResult.getpMin should beEquivalentTo(
          storageInput.getType.getpMax().multiply(-1)
        )
        flexResult.getpMax should beEquivalentTo(storageInput.getType.getpMax)
      }

      emAgent.send(storageAgent, IssuePowerCtrl(28800L, Kilowatts(9)))

      emAgent.expectMsgPF() {
        case ParticipantResultEvent(result: StorageResult) =>
          result.getP should beEquivalentTo(9d.asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }

      // after 8 hours, we're at about half full storage: 95.39296 kWh
      // net power = 9kW * 0.92 = 8.28kW
      // time to charge fully ~= 12.6337004831h = 45481 ticks (rounded) from now
      // current tick is 28800, thus: 28800 + 45481 = 74281
      emAgent.expectMsg(
        FlexCtrlCompletion(
          modelUuid = storageInput.getUuid,
          revokeRequestAtTick = Some(60382L),
          requestAtTick = Some(74281L)
        )
      )

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: StorageResult) =>
          result.getInputModel shouldBe storageInput.getUuid
          result.getTime shouldBe 28800L.toDateTime(simulationStartDate)
          result.getP should beEquivalentTo(9d.asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
          result.getSoc should beEquivalentTo(47.69648d.asPercent)
      }

      /* TICK 36000 (unplanned activation)
         - discharging with pMax (-12.961 kW)
         - expecting trigger revoke
       */

      emAgent.send(
        storageAgent,
        IssuePowerCtrl(
          36000L,
          Kilowatts(
            storageInput.getType.getpMax().multiply(-1).getValue.doubleValue()
          )
        )
      )

      emAgent.expectMsgPF() {
        case ParticipantResultEvent(result: StorageResult) =>
          result.getP should beEquivalentTo(
            storageInput.getType.getpMax().multiply(-1)
          )
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }

      // after 2 hours, we're at: 111.95296 kWh
      // net power = -12.961kW * 0.92 = -11.92412kW
      // time to discharge until lowest energy (40 kWh) ~= 6.03423648873h = 21723 ticks (rounded) from now
      // current tick is 36000, thus: 36000 + 21723 = 57723
      emAgent.expectMsg(
        FlexCtrlCompletion(
          modelUuid = storageInput.getUuid,
          revokeRequestAtTick = Some(74281L),
          requestAtTick = Some(57723L)
        )
      )

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: StorageResult) =>
          result.getInputModel shouldBe storageInput.getUuid
          result.getTime shouldBe 36000L.toDateTime(simulationStartDate)
          result.getP should beEquivalentTo(
            storageInput.getType.getpMax().multiply(-1)
          )
          result.getQ should beEquivalentTo(0d.asMegaVar)
          result.getSoc should beEquivalentTo(55.97648d.asPercent)
      }

      /* TICK 43200 (unplanned activation)
         - charging with 12 kW
         - expecting trigger revoke
       */

      emAgent.send(storageAgent, IssuePowerCtrl(43200L, Kilowatts(12)))

      emAgent.expectMsgPF() {
        case ParticipantResultEvent(result: StorageResult) =>
          result.getP should beEquivalentTo(12d.asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }

      // after 2 hours, we're at: 88.10472 kWh
      // net power = 12 * 0.92 = 11.04 kW
      // time to charge until full ~= 10.135442029h = 36488 ticks (rounded) from now
      // current tick is 43200, thus: 43200 + 36488 = 79688
      emAgent.expectMsg(
        FlexCtrlCompletion(
          modelUuid = storageInput.getUuid,
          revokeRequestAtTick = Some(57723L),
          requestAtTick = Some(79688L)
        )
      )

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: StorageResult) =>
          result.getInputModel shouldBe storageInput.getUuid
          result.getTime shouldBe 43200L.toDateTime(simulationStartDate)
          result.getP should beEquivalentTo(12d.asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
          result.getSoc should beEquivalentTo(44.05236d.asPercent)
      }

      /* TICK 79688 (expected activation)
         - discharging with 12 kW
         - expecting changing flex options indicator (discharging from full)
       */

      // Request flex options
      emAgent.send(storageAgent, RequestFlexOptions(79688L))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              refPower,
              minPower,
              maxPower
            ) =>
          modelUuid shouldBe storageInput.getUuid
          (refPower ~= Kilowatts(0.0)) shouldBe true
          (minPower ~= pMax * -1) shouldBe true
          (maxPower ~= Kilowatts(0.0)) shouldBe true
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe storageInput.getUuid
        flexResult.getTime shouldBe 79688L.toDateTime(simulationStartDate)
        flexResult.getpRef should beEquivalentTo(0d.asKiloWatt)
        flexResult.getpMin should beEquivalentTo(
          storageInput.getType.getpMax().multiply(-1)
        )
        flexResult.getpMax should beEquivalentTo(0d.asKiloWatt)
      }

      emAgent.send(storageAgent, IssuePowerCtrl(79688L, Kilowatts(-12)))

      emAgent.expectMsgPF() {
        case ParticipantResultEvent(result: StorageResult) =>
          result.getP should beEquivalentTo((-12d).asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }

      // we're full now at 200 kWh
      // net power = -12 * 0.92 = -11.04 kW
      // time to discharge until lowest energy ~= 14.4927536232h = 52174 ticks (rounded) from now
      // current tick is 79688, thus: 79688 + 52174 = 131862
      emAgent.expectMsg(
        FlexCtrlCompletion(
          modelUuid = storageInput.getUuid,
          requestAtNextActivation = true,
          requestAtTick = Some(131862L)
        )
      )

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: StorageResult) =>
          result.getInputModel shouldBe storageInput.getUuid
          result.getTime shouldBe 79688L.toDateTime(simulationStartDate)
          result.getP should beEquivalentTo((-12d).asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
          result.getSoc should beEquivalentTo(100d.asPercent)
      }

      /* TICK 131862 (expected activation)
         - no charging
         - expecting no changing flex options indicator
       */

      // Request flex options
      emAgent.send(storageAgent, RequestFlexOptions(131862L))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              refPower,
              minPower,
              maxPower
            ) =>
          modelUuid shouldBe storageInput.getUuid
          (refPower ~= Kilowatts(0.0)) shouldBe true
          (minPower ~= Kilowatts(0.0)) shouldBe true
          (maxPower ~= pMax) shouldBe true
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe storageInput.getUuid
        flexResult.getTime shouldBe 131862L.toDateTime(simulationStartDate)
        flexResult.getpRef should beEquivalentTo(0d.asKiloWatt)
        flexResult.getpMin should beEquivalentTo(0d.asKiloWatt)
        flexResult.getpMax should beEquivalentTo(storageInput.getType.getpMax)
      }

      emAgent.send(storageAgent, IssuePowerCtrl(131862L, Kilowatts(0d)))

      emAgent.expectMsgPF() {
        case ParticipantResultEvent(result: StorageResult) =>
          result.getP should beEquivalentTo(0d.asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
      }

      // we're not charging or discharging, no new expected tick
      emAgent.expectMsg(
        FlexCtrlCompletion(
          modelUuid = storageInput.getUuid
        )
      )

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: StorageResult) =>
          result.getInputModel shouldBe storageInput.getUuid
          result.getTime shouldBe 131862L.toDateTime(simulationStartDate)
          result.getP should beEquivalentTo(0.asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
          result.getSoc should beEquivalentTo(19.999866666667d.asPercent)
      }

    }

  }
}
