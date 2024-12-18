/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.input.system.StorageInput
import edu.ie3.datamodel.models.input.system.characteristic.QV
import edu.ie3.datamodel.models.result.system.StorageResult
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.grid.GridAgentMessages.AssetPowerChangedMessage
import edu.ie3.simona.agent.participant.ParticipantAgent.RequestAssetPowerMessage
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPower
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.{
  ParticipantInitializeStateData,
  ParticipantInitializingStateData,
  SimpleInputContainer,
}
import edu.ie3.simona.agent.participant.storage.StorageAgent
import edu.ie3.simona.agent.state.AgentState.Idle
import edu.ie3.simona.agent.state.ParticipantAgentState.HandleInformation
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.StorageRuntimeConfig
import edu.ie3.simona.event.ResultEvent.{
  FlexOptionsResultEvent,
  ParticipantResultEvent,
}
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.model.participant.load.{LoadModelBehaviour, LoadReference}
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage._
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.ontology.messages.services.PrimaryDataMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessageUniversal.RegistrationResponseMessage.RegistrationFailedMessage
import edu.ie3.simona.test.ParticipantAgentSpec
import edu.ie3.simona.test.common.input.StorageInputTestData
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.{Megavars, ReactivePower, Vars}
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.actor.{ActorRef, ActorSystem}
import org.apache.pekko.testkit.{TestFSMRef, TestProbe}
import squants.energy.{Kilowatts, Megawatts, Watts}
import squants.{Each, Power}

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
    """.stripMargin),
      )
    )
    with StorageInputTestData {

  protected implicit val simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z")
  protected val simulationEndDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T01:00:00Z")

  /* Alter the input model to have a voltage sensitive reactive power calculation */
  private val storageInputQv = storageInput
    .copy()
    .qCharacteristics(new QV("qV:{(0.95,-0.625),(1.05,0.625)}"))
    .build()

  /* Assign this test to receive the result events from agent */
  override val systemListener: Iterable[ActorRef] = Vector(self)

  private val simonaConfig: SimonaConfig =
    createSimonaConfig(
      LoadModelBehaviour.FIX,
      LoadReference.ActivePower(Kilowatts(0d)),
    )
  private val outputConfig = NotifierConfig(
    simulationResultInfo = true,
    powerRequestReply = false,
    flexResult = true,
  )
  private val configUtil = ConfigUtil.ParticipantConfigUtil(
    simonaConfig.simona.runtime.participant
  )
  private val modelConfig = configUtil.getOrDefault[StorageRuntimeConfig](
    storageInputQv.getUuid
  )
  private val services = Iterable.empty
  private val resolution = simonaConfig.simona.powerflow.resolution.getSeconds

  private implicit val powerTolerance: Power = Watts(0.1)
  private implicit val reactivePowerTolerance: ReactivePower = Vars(0.1)

  "A storage agent with model calculation depending on no secondary data service" should {
    val emAgent = TestProbe("EmAgent")

    val initStateData = ParticipantInitializeStateData[
      StorageInput,
      StorageRuntimeConfig,
      ComplexPower,
    ](
      inputModel = storageInputQv,
      modelConfig = modelConfig,
      secondaryDataServices = services,
      simulationStartDate = simulationStartDate,
      simulationEndDate = simulationEndDate,
      resolution = resolution,
      requestVoltageDeviationThreshold =
        simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
      outputConfig = outputConfig,
      primaryServiceProxy = primaryServiceProxy.ref.toTyped,
      maybeEmAgent = Some(emAgent.ref.toTyped),
    )

    "end in correct state with correct state data after initialisation" in {
      val storageAgent = TestFSMRef(
        new StorageAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = Iterable.empty,
        )
      )

      scheduler.send(storageAgent, Activation(INIT_SIM_TICK))

      /* Actor should ask for registration with primary service */
      primaryServiceProxy.expectMsg(
        PrimaryServiceRegistrationMessage(
          storageAgent.ref,
          storageInputQv.getUuid,
        )
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
            ) =>
          inputModel shouldBe SimpleInputContainer(storageInputQv)
          modelConfig shouldBe modelConfig
          secondaryDataServices shouldBe services
          simulationStartDate shouldBe simulationStartDate
          simulationEndDate shouldBe simulationEndDate
          resolution shouldBe resolution
          requestVoltageDeviationThreshold shouldBe simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold
          outputConfig shouldBe outputConfig
          maybeEmAgent shouldBe Some(emAgent.ref.toTyped)
        case unsuitableStateData =>
          fail(s"Agent has unsuitable state data '$unsuitableStateData'.")
      }

      /* Refuse registration */
      primaryServiceProxy.send(
        storageAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref.toTyped),
      )

      emAgent.expectMsg(
        RegisterParticipant(
          storageInputQv.getUuid,
          storageAgent.toTyped,
          storageInputQv,
        )
      )
      emAgent.expectMsg(
        ScheduleFlexRequest(storageInputQv.getUuid, 0)
      )

      scheduler.expectMsg(Completion(storageAgent.toTyped))

      /* ... as well as corresponding state and state data */
      storageAgent.stateName shouldBe Idle
      storageAgent.stateData match {
        case ParticipantModelBaseStateData(
              startDate,
              endDate,
              _,
              secondaryDataServices,
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
          secondaryDataServices shouldBe services
          outputConfig shouldBe outputConfig
          additionalActivationTicks shouldBe empty
          foreseenDataTicks shouldBe Map.empty
          voltageValueStore shouldBe ValueStore(
            resolution,
            SortedMap(0L -> Each(1.0)),
          )
          resultValueStore shouldBe ValueStore(
            resolution
          )
          requestValueStore shouldBe ValueStore[ComplexPower](
            resolution
          )
        case unrecognized =>
          fail(
            s"Did not find expected state data $ParticipantModelBaseStateData, but $unrecognized"
          )
      }
    }

    "answer with zero power, if asked directly after initialisation" in {
      val storageAgent = TestFSMRef(
        new StorageAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = Iterable.empty,
        )
      )

      scheduler.send(storageAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        storageAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref.toTyped),
      )

      emAgent.expectMsgType[RegisterParticipant]
      emAgent.expectMsg(ScheduleFlexRequest(storageInputQv.getUuid, 0))

      /* I'm not interested in the content of the Completion */
      scheduler.expectMsgType[Completion]

      storageAgent.stateName shouldBe Idle
      /* State data has already been tested */

      storageAgent ! RequestAssetPowerMessage(
        0,
        Each(1d),
        Each(0d),
      )
      expectMsg(
        AssetPowerChangedMessage(
          Megawatts(0d),
          Megavars(0d),
        )
      )

      inside(storageAgent.stateData) {
        case modelBaseStateData: ParticipantModelBaseStateData[_, _, _, _] =>
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
            s"Did not find expected state data $ParticipantModelBaseStateData, but ${storageAgent.stateData}"
          )
      }
    }

    "provide correct flex options when in Idle" in {
      val resultListener = TestProbe("ResultListener")

      val storageAgent = TestFSMRef(
        new StorageAgent(
          scheduler = scheduler.ref,
          initStateData = initStateData,
          listener = Iterable(resultListener.ref),
        )
      )

      scheduler.send(storageAgent, Activation(INIT_SIM_TICK))

      /* Refuse registration with primary service */
      primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
      primaryServiceProxy.send(
        storageAgent,
        RegistrationFailedMessage(primaryServiceProxy.ref.toTyped),
      )

      emAgent.expectMsgType[RegisterParticipant]
      emAgent.expectMsg(ScheduleFlexRequest(storageInputQv.getUuid, 0))

      /* I am not interested in the Completion */
      scheduler.expectMsgType[Completion]
      awaitAssert(storageAgent.stateName shouldBe Idle)
      /* State data is tested in another test */

      val pMax = Kilowatts(
        storageInputQv.getType.getpMax
          .to(PowerSystemUnits.KILOWATT)
          .getValue
          .doubleValue
      )

      /* TICK 0 (expected activation)
         - charging with pMax (12.961 kW)
         - expecting changing flex options indicator (charging from empty)
       */

      emAgent.send(storageAgent, FlexActivation(0))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              refPower,
              minPower,
              maxPower,
            ) =>
          modelUuid shouldBe storageInputQv.getUuid
          refPower should approximate(Kilowatts(0.0))
          minPower should approximate(Kilowatts(0.0))
          maxPower should approximate(pMax)
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe storageInputQv.getUuid
        flexResult.getTime shouldBe 0.toDateTime(simulationStartDate)
        flexResult.getpRef should beEquivalentTo(0d.asKiloWatt)
        flexResult.getpMin should beEquivalentTo(0d.asKiloWatt)
        flexResult.getpMax should beEquivalentTo(storageInputQv.getType.getpMax)
      }

      emAgent.send(
        storageAgent,
        IssuePowerControl(
          0,
          Kilowatts(storageInputQv.getType.getpMax().getValue.doubleValue()),
        ),
      )

      // next potential activation at fully charged battery:
      // net power = 12.961kW * 0.92 = 11.92412kW
      // time to charge fully ~= 16.7727262054h = 60382 ticks (rounded)
      emAgent.expectMsgPF() { case FlexResult(modelUuid, result) =>
        modelUuid shouldBe storageInputQv.getUuid
        result.p should approximate(pMax)
        result.q should approximate(Megavars(0))
      }
      emAgent.expectMsg(
        FlexCompletion(
          modelUuid = storageInputQv.getUuid,
          requestAtNextActivation = true,
          requestAtTick = Some(60382),
        )
      )

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: StorageResult) =>
          result.getInputModel shouldBe storageInputQv.getUuid
          result.getTime shouldBe 0.toDateTime(simulationStartDate)
          result.getP should beEquivalentTo(storageInputQv.getType.getpMax)
          result.getQ should beEquivalentTo(0d.asMegaVar)
          result.getSoc should beEquivalentTo(0d.asPercent)
      }

      /* TICK 28800 (unplanned activation)
         - charging with 9 kW
         - expecting trigger revoke
       */

      // Re-request flex options, since we've been asked to
      emAgent.send(storageAgent, FlexActivation(28800))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              refPower,
              minPower,
              maxPower,
            ) =>
          modelUuid shouldBe storageInputQv.getUuid
          refPower should approximate(Kilowatts(0.0))
          minPower should approximate(pMax * -1)
          maxPower should approximate(pMax)
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe storageInputQv.getUuid
        flexResult.getTime shouldBe 28800.toDateTime(simulationStartDate)
        flexResult.getpRef should beEquivalentTo(0d.asKiloWatt)
        flexResult.getpMin should beEquivalentTo(
          storageInputQv.getType.getpMax().multiply(-1)
        )
        flexResult.getpMax should beEquivalentTo(storageInputQv.getType.getpMax)
      }

      emAgent.send(storageAgent, IssuePowerControl(28800, Kilowatts(9)))

      // after 8 hours, we're at about half full storage: 95.39296 kWh
      // net power = 9kW * 0.92 = 8.28kW
      // time to charge fully ~= 12.6337004831h = 45481 ticks (rounded) from now
      // current tick is 28800, thus: 28800 + 45481 = 74281
      emAgent.expectMsgPF() { case FlexResult(modelUuid, result) =>
        modelUuid shouldBe storageInputQv.getUuid
        result.p should approximate(Kilowatts(9))
        result.q should approximate(Megavars(0))
      }
      emAgent.expectMsg(
        FlexCompletion(
          modelUuid = storageInputQv.getUuid,
          requestAtTick = Some(74281),
        )
      )

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: StorageResult) =>
          result.getInputModel shouldBe storageInputQv.getUuid
          result.getTime shouldBe 28800.toDateTime(simulationStartDate)
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
        IssuePowerControl(
          36000,
          Kilowatts(
            storageInputQv.getType.getpMax().multiply(-1).getValue.doubleValue
          ),
        ),
      )

      // after 2 hours, we're at: 111.95296 kWh
      // net power = -12.961kW / 0.92 = -14.08804348kW
      // time to discharge until lowest energy (0 kWh) ~= 7.946664856h = 28608 ticks (rounded) from now
      // current tick is 36000, thus: 36000 + 28608 = 64608
      emAgent.expectMsgPF() { case FlexResult(modelUuid, result) =>
        modelUuid shouldBe storageInputQv.getUuid
        result.p should approximate(pMax * -1)
        result.q should approximate(Megavars(0))
      }
      emAgent.expectMsg(
        FlexCompletion(
          modelUuid = storageInputQv.getUuid,
          requestAtTick = Some(64608),
        )
      )

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: StorageResult) =>
          result.getInputModel shouldBe storageInputQv.getUuid
          result.getTime shouldBe 36000.toDateTime(simulationStartDate)
          result.getP should beEquivalentTo(
            storageInputQv.getType.getpMax().multiply(-1)
          )
          result.getQ should beEquivalentTo(0d.asMegaVar)
          result.getSoc should beEquivalentTo(55.97648d.asPercent)
      }

      /* TICK 43200 (unplanned activation)
         - charging with 12 kW
         - expecting trigger revoke
       */

      emAgent.send(storageAgent, IssuePowerControl(43200, Kilowatts(12)))

      // after 2 hours, we're at: 83.77687304 kWh
      // net power = 12 * 0.92 = 11.04 kW
      // time to charge until full ~= 10.52745715h = 37899 ticks (rounded) from now
      // current tick is 43200, thus: 43200 + 37899 = 81099
      emAgent.expectMsgPF() { case FlexResult(modelUuid, result) =>
        modelUuid shouldBe storageInputQv.getUuid
        result.p should approximate(Kilowatts(12))
        result.q should approximate(Megavars(0))
      }
      emAgent.expectMsg(
        FlexCompletion(
          modelUuid = storageInputQv.getUuid,
          requestAtTick = Some(81099),
        )
      )

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: StorageResult) =>
          result.getInputModel shouldBe storageInputQv.getUuid
          result.getTime shouldBe 43200.toDateTime(simulationStartDate)
          result.getP should beEquivalentTo(12d.asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
          result.getSoc should beEquivalentTo(41.88843652173913d.asPercent)
      }

      /* TICK 81099 (expected activation)
         - discharging with 12 kW
         - expecting changing flex options indicator (discharging from full)
       */

      // Request flex options
      emAgent.send(storageAgent, FlexActivation(81099))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              refPower,
              minPower,
              maxPower,
            ) =>
          modelUuid shouldBe storageInputQv.getUuid
          refPower should approximate(Kilowatts(0.0))
          minPower should approximate(pMax * -1)
          maxPower should approximate(Kilowatts(0.0))
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe storageInputQv.getUuid
        flexResult.getTime shouldBe 81099.toDateTime(simulationStartDate)
        flexResult.getpRef should beEquivalentTo(0d.asKiloWatt)
        flexResult.getpMin should beEquivalentTo(
          storageInputQv.getType.getpMax().multiply(-1)
        )
        flexResult.getpMax should beEquivalentTo(0d.asKiloWatt)
      }

      emAgent.send(storageAgent, IssuePowerControl(81099, Kilowatts(-12)))

      // we're full now at 200 kWh
      // net power = -12 / 0.92 = -13.04347826 kW
      // time to discharge until empty ~= 15.33333333h = 55200 ticks from now
      // current tick is 79688, thus: 81099 + 55200 = 136299
      emAgent.expectMsgPF() { case FlexResult(modelUuid, result) =>
        modelUuid shouldBe storageInputQv.getUuid
        result.p should approximate(Kilowatts(-12))
        result.q should approximate(Megavars(0))
      }
      emAgent.expectMsg(
        FlexCompletion(
          modelUuid = storageInputQv.getUuid,
          requestAtNextActivation = true,
          requestAtTick = Some(136299),
        )
      )

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: StorageResult) =>
          result.getInputModel shouldBe storageInputQv.getUuid
          result.getTime shouldBe 81099.toDateTime(simulationStartDate)
          result.getP should beEquivalentTo((-12d).asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
          result.getSoc should beEquivalentTo(100d.asPercent)
      }

      /* TICK 136299 (expected activation)
         - no charging
         - expecting no changing flex options indicator
       */

      // Request flex options
      emAgent.send(storageAgent, FlexActivation(136299))

      emAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              refPower,
              minPower,
              maxPower,
            ) =>
          modelUuid shouldBe storageInputQv.getUuid
          refPower should approximate(Kilowatts(0.0))
          minPower should approximate(Kilowatts(0.0))
          maxPower should approximate(pMax)
      }

      resultListener.expectMsgPF() { case FlexOptionsResultEvent(flexResult) =>
        flexResult.getInputModel shouldBe storageInputQv.getUuid
        flexResult.getTime shouldBe 136299.toDateTime(simulationStartDate)
        flexResult.getpRef should beEquivalentTo(0d.asKiloWatt)
        flexResult.getpMin should beEquivalentTo(0d.asKiloWatt)
        flexResult.getpMax should beEquivalentTo(storageInputQv.getType.getpMax)
      }

      emAgent.send(storageAgent, IssuePowerControl(136299, Kilowatts(0d)))

      // we're not charging or discharging, no new expected tick
      emAgent.expectMsgPF() { case FlexResult(modelUuid, result) =>
        modelUuid shouldBe storageInputQv.getUuid
        result.p should approximate(Kilowatts(0))
        result.q should approximate(Megavars(0))
      }
      emAgent.expectMsg(
        FlexCompletion(
          modelUuid = storageInputQv.getUuid
        )
      )

      resultListener.expectMsgPF() {
        case ParticipantResultEvent(result: StorageResult) =>
          result.getInputModel shouldBe storageInputQv.getUuid
          result.getTime shouldBe 136299.toDateTime(simulationStartDate)
          result.getP should beEquivalentTo(0.asKiloWatt)
          result.getQ should beEquivalentTo(0d.asMegaVar)
          result.getSoc should beEquivalentTo(0d.asPercent)
      }

    }

  }
}
