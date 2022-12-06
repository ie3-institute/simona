/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.result.system.{EmResult, EvcsResult, PvResult}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.em.EmAgent
import edu.ie3.simona.agent.participant.em.EmAgent.EmAgentInitializeStateData
import edu.ie3.simona.agent.participant.statedata.InitializeStateData
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.model.participant.em.PrioritizedFlexStrat
import edu.ie3.simona.ontology.messages.FlexibilityMessage._
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeParticipantAgentTrigger
}
import edu.ie3.simona.test.ParticipantAgentSpec
import edu.ie3.simona.test.common.input.EmInputTestData
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.scalatestplus.mockito.MockitoSugar

import java.time.ZonedDateTime

class EmAgentSpec
    extends ParticipantAgentSpec(
      ActorSystem(
        "EmAgentSelfOptSpec",
        ConfigFactory
          .parseString("""
        |akka.loggers =["akka.event.slf4j.Slf4jLogger"]
        |akka.loglevel="DEBUG"
  """.stripMargin)
      )
    )
    with EmInputTestData
    with MockitoSugar {

  protected implicit val simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01 00:00:00")
  protected val simulationEndDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-02 02:00:00")

  private val resolution =
    simonaConfig.simona.powerflow.resolution.getSeconds // TODO does this make sense?

  private val outputConfig = NotifierConfig(
    simulationResultInfo = true,
    powerRequestReply = false
  )

  private val tolerance = 1e-10d

  "A self-optimizing EM agent" should {
    "be initialized correctly and run through some activations" in {
      val resultsProbe = TestProbe("ResultListener")

      val emAgent = TestActorRef(
        new EmAgent(
          scheduler = scheduler.ref,
          listener = Iterable(resultsProbe.ref)
        )
      )

      val initId = 0

      val pvAgent = TestProbe("PvAgent")
      val evcsAgent = TestProbe("EvcsAgent")

      val pvAgentInit =
        InitializeParticipantAgentTrigger[ApparentPower, InitializeStateData[
          ApparentPower
        ]](mock[InitializeStateData[ApparentPower]])
      val evcsAgentInit =
        InitializeParticipantAgentTrigger[ApparentPower, InitializeStateData[
          ApparentPower
        ]](mock[InitializeStateData[ApparentPower]])

      val connectedAgents = Seq(
        (pvAgent.ref, pvAgentInit, pvInput),
        (evcsAgent.ref, evcsAgentInit, evcsInput)
      )

      scheduler.send(
        emAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            EmAgentInitializeStateData
          ](
            EmAgentInitializeStateData(
              inputModel = emInput,
              modelConfig = modelConfig,
              secondaryDataServices = None,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = outputConfig,
              modelStrategy = PrioritizedFlexStrat,
              primaryServiceProxy = primaryServiceProxy.ref,
              connectedAgents = connectedAgents
            )
          ),
          initId,
          emAgent
        )
      )

      val receivedPvInit = pvAgent.expectMsgType[TriggerWithIdMessage]
      receivedPvInit.trigger shouldBe pvAgentInit

      val receivedEvcsInit = evcsAgent.expectMsgType[TriggerWithIdMessage]
      receivedEvcsInit.trigger shouldBe evcsAgentInit

      pvAgent.send(
        emAgent,
        CompletionMessage(
          receivedPvInit.triggerId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(0L),
                pvAgent.ref
              )
            )
          )
        )
      )

      scheduler.expectNoMessage()

      evcsAgent.send(
        emAgent,
        CompletionMessage(
          receivedEvcsInit.triggerId,
          None
        )
      )

      scheduler.expectMsg(
        CompletionMessage(
          initId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(0L),
                emAgent
              )
            )
          )
        )
      )

      // we receive ev arrivals at the first tick
      emAgent ! ScheduleTriggerMessage(ActivityStartTrigger(0L), evcsAgent.ref)

      // no additional scheduling message, since tick 0 has already been scheduled
      scheduler.expectNoMessage()

      // init done, start EmAgent
      val activationId1 = 1L

      scheduler.send(
        emAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          activationId1,
          emAgent
        )
      )

      // expect activations and flex requests
      val receivedPvActivation1 =
        pvAgent.expectMsgType[TriggerWithIdMessage]
      receivedPvActivation1.trigger shouldBe ActivityStartTrigger(0L)
      receivedPvActivation1.receiverActor shouldBe pvAgent.ref

      pvAgent.expectMsg(RequestFlexOptions(0L))

      val receivedEvcsActivation1 =
        evcsAgent.expectMsgType[TriggerWithIdMessage]
      receivedEvcsActivation1.trigger shouldBe ActivityStartTrigger(0L)
      receivedEvcsActivation1.receiverActor shouldBe evcsAgent.ref

      evcsAgent.expectMsg(RequestFlexOptions(0L))

      // send flex options
      pvAgent.send(
        emAgent,
        ProvideMinMaxFlexOptions(
          pvInput.getUuid,
          (-5d).asKiloWatt,
          (-5d).asKiloWatt,
          0d.asKiloWatt
        )
      )

      pvAgent.expectNoMessage()
      evcsAgent.expectNoMessage()

      evcsAgent.send(
        emAgent,
        ProvideMinMaxFlexOptions(
          evcsInput.getUuid,
          2d.asKiloWatt,
          (-11d).asKiloWatt,
          11d.asKiloWatt
        )
      )

      // receive flex control messages
      pvAgent.expectMsg(IssueNoCtrl(0L))
      pvAgent.send(
        emAgent,
        ParticipantResultEvent(
          new PvResult(
            0L.toDateTime,
            pvInput.getUuid,
            (-5d).asKiloWatt,
            (-0.5d).asKiloVar
          )
        )
      )

      evcsAgent.expectMsgType[IssuePowerCtrl] match {
        case IssuePowerCtrl(0L, setPower) =>
          setPower should equalWithTolerance(5d.asKiloWatt, tolerance)
      }
      evcsAgent.send(
        emAgent,
        ParticipantResultEvent(
          new EvcsResult(
            0L.toDateTime,
            evcsInput.getUuid,
            5d.asKiloWatt,
            0.1d.asKiloVar
          )
        )
      )

      // expect correct results
      resultsProbe.expectMsgType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe simulationStartDate
          emResult.getP should equalWithTolerance(0d.asMegaWatt, tolerance)
          emResult.getQ should equalWithTolerance(
            (-.0004d).asMegaVar,
            tolerance
          )
        case unexpected =>
          fail(s"Received unexpected result $unexpected")
      }

      // send completions
      pvAgent.send(
        emAgent,
        FlexCtrlCompletion(
          pvInput.getUuid
        )
      )

      pvAgent.send(
        emAgent,
        CompletionMessage(
          receivedPvActivation1.triggerId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(600L),
                pvAgent.ref
              )
            )
          )
        )
      )

      evcsAgent.send(
        emAgent,
        FlexCtrlCompletion(
          evcsInput.getUuid,
          requestAtTick = Some(300L)
        )
      )

      scheduler.expectNoMessage()

      evcsAgent.send(
        emAgent,
        CompletionMessage(
          receivedEvcsActivation1.triggerId,
          None
        )
      )

      // expect completion from EmAgent
      scheduler.expectMsg(
        CompletionMessage(
          activationId1,
          Some(Seq(ScheduleTriggerMessage(ActivityStartTrigger(300L), emAgent)))
        )
      )

      // trigger EmAgent with next tick
      val activationId2 = 1L

      scheduler.send(
        emAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(300L),
          activationId2,
          emAgent
        )
      )

      // expect activations and flex requests.
      // only participant 2 has been scheduled for this tick,
      // thus 1 does not get activated
      pvAgent.expectNoMessage()

      evcsAgent.expectMsg(RequestFlexOptions(300L))

      // send flex options again, ev is fully charged
      evcsAgent.send(
        emAgent,
        ProvideMinMaxFlexOptions(
          evcsInput.getUuid,
          0d.asKiloWatt,
          (-11d).asKiloWatt,
          0d.asKiloWatt
        )
      )

      // receive flex control messages
      evcsAgent.expectMsg(IssueNoCtrl(300L))
      evcsAgent.send(
        emAgent,
        ParticipantResultEvent(
          new EvcsResult(
            300L.toDateTime,
            evcsInput.getUuid,
            0d.asKiloWatt,
            0d.asKiloVar
          )
        )
      )

      pvAgent.expectNoMessage()

      // expect correct results
      resultsProbe.expectMsgType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 300L.toDateTime(simulationStartDate)
          emResult.getP should equalWithTolerance(
            (-0.005d).asMegaWatt,
            tolerance
          )
          emResult.getQ should equalWithTolerance(
            (-0.0005d).asMegaVar,
            tolerance
          )
        case unexpected =>
          fail(s"Received unexpected result $unexpected")
      }

      // send completion
      scheduler.expectNoMessage()

      evcsAgent.send(
        emAgent,
        FlexCtrlCompletion(
          evcsInput.getUuid
        )
      )

      // expect completion from EmAgent
      scheduler.expectMsg(
        CompletionMessage(
          activationId2,
          Some(Seq(ScheduleTriggerMessage(ActivityStartTrigger(600L), emAgent)))
        )
      )

    }

    "revoke triggers correctly" in {
      val resultsProbe = TestProbe("ResultListener")

      val emAgent = TestActorRef(
        new EmAgent(
          scheduler = scheduler.ref,
          listener = Iterable(resultsProbe.ref)
        )
      )

      val initId = 0

      val pvAgent = TestProbe("PvAgent")
      val evcsAgent = TestProbe("EvcsAgent")

      val pvAgentInit =
        InitializeParticipantAgentTrigger[ApparentPower, InitializeStateData[
          ApparentPower
        ]](mock[InitializeStateData[ApparentPower]])
      val evcsAgentInit =
        InitializeParticipantAgentTrigger[ApparentPower, InitializeStateData[
          ApparentPower
        ]](mock[InitializeStateData[ApparentPower]])

      val connectedAgents = Seq(
        (
          pvAgent.ref,
          pvAgentInit,
          pvInput
        ),
        (
          evcsAgent.ref,
          evcsAgentInit,
          evcsInput
        )
      )

      scheduler.send(
        emAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            EmAgentInitializeStateData
          ](
            EmAgentInitializeStateData(
              inputModel = emInput,
              modelConfig = modelConfig,
              secondaryDataServices = None,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = outputConfig,
              modelStrategy = PrioritizedFlexStrat,
              primaryServiceProxy = primaryServiceProxy.ref,
              connectedAgents = connectedAgents
            )
          ),
          initId,
          emAgent
        )
      )

      val receivedPvInit = pvAgent
        .expectMsgType[TriggerWithIdMessage]
      receivedPvInit.trigger shouldBe pvAgentInit

      val receivedEvcsInit = evcsAgent
        .expectMsgType[TriggerWithIdMessage]
      receivedEvcsInit.trigger shouldBe evcsAgentInit

      pvAgent.send(
        emAgent,
        CompletionMessage(
          receivedPvInit.triggerId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(0L),
                pvAgent.ref
              )
            )
          )
        )
      )

      scheduler.expectNoMessage()

      evcsAgent.send(
        emAgent,
        CompletionMessage(
          receivedEvcsInit.triggerId,
          None
        )
      )

      scheduler.expectMsg(
        CompletionMessage(
          initId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(0L),
                emAgent
              )
            )
          )
        )
      )

      // we receive ev arrivals at the first tick
      emAgent ! ScheduleTriggerMessage(ActivityStartTrigger(0L), evcsAgent.ref)

      // init done, start EmAgent
      val activationId1 = 1L

      scheduler.send(
        emAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          activationId1,
          emAgent
        )
      )

      // expect activations and flex requests
      val receivedPvActivation1 =
        pvAgent.expectMsgType[TriggerWithIdMessage]
      receivedPvActivation1.trigger shouldBe ActivityStartTrigger(0L)
      receivedPvActivation1.receiverActor shouldBe pvAgent.ref

      pvAgent.expectMsg(RequestFlexOptions(0L))

      val receivedEvcsActivation1 =
        evcsAgent.expectMsgType[TriggerWithIdMessage]
      receivedEvcsActivation1.trigger shouldBe ActivityStartTrigger(0L)
      receivedEvcsActivation1.receiverActor shouldBe evcsAgent.ref

      evcsAgent.expectMsg(RequestFlexOptions(0L))

      // send flex options
      pvAgent.send(
        emAgent,
        ProvideMinMaxFlexOptions(
          pvInput.getUuid,
          (-5d).asKiloWatt,
          (-5d).asKiloWatt,
          0d.asKiloWatt
        )
      )

      pvAgent.expectNoMessage()
      evcsAgent.expectNoMessage()

      evcsAgent.send(
        emAgent,
        ProvideMinMaxFlexOptions(
          evcsInput.getUuid,
          2d.asKiloWatt,
          (-11d).asKiloWatt,
          11d.asKiloWatt
        )
      )

      // receive flex control messages
      pvAgent.expectMsg(IssueNoCtrl(0L))
      pvAgent.send(
        emAgent,
        ParticipantResultEvent(
          new PvResult(
            0L.toDateTime,
            pvInput.getUuid,
            (-5d).asKiloWatt,
            (-0.5d).asKiloVar
          )
        )
      )

      evcsAgent.expectMsgType[IssuePowerCtrl] match {
        case IssuePowerCtrl(0L, setPower) =>
          setPower should equalWithTolerance(5d.asKiloWatt, tolerance)
      }
      evcsAgent.send(
        emAgent,
        ParticipantResultEvent(
          new EvcsResult(
            0L.toDateTime,
            evcsInput.getUuid,
            5d.asKiloWatt,
            0.1d.asKiloVar
          )
        )
      )

      // expect correct results
      resultsProbe.expectMsgType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe simulationStartDate
          emResult.getP should equalWithTolerance(0d.asMegaWatt, tolerance)
          emResult.getQ should equalWithTolerance(
            (-.0004d).asMegaVar,
            tolerance
          )
        case unexpected =>
          fail(s"Received unexpected result $unexpected")
      }

      // send completions

      pvAgent.send(
        emAgent,
        FlexCtrlCompletion(
          pvInput.getUuid
        )
      )

      pvAgent.send(
        emAgent,
        CompletionMessage(
          receivedPvActivation1.triggerId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(300L),
                pvAgent.ref
              )
            )
          )
        )
      )

      evcsAgent.send(
        emAgent,
        FlexCtrlCompletion(
          evcsInput.getUuid,
          requestAtTick = Some(600L)
        )
      )

      scheduler.expectNoMessage()

      evcsAgent.send(
        emAgent,
        CompletionMessage(
          receivedEvcsActivation1.triggerId,
          None
        )
      )

      // expect completion from EmAgent
      scheduler.expectMsg(
        CompletionMessage(
          activationId1,
          Some(Seq(ScheduleTriggerMessage(ActivityStartTrigger(300L), emAgent)))
        )
      )

      // trigger EmAgent with next tick
      val activationId2 = 1L

      scheduler.send(
        emAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(300L),
          activationId2,
          emAgent
        )
      )

      // expect activations and flex requests.
      // only participant 1 has been scheduled for this tick,
      // thus 2 does not get activated
      evcsAgent.expectNoMessage()

      val receivedPvActivation2 =
        pvAgent.expectMsgType[TriggerWithIdMessage]
      receivedPvActivation2.trigger shouldBe ActivityStartTrigger(300L)
      receivedPvActivation2.receiverActor shouldBe pvAgent.ref

      pvAgent.expectMsg(RequestFlexOptions(300L))

      // send flex options again, now there's a cloud and thus less feed-in
      pvAgent.send(
        emAgent,
        ProvideMinMaxFlexOptions(
          pvInput.getUuid,
          (-3d).asKiloWatt,
          (-3d).asKiloWatt,
          0d.asKiloWatt
        )
      )

      // receive flex control messages
      pvAgent.expectMsg(IssueNoCtrl(300L))

      pvAgent.send(
        emAgent,
        ParticipantResultEvent(
          new PvResult(
            300L.toDateTime,
            pvInput.getUuid,
            (-3d).asKiloWatt,
            (-0.06d).asKiloVar
          )
        )
      )

      pvAgent.send(
        emAgent,
        FlexCtrlCompletion(
          pvInput.getUuid
        )
      )

      pvAgent.send(
        emAgent,
        CompletionMessage(receivedPvActivation2.triggerId, None)
      )

      // evcs is now sent control too
      evcsAgent.expectMsgType[IssuePowerCtrl] match {
        case IssuePowerCtrl(300L, setPower) =>
          setPower should equalWithTolerance(3d.asKiloWatt, tolerance)
      }

      scheduler.expectNoMessage()

      evcsAgent.send(
        emAgent,
        FlexCtrlCompletion(
          modelUuid = evcsInput.getUuid,
          revokeRequestAtTick = Some(600L),
          requestAtTick = Some(800L)
        )
      )

      evcsAgent.send(
        emAgent,
        ParticipantResultEvent(
          new EvcsResult(
            300L.toDateTime,
            evcsInput.getUuid,
            3d.asKiloWatt,
            0.06d.asKiloVar
          )
        )
      )

      // expect correct results
      resultsProbe.expectMsgType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 300L.toDateTime(simulationStartDate)
          emResult.getP should equalWithTolerance(
            0d.asMegaWatt,
            tolerance
          )
          emResult.getQ should equalWithTolerance(
            0d.asMegaVar,
            tolerance
          )
        case unexpected =>
          fail(s"Received unexpected result $unexpected")
      }

      // expect completion from EmAgent with new tick (800) instead of revoked tick (600)
      scheduler.expectMsg(
        CompletionMessage(
          activationId2,
          Some(Seq(ScheduleTriggerMessage(ActivityStartTrigger(800L), emAgent)))
        )
      )
    }

    "handle ChangingFlexOptions indicator correctly" in {
      val resultsProbe = TestProbe("ResultListener")

      val emAgent = TestActorRef(
        new EmAgent(
          scheduler = scheduler.ref,
          listener = Iterable(resultsProbe.ref)
        )
      )

      val initId = 0

      val pvAgent = TestProbe("PvAgent")
      val evcsAgent = TestProbe("EvcsAgent")

      val pvAgentInit =
        InitializeParticipantAgentTrigger[ApparentPower, InitializeStateData[
          ApparentPower
        ]](mock[InitializeStateData[ApparentPower]])
      val evcsAgentInit =
        InitializeParticipantAgentTrigger[ApparentPower, InitializeStateData[
          ApparentPower
        ]](mock[InitializeStateData[ApparentPower]])

      val connectedAgents = Seq(
        (
          pvAgent.ref,
          pvAgentInit,
          pvInput
        ),
        (
          evcsAgent.ref,
          evcsAgentInit,
          evcsInput
        )
      )

      scheduler.send(
        emAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            EmAgentInitializeStateData
          ](
            EmAgentInitializeStateData(
              inputModel = emInput,
              modelConfig = modelConfig,
              secondaryDataServices = None,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = outputConfig,
              modelStrategy = PrioritizedFlexStrat,
              primaryServiceProxy = primaryServiceProxy.ref,
              connectedAgents = connectedAgents
            )
          ),
          initId,
          emAgent
        )
      )

      val receivedPvInit = pvAgent
        .expectMsgType[TriggerWithIdMessage]
      receivedPvInit.trigger shouldBe pvAgentInit

      val receivedEvcsInit = evcsAgent
        .expectMsgType[TriggerWithIdMessage]
      receivedEvcsInit.trigger shouldBe evcsAgentInit

      pvAgent.send(
        emAgent,
        CompletionMessage(
          receivedPvInit.triggerId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(0L),
                pvAgent.ref
              )
            )
          )
        )
      )

      scheduler.expectNoMessage()

      evcsAgent.send(
        emAgent,
        CompletionMessage(
          receivedEvcsInit.triggerId,
          None
        )
      )

      scheduler.expectMsg(
        CompletionMessage(
          initId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(0L),
                emAgent
              )
            )
          )
        )
      )

      // we receive ev arrivals at the first tick
      emAgent ! ScheduleTriggerMessage(ActivityStartTrigger(0L), evcsAgent.ref)

      // init done, start EmAgent
      val activationId1 = 1L

      scheduler.send(
        emAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          activationId1,
          emAgent
        )
      )

      // expect activations and flex requests
      val receivedPvActivation1 =
        pvAgent.expectMsgType[TriggerWithIdMessage]
      receivedPvActivation1.trigger shouldBe ActivityStartTrigger(0L)
      receivedPvActivation1.receiverActor shouldBe pvAgent.ref

      pvAgent.expectMsg(RequestFlexOptions(0L))

      val receivedEvcsActivation1 =
        evcsAgent.expectMsgType[TriggerWithIdMessage]
      receivedEvcsActivation1.trigger shouldBe ActivityStartTrigger(0L)
      receivedEvcsActivation1.receiverActor shouldBe evcsAgent.ref

      evcsAgent.expectMsg(RequestFlexOptions(0L))

      // send flex options
      pvAgent.send(
        emAgent,
        ProvideMinMaxFlexOptions(
          pvInput.getUuid,
          (-5d).asKiloWatt,
          (-5d).asKiloWatt,
          0d.asKiloWatt
        )
      )

      pvAgent.expectNoMessage()
      evcsAgent.expectNoMessage()

      evcsAgent.send(
        emAgent,
        ProvideMinMaxFlexOptions(
          evcsInput.getUuid,
          2d.asKiloWatt,
          0d.asKiloWatt,
          11d.asKiloWatt
        )
      )

      // receive flex control messages
      pvAgent.expectMsg(IssueNoCtrl(0L))
      pvAgent.send(
        emAgent,
        ParticipantResultEvent(
          new PvResult(
            0L.toDateTime,
            pvInput.getUuid,
            (-5d).asKiloWatt,
            (-0.5d).asKiloVar
          )
        )
      )

      evcsAgent.expectMsgType[IssuePowerCtrl] match {
        case IssuePowerCtrl(0L, setPower) =>
          setPower should equalWithTolerance(5d.asKiloWatt, tolerance)
      }

      evcsAgent.send(
        emAgent,
        ParticipantResultEvent(
          new EvcsResult(
            0L.toDateTime,
            evcsInput.getUuid,
            5d.asKiloWatt,
            0.1d.asKiloVar
          )
        )
      )

      // expect correct results
      resultsProbe.expectMsgType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe simulationStartDate
          emResult.getP should equalWithTolerance(0d.asMegaWatt, tolerance)
          emResult.getQ should equalWithTolerance(
            (-.0004d).asMegaVar,
            tolerance
          )
        case unexpected =>
          fail(s"Received unexpected result $unexpected")
      }

      // send completions
      pvAgent.send(
        emAgent,
        FlexCtrlCompletion(
          pvInput.getUuid
        )
      )

      pvAgent.send(
        emAgent,
        CompletionMessage(
          receivedPvActivation1.triggerId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(300L),
                pvAgent.ref
              )
            )
          )
        )
      )

      // sending ChangingFlexOptions indicator
      evcsAgent.send(
        emAgent,
        FlexCtrlCompletion(
          modelUuid = evcsInput.getUuid,
          requestAtNextActivation = true,
          requestAtTick = Some(600L)
        )
      )

      scheduler.expectNoMessage()

      evcsAgent.send(
        emAgent,
        CompletionMessage(
          receivedEvcsActivation1.triggerId,
          None
        )
      )

      // expect completion from EmAgent
      scheduler.expectMsg(
        CompletionMessage(
          activationId1,
          Some(Seq(ScheduleTriggerMessage(ActivityStartTrigger(300L), emAgent)))
        )
      )

      // trigger EmAgent with next tick
      val activationId2 = 1L

      scheduler.send(
        emAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(300L),
          activationId2,
          emAgent
        )
      )

      // expect activations and flex requests.
      // pv is scheduled regularly and evcs at any next tick
      // thus, we expect activations for both
      val receivedPvActivation2 =
        pvAgent.expectMsgType[TriggerWithIdMessage]
      receivedPvActivation2.trigger shouldBe ActivityStartTrigger(300L)
      receivedPvActivation2.receiverActor shouldBe pvAgent.ref

      // FLEX OPTIONS
      pvAgent.expectMsg(RequestFlexOptions(300L))

      // send flex options again, now there's a cloud and thus less feed-in
      pvAgent.send(
        emAgent,
        ProvideMinMaxFlexOptions(
          pvInput.getUuid,
          (-3d).asKiloWatt,
          (-3d).asKiloWatt,
          0d.asKiloWatt
        )
      )

      // expecting flex options request, since we asked for it last time
      evcsAgent.expectMsg(RequestFlexOptions(300L))

      evcsAgent.send(
        emAgent,
        ProvideMinMaxFlexOptions(
          evcsInput.getUuid,
          2d.asKiloWatt,
          (-11d).asKiloWatt,
          11d.asKiloWatt
        )
      )

      // FLEX CONTROL
      pvAgent.expectMsg(IssueNoCtrl(300L))

      pvAgent.send(
        emAgent,
        ParticipantResultEvent(
          new PvResult(
            300L.toDateTime,
            pvInput.getUuid,
            (-3d).asKiloWatt,
            (-0.06d).asKiloVar
          )
        )
      )

      pvAgent.send(
        emAgent,
        FlexCtrlCompletion(
          pvInput.getUuid
        )
      )

      pvAgent.send(
        emAgent,
        CompletionMessage(receivedPvActivation2.triggerId, None)
      )

      evcsAgent.expectMsgType[IssuePowerCtrl] match {
        case IssuePowerCtrl(300L, setPower) =>
          setPower should equalWithTolerance(3d.asKiloWatt, tolerance)
      }
      evcsAgent.send(
        emAgent,
        FlexCtrlCompletion(
          modelUuid = evcsInput.getUuid,
          revokeRequestAtTick = Some(600)
        )
      )

      evcsAgent.send(
        emAgent,
        ParticipantResultEvent(
          new EvcsResult(
            300L.toDateTime,
            evcsInput.getUuid,
            3d.asKiloWatt,
            0.06d.asKiloVar
          )
        )
      )

      // expect correct results
      resultsProbe.expectMsgType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 300L.toDateTime(simulationStartDate)
          emResult.getP should equalWithTolerance(
            0d.asMegaWatt,
            tolerance
          )
          emResult.getQ should equalWithTolerance(
            0d.asMegaVar,
            tolerance
          )
        case unexpected =>
          fail(s"Received unexpected result $unexpected")
      }

      scheduler.expectMsg(
        CompletionMessage(
          activationId2,
          None
        )
      )
    }

  }

  "An EM-controlled EM agent" should {
    "be initialized correctly and run through some activations" in {
      val resultsProbe = TestProbe("ResultListener")

      val parentEmAgent = TestProbe("ParentEmAgent")

      val emAgent = TestActorRef(
        new EmAgent(
          scheduler = parentEmAgent.ref,
          listener = Iterable(resultsProbe.ref)
        )
      )

      val initId = 0

      val pvAgent = TestProbe("PvAgent")
      val evcsAgent = TestProbe("EvcsAgent")

      val pvAgentInit =
        InitializeParticipantAgentTrigger[ApparentPower, InitializeStateData[
          ApparentPower
        ]](mock[InitializeStateData[ApparentPower]])
      val evcsAgentInit =
        InitializeParticipantAgentTrigger[ApparentPower, InitializeStateData[
          ApparentPower
        ]](mock[InitializeStateData[ApparentPower]])

      val connectedAgents = Seq(
        (pvAgent.ref, pvAgentInit, pvInput),
        (evcsAgent.ref, evcsAgentInit, evcsInput)
      )

      scheduler.send(
        emAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            EmAgentInitializeStateData
          ](
            EmAgentInitializeStateData(
              inputModel = emInput,
              modelConfig = modelConfig,
              secondaryDataServices = None,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = outputConfig,
              primaryServiceProxy = primaryServiceProxy.ref,
              modelStrategy = PrioritizedFlexStrat,
              connectedAgents = connectedAgents,
              maybeParentEmAgent = Some(parentEmAgent.ref) // em-controlled
            )
          ),
          initId,
          emAgent
        )
      )

      val receivedPvInit = pvAgent.expectMsgType[TriggerWithIdMessage]
      receivedPvInit.trigger shouldBe pvAgentInit

      val receivedEvcsInit = evcsAgent.expectMsgType[TriggerWithIdMessage]
      receivedEvcsInit.trigger shouldBe evcsAgentInit

      pvAgent.send(
        emAgent,
        ScheduleTriggerMessage(
          RequestFlexOptions(0L),
          pvAgent.ref
        )
      )

      pvAgent.send(
        emAgent,
        CompletionMessage(
          receivedPvInit.triggerId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(0L),
                pvAgent.ref
              )
            )
          )
        )
      )

      evcsAgent.send(
        emAgent,
        ScheduleTriggerMessage(
          RequestFlexOptions(0L),
          evcsAgent.ref
        )
      )

      parentEmAgent.expectNoMessage()

      evcsAgent.send(
        emAgent,
        CompletionMessage(
          receivedEvcsInit.triggerId,
          None
        )
      )

      parentEmAgent.expectMsg(
        CompletionMessage(
          initId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                RequestFlexOptions(0L),
                emAgent
              )
            )
          )
        )
      )

      // no arrivals for EVCS yet

      // init done, start EmAgent
      parentEmAgent.send(
        emAgent,
        RequestFlexOptions(0L)
      )

      // expect activations and flex requests
      val receivedPvActivation1 =
        pvAgent.expectMsgType[TriggerWithIdMessage]
      receivedPvActivation1.trigger shouldBe ActivityStartTrigger(0L)
      receivedPvActivation1.receiverActor shouldBe pvAgent.ref

      pvAgent.expectMsg(RequestFlexOptions(0L))

      evcsAgent.expectMsg(RequestFlexOptions(0L))

      // send flex options
      pvAgent.send(
        emAgent,
        ProvideMinMaxFlexOptions(
          pvInput.getUuid,
          (-5d).asKiloWatt,
          (-5d).asKiloWatt,
          0d.asKiloWatt
        )
      )

      evcsAgent.send(
        emAgent,
        ProvideMinMaxFlexOptions(
          evcsInput.getUuid,
          2d.asKiloWatt,
          (-11d).asKiloWatt,
          11d.asKiloWatt
        )
      )

      parentEmAgent.expectMsgType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              referencePower,
              minPower,
              maxPower
            ) =>
          modelUuid shouldBe emInput.getUuid
          referencePower shouldBe 0d.asKiloWatt
          minPower shouldBe (-16d).asKiloWatt
          maxPower shouldBe 6d.asKiloWatt // hint: PV is not flexible
      }

      // issue power control and expect EmAgent to distribute it
      parentEmAgent.send(emAgent, IssuePowerCtrl(0L, 6d.asKiloWatt))

      // expect issue power control
      pvAgent.expectMsg(IssueNoCtrl(0L))

      pvAgent.send(
        emAgent,
        ParticipantResultEvent(
          new PvResult(
            0L.toDateTime,
            pvInput.getUuid,
            (-5d).asKiloWatt,
            (-0.5d).asKiloVar
          )
        )
      )
      pvAgent.send(
        emAgent,
        FlexCtrlCompletion(pvInput.getUuid)
      )
      pvAgent.send(
        emAgent,
        CompletionMessage(
          receivedPvActivation1.triggerId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(600L),
                pvAgent.ref
              )
            )
          )
        )
      )

      evcsAgent.expectMsgType[IssuePowerCtrl] match {
        case IssuePowerCtrl(0L, setPower) =>
          setPower should equalWithTolerance(11d.asKiloWatt, tolerance)
      }

      evcsAgent.send(
        emAgent,
        ParticipantResultEvent(
          new EvcsResult(
            0L.toDateTime,
            evcsInput.getUuid,
            11d.asKiloWatt,
            1.1d.asKiloVar
          )
        )
      )
      evcsAgent.send(
        emAgent,
        FlexCtrlCompletion(
          evcsInput.getUuid,
          requestAtTick = Some(300L)
        )
      )

      // expect correct results
      parentEmAgent.expectMsgType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe simulationStartDate
          emResult.getP should beEquivalentTo(6d.asKiloWatt, tolerance)
          emResult.getQ should beEquivalentTo(0.6d.asKiloVar, tolerance)
        case unexpected =>
          fail(s"Received unexpected result $unexpected")
      }

      // expect correct results
      resultsProbe.expectMsgType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe simulationStartDate
          emResult.getP should beEquivalentTo(6d.asKiloWatt, tolerance)
          emResult.getQ should beEquivalentTo(0.6d.asKiloVar, tolerance)
        case unexpected =>
          fail(s"Received unexpected result $unexpected")
      }

      parentEmAgent.expectMsg(
        FlexCtrlCompletion(
          emInput.getUuid,
          requestAtTick = Some(300L)
        )
      )

    }
  }

}