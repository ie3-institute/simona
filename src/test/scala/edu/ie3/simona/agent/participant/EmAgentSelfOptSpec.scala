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
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.ontology.messages.FlexibilityMessage.{
  IssueNoCtrl,
  IssuePowerCtrl,
  ProvideMinMaxFlexOptions,
  RequestFlexOptions
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  RevokeTriggerMessage,
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
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.scalatestplus.mockito.MockitoSugar
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime

class EmAgentSelfOptSpec
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

  private val outputConfig = ParticipantNotifierConfig(
    simulationResultInfo = true,
    powerRequestReply = false
  )

  private val tolerance = 1e-10d

  "An em agent" should {
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
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(0L),
                evcsAgent.ref
              )
            )
          )
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

      pvAgent.expectMsg(RequestFlexOptions)

      val receivedEvcsActivation1 =
        evcsAgent.expectMsgType[TriggerWithIdMessage]
      receivedEvcsActivation1.trigger shouldBe ActivityStartTrigger(0L)
      receivedEvcsActivation1.receiverActor shouldBe evcsAgent.ref

      evcsAgent.expectMsg(RequestFlexOptions)

      // send flex options
      pvAgent.send(
        emAgent,
        ProvideMinMaxFlexOptions(
          pvInput.getUuid,
          Quantities.getQuantity(-5d, PowerSystemUnits.KILOWATT),
          Quantities.getQuantity(-5d, PowerSystemUnits.KILOWATT),
          Quantities.getQuantity(0d, PowerSystemUnits.KILOWATT)
        )
      )

      pvAgent.expectNoMessage()
      evcsAgent.expectNoMessage()

      evcsAgent.send(
        emAgent,
        ProvideMinMaxFlexOptions(
          evcsInput.getUuid,
          Quantities.getQuantity(2d, PowerSystemUnits.KILOWATT),
          Quantities.getQuantity(-11d, PowerSystemUnits.KILOWATT),
          Quantities.getQuantity(11d, PowerSystemUnits.KILOWATT)
        )
      )

      // receive flex control messages
      pvAgent.expectMsg(IssueNoCtrl)
      pvAgent.send(
        emAgent,
        ParticipantResultEvent(
          new PvResult(
            0L.toDateTime,
            pvInput.getUuid,
            Quantities.getQuantity(-5d, PowerSystemUnits.KILOWATT),
            Quantities.getQuantity(-0.5d, PowerSystemUnits.KILOVAR)
          )
        )
      )

      evcsAgent.expectMsgType[IssuePowerCtrl] match {
        case IssuePowerCtrl(setPower) =>
          setPower should equalWithTolerance(5d.asKiloWatt, tolerance)
      }
      evcsAgent.send(
        emAgent,
        ParticipantResultEvent(
          new EvcsResult(
            0L.toDateTime,
            evcsInput.getUuid,
            Quantities.getQuantity(5d, PowerSystemUnits.KILOWATT),
            Quantities.getQuantity(0.1d, PowerSystemUnits.KILOVAR)
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

      scheduler.expectNoMessage()

      evcsAgent.send(
        emAgent,
        CompletionMessage(
          receivedEvcsActivation1.triggerId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(300L),
                evcsAgent.ref
              )
            )
          )
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

      val receivedEvcsActivation2 =
        evcsAgent.expectMsgType[TriggerWithIdMessage]
      receivedEvcsActivation2.trigger shouldBe ActivityStartTrigger(300L)
      receivedEvcsActivation2.receiverActor shouldBe evcsAgent.ref

      evcsAgent.expectMsg(RequestFlexOptions)

      // send flex options again, ev is fully charged
      evcsAgent.send(
        emAgent,
        ProvideMinMaxFlexOptions(
          evcsInput.getUuid,
          Quantities.getQuantity(0d, PowerSystemUnits.KILOWATT),
          Quantities.getQuantity(-11d, PowerSystemUnits.KILOWATT),
          Quantities.getQuantity(0d, PowerSystemUnits.KILOWATT)
        )
      )

      // receive flex control messages
      pvAgent.expectNoMessage()

      evcsAgent.expectMsg(IssueNoCtrl)
      evcsAgent.send(
        emAgent,
        ParticipantResultEvent(
          new EvcsResult(
            0L.toDateTime,
            evcsInput.getUuid,
            Quantities.getQuantity(0d, PowerSystemUnits.KILOWATT),
            Quantities.getQuantity(0d, PowerSystemUnits.KILOVAR)
          )
        )
      )

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
        CompletionMessage(receivedEvcsActivation2.triggerId, None)
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
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(0L),
                evcsAgent.ref
              )
            )
          )
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

      pvAgent.expectMsg(RequestFlexOptions)

      val receivedEvcsActivation1 =
        evcsAgent.expectMsgType[TriggerWithIdMessage]
      receivedEvcsActivation1.trigger shouldBe ActivityStartTrigger(0L)
      receivedEvcsActivation1.receiverActor shouldBe evcsAgent.ref

      evcsAgent.expectMsg(RequestFlexOptions)

      // send flex options
      pvAgent.send(
        emAgent,
        ProvideMinMaxFlexOptions(
          pvInput.getUuid,
          Quantities.getQuantity(-5d, PowerSystemUnits.KILOWATT),
          Quantities.getQuantity(-5d, PowerSystemUnits.KILOWATT),
          Quantities.getQuantity(0d, PowerSystemUnits.KILOWATT)
        )
      )

      pvAgent.expectNoMessage()
      evcsAgent.expectNoMessage()

      evcsAgent.send(
        emAgent,
        ProvideMinMaxFlexOptions(
          evcsInput.getUuid,
          Quantities.getQuantity(2d, PowerSystemUnits.KILOWATT),
          Quantities.getQuantity(-11d, PowerSystemUnits.KILOWATT),
          Quantities.getQuantity(11d, PowerSystemUnits.KILOWATT)
        )
      )

      // receive flex control messages
      pvAgent.expectMsg(IssueNoCtrl)
      pvAgent.send(
        emAgent,
        ParticipantResultEvent(
          new PvResult(
            0L.toDateTime,
            pvInput.getUuid,
            Quantities.getQuantity(-5d, PowerSystemUnits.KILOWATT),
            Quantities.getQuantity(-0.5d, PowerSystemUnits.KILOVAR)
          )
        )
      )

      evcsAgent.expectMsgType[IssuePowerCtrl] match {
        case IssuePowerCtrl(setPower) =>
          setPower should equalWithTolerance(5d.asKiloWatt, tolerance)
      }
      evcsAgent.send(
        emAgent,
        ParticipantResultEvent(
          new EvcsResult(
            0L.toDateTime,
            evcsInput.getUuid,
            Quantities.getQuantity(5d, PowerSystemUnits.KILOWATT),
            Quantities.getQuantity(0.1d, PowerSystemUnits.KILOVAR)
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

      scheduler.expectNoMessage()

      evcsAgent.send(
        emAgent,
        CompletionMessage(
          receivedEvcsActivation1.triggerId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(600L),
                evcsAgent.ref
              )
            )
          )
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

      pvAgent.expectMsg(RequestFlexOptions)

      // send flex options again, now there's a cloud and thus less feed-in
      pvAgent.send(
        emAgent,
        ProvideMinMaxFlexOptions(
          pvInput.getUuid,
          Quantities.getQuantity(-3d, PowerSystemUnits.KILOWATT),
          Quantities.getQuantity(-3d, PowerSystemUnits.KILOWATT),
          Quantities.getQuantity(0d, PowerSystemUnits.KILOWATT)
        )
      )

      // receive flex control messages
      pvAgent.expectMsg(IssueNoCtrl)

      pvAgent.send(
        emAgent,
        ParticipantResultEvent(
          new PvResult(
            300L.toDateTime,
            pvInput.getUuid,
            Quantities.getQuantity(-3d, PowerSystemUnits.KILOWATT),
            Quantities.getQuantity(-0.06d, PowerSystemUnits.KILOVAR)
          )
        )
      )

      pvAgent.send(
        emAgent,
        CompletionMessage(receivedPvActivation2.triggerId, None)
      )

      // evcs is now activated too
      val receivedEvcsActivation2 =
        evcsAgent.expectMsgType[TriggerWithIdMessage]
      receivedEvcsActivation2.trigger shouldBe ActivityStartTrigger(300L)
      receivedEvcsActivation2.receiverActor shouldBe evcsAgent.ref

      evcsAgent.expectMsgType[IssuePowerCtrl] match {
        case IssuePowerCtrl(setPower) =>
          setPower should equalWithTolerance(3d.asKiloWatt, tolerance)
      }
      evcsAgent.send(
        emAgent,
        RevokeTriggerMessage(
          ActivityStartTrigger(600L),
          evcsAgent.ref
        )
      )

      evcsAgent.send(
        emAgent,
        ParticipantResultEvent(
          new EvcsResult(
            300L.toDateTime,
            evcsInput.getUuid,
            Quantities.getQuantity(3d, PowerSystemUnits.KILOWATT),
            Quantities.getQuantity(0.06d, PowerSystemUnits.KILOVAR)
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

      // send completion
      scheduler.expectNoMessage()

      evcsAgent.send(
        emAgent,
        CompletionMessage(
          receivedEvcsActivation2.triggerId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(800L),
                evcsAgent.ref
              )
            )
          )
        )
      )

      // expect completion from EmAgent with new tick (800) instead of revoked tick (600)
      scheduler.expectMsg(
        CompletionMessage(
          activationId2,
          Some(Seq(ScheduleTriggerMessage(ActivityStartTrigger(800L), emAgent)))
        )
      )

    }
  }

}
