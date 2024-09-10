/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.em

import edu.ie3.datamodel.models.result.system.EmResult
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPower
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.{
  FlexOptionsResultEvent,
  ParticipantResultEvent,
}
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage._
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.test.common.input.EmInputTestData
import edu.ie3.simona.test.matchers.SquantsMatchers
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.QuantityMatchers.equalWithTolerance
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.{Kilovars, ReactivePower}
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.mockito.MockitoSugar
import squants.Power
import squants.energy.Kilowatts

import java.time.ZonedDateTime

class EmAgentSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with should.Matchers
    with EmInputTestData
    with MockitoSugar
    with SquantsMatchers {

  protected implicit val simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z")

  private val outputConfig = NotifierConfig(
    simulationResultInfo = true,
    powerRequestReply = false,
    flexResult = true, // also test FlexOptionsResult if EM-controlled
  )

  override protected val modelConfig: EmRuntimeConfig = EmRuntimeConfig(
    calculateMissingReactivePowerWithModel = false,
    scaling = 1d,
    uuids = List("default"),
    aggregateFlex = "SELF_OPT_EXCL_REG",
    curtailRegenerative = false,
  )

  private implicit val activePowerTolerance: Power = Kilowatts(1e-10)
  private implicit val reactivePowerTolerance: ReactivePower = Kilovars(1e-10)

  "A self-optimizing EM agent" should {
    "be initialized correctly and run through some activations" in {
      val resultListener = TestProbe[ResultEvent]("ResultListener")
      val scheduler = TestProbe[SchedulerMessage]("Scheduler")

      val emAgent = spawn(
        EmAgent(
          emInput,
          modelConfig,
          outputConfig,
          "PRIORITIZED",
          simulationStartDate,
          parent = Left(scheduler.ref),
          listener = Iterable(resultListener.ref),
        )
      )

      val pvAgent = TestProbe[FlexRequest]("PvAgent")
      emAgent ! RegisterParticipant(pvInput.getUuid, pvAgent.ref, pvInput)
      emAgent ! ScheduleFlexRequest(pvInput.getUuid, 0)

      val sa1 = scheduler.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe 0
      sa1.unlockKey shouldBe None
      val emAgentActivation = sa1.actor

      val evcsAgent = TestProbe[FlexRequest]("EvcsAgent")
      emAgent ! RegisterParticipant(evcsInput.getUuid, evcsAgent.ref, evcsInput)
      emAgent ! ScheduleFlexRequest(evcsInput.getUuid, 0)

      // no additional scheduling message, since tick 0 has already been scheduled
      scheduler.expectNoMessage()

      /* TICK 0 */
      emAgentActivation ! Activation(0)

      // expect flex requests
      pvAgent.expectMessage(RequestFlexOptions(0))
      evcsAgent.expectMessage(RequestFlexOptions(0))

      // send flex options
      emAgent ! ProvideMinMaxFlexOptions(
        pvInput.getUuid,
        Kilowatts(-5d),
        Kilowatts(-5d),
        Kilowatts(0d),
      )

      pvAgent.expectNoMessage()
      evcsAgent.expectNoMessage()

      emAgent ! ProvideMinMaxFlexOptions(
        evcsInput.getUuid,
        Kilowatts(2d),
        Kilowatts(-11d),
        Kilowatts(11d),
      )

      // receive flex control messages
      pvAgent.expectMessage(IssueNoControl(0))
      emAgent ! FlexCtrlCompletion(
        modelUuid = pvInput.getUuid,
        result = ComplexPower(Kilowatts(-5d), Kilovars(-0.5d)),
        requestAtTick = Some(600),
      )

      scheduler.expectNoMessage()

      evcsAgent.expectMessageType[IssuePowerControl] match {
        case IssuePowerControl(0, setPower) =>
          setPower should approximate(Kilowatts(5.0))
      }
      emAgent ! FlexCtrlCompletion(
        modelUuid = evcsInput.getUuid,
        result = ComplexPower(Kilowatts(5d), Kilovars(0.1d)),
        requestAtTick = Some(300),
      )

      // expect correct results
      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe simulationStartDate
          emResult.getP should equalWithTolerance(0d.asMegaWatt)
          emResult.getQ should equalWithTolerance((-.0004d).asMegaVar)
      }

      // expect completion from EmAgent
      scheduler.expectMessage(
        Completion(emAgentActivation, Some(300))
      )

      /* TICK 300 */
      emAgentActivation ! Activation(300)

      // expect activations and flex requests.
      // only participant 2 has been scheduled for this tick,
      // thus 1 does not get activated
      pvAgent.expectNoMessage()

      evcsAgent.expectMessage(RequestFlexOptions(300))

      // send flex options again, ev is fully charged
      emAgent ! ProvideMinMaxFlexOptions(
        evcsInput.getUuid,
        Kilowatts(0d),
        Kilowatts(-11d),
        Kilowatts(0d),
      )

      // receive flex control messages
      evcsAgent.expectMessage(IssueNoControl(300))

      pvAgent.expectNoMessage()

      emAgent !
        FlexCtrlCompletion(
          evcsInput.getUuid,
          ComplexPower(Kilowatts(0d), Kilovars(0d)),
        )

      // expect correct results
      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 300.toDateTime(simulationStartDate)
          emResult.getP should equalWithTolerance((-0.005d).asMegaWatt)
          emResult.getQ should equalWithTolerance((-0.0005d).asMegaVar)
      }

      // expect completion from EmAgent
      scheduler.expectMessage(Completion(emAgentActivation, Some(600)))

    }

    "revoke triggers correctly" in {
      val resultListener = TestProbe[ResultEvent]("ResultListener")
      val scheduler = TestProbe[SchedulerMessage]("Scheduler")

      val emAgent = spawn(
        EmAgent(
          emInput,
          modelConfig,
          outputConfig,
          "PRIORITIZED",
          simulationStartDate,
          parent = Left(scheduler.ref),
          listener = Iterable(resultListener.ref),
        )
      )

      val pvAgent = TestProbe[FlexRequest]("PvAgent")
      emAgent ! RegisterParticipant(pvInput.getUuid, pvAgent.ref, pvInput)
      emAgent ! ScheduleFlexRequest(pvInput.getUuid, 0)

      val sa1 = scheduler.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe 0
      sa1.unlockKey shouldBe None
      val emAgentActivation = sa1.actor

      val evcsAgent = TestProbe[FlexRequest]("EvcsAgent")
      emAgent ! RegisterParticipant(evcsInput.getUuid, evcsAgent.ref, evcsInput)
      emAgent ! ScheduleFlexRequest(evcsInput.getUuid, 0)

      // no additional scheduling message, since tick 0 has already been scheduled
      scheduler.expectNoMessage()

      /* TICK 0 */
      emAgentActivation ! Activation(0)

      // expect flex requests
      pvAgent.expectMessage(RequestFlexOptions(0))
      evcsAgent.expectMessage(RequestFlexOptions(0))

      // send flex options
      emAgent ! ProvideMinMaxFlexOptions(
        pvInput.getUuid,
        Kilowatts(-5d),
        Kilowatts(-5d),
        Kilowatts(0d),
      )

      pvAgent.expectNoMessage()
      evcsAgent.expectNoMessage()

      emAgent ! ProvideMinMaxFlexOptions(
        evcsInput.getUuid,
        Kilowatts(2d),
        Kilowatts(-11d),
        Kilowatts(11d),
      )

      // receive flex control messages
      pvAgent.expectMessage(IssueNoControl(0))
      evcsAgent.expectMessageType[IssuePowerControl] match {
        case IssuePowerControl(0, setPower) =>
          setPower should approximate(Kilowatts(5.0))
      }

      // send completions
      emAgent ! FlexCtrlCompletion(
        modelUuid = pvInput.getUuid,
        result = ComplexPower(Kilowatts(-5d), Kilovars(-0.5d)),
        requestAtTick = Some(300),
      )

      scheduler.expectNoMessage()

      emAgent ! FlexCtrlCompletion(
        modelUuid = evcsInput.getUuid,
        result = ComplexPower(Kilowatts(5d), Kilovars(0.1d)),
        requestAtTick = Some(600),
      )

      // expect correct results
      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe simulationStartDate
          emResult.getP should equalWithTolerance(0d.asMegaWatt)
          emResult.getQ should equalWithTolerance((-.0004d).asMegaVar)
      }

      // expect completion from EmAgent
      scheduler.expectMessage(Completion(emAgentActivation, Some(300)))

      /* TICK 300 */
      emAgentActivation ! Activation(300)

      // expect activations and flex requests.
      // only pv agent has been scheduled for this tick,
      // thus evcs does not get activated
      evcsAgent.expectNoMessage()

      pvAgent.expectMessage(RequestFlexOptions(300))

      // send flex options again, now there's a cloud and thus less feed-in
      emAgent ! ProvideMinMaxFlexOptions(
        pvInput.getUuid,
        Kilowatts(-3d),
        Kilowatts(-3d),
        Kilowatts(0d),
      )

      // receive flex control messages
      pvAgent.expectMessage(IssueNoControl(300))

      emAgent ! FlexCtrlCompletion(
        pvInput.getUuid,
        ComplexPower(Kilowatts(-3d), Kilovars(-0.06d)),
      )

      // evcs is now sent control too
      evcsAgent.expectMessageType[IssuePowerControl] match {
        case IssuePowerControl(300, setPower) =>
          setPower should approximate(Kilowatts(3.0))
      }

      scheduler.expectNoMessage()

      emAgent ! FlexCtrlCompletion(
        evcsInput.getUuid,
        ComplexPower(Kilowatts(3d), Kilovars(0.06d)),
        requestAtTick = Some(800), // should overwrite tick 600
      )

      // expect correct results
      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 300.toDateTime(simulationStartDate)
          emResult.getP should equalWithTolerance(0d.asMegaWatt)
          emResult.getQ should equalWithTolerance(0d.asMegaVar)
      }

      // expect completion from EmAgent with new tick (800) instead of revoked tick (600)
      scheduler.expectMessage(
        Completion(emAgentActivation, Some(800))
      )
    }

    "handle ChangingFlexOptions indicator correctly" in {
      val resultListener = TestProbe[ResultEvent]("ResultListener")
      val scheduler = TestProbe[SchedulerMessage]("Scheduler")

      val emAgent = spawn(
        EmAgent(
          emInput,
          modelConfig,
          outputConfig,
          "PRIORITIZED",
          simulationStartDate,
          parent = Left(scheduler.ref),
          listener = Iterable(resultListener.ref),
        )
      )

      val pvAgent = TestProbe[FlexRequest]("PvAgent")
      emAgent ! RegisterParticipant(pvInput.getUuid, pvAgent.ref, pvInput)
      emAgent ! ScheduleFlexRequest(pvInput.getUuid, 0)

      val sa1 = scheduler.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe 0
      sa1.unlockKey shouldBe None
      val emAgentActivation = sa1.actor

      val evcsAgent = TestProbe[FlexRequest]("EvcsAgent")
      emAgent ! RegisterParticipant(evcsInput.getUuid, evcsAgent.ref, evcsInput)
      emAgent ! ScheduleFlexRequest(evcsInput.getUuid, 0)

      // no additional scheduling message, since tick 0 has already been scheduled
      scheduler.expectNoMessage()

      /* TICK 0 */
      emAgentActivation ! Activation(0)

      // expect flex requests
      pvAgent.expectMessage(RequestFlexOptions(0))
      evcsAgent.expectMessage(RequestFlexOptions(0))

      // send flex options
      emAgent ! ProvideMinMaxFlexOptions(
        pvInput.getUuid,
        Kilowatts(-5d),
        Kilowatts(-5d),
        Kilowatts(0d),
      )

      pvAgent.expectNoMessage()
      evcsAgent.expectNoMessage()

      emAgent ! ProvideMinMaxFlexOptions(
        evcsInput.getUuid,
        Kilowatts(2d),
        Kilowatts(-11d),
        Kilowatts(11d),
      )

      // receive flex control messages
      pvAgent.expectMessage(IssueNoControl(0))

      evcsAgent.expectMessageType[IssuePowerControl] match {
        case IssuePowerControl(0, setPower) =>
          setPower should approximate(Kilowatts(5.0))
      }

      // send completions
      emAgent ! FlexCtrlCompletion(
        pvInput.getUuid,
        ComplexPower(Kilowatts(-5d), Kilovars(-0.5d)),
        requestAtTick = Some(300),
      )

      scheduler.expectNoMessage()

      emAgent ! FlexCtrlCompletion(
        evcsInput.getUuid,
        ComplexPower(Kilowatts(5d), Kilovars(0.1d)),
        requestAtNextActivation = true, // sending ChangingFlexOptions indicator
        requestAtTick = Some(600),
      )

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe simulationStartDate
          emResult.getP should equalWithTolerance(0d.asMegaWatt)
          emResult.getQ should equalWithTolerance((-.0004d).asMegaVar)
      }

      // expect completion from EmAgent
      scheduler.expectMessage(Completion(emAgentActivation, Some(300)))

      /* TICK 300 */
      emAgentActivation ! Activation(300)

      // FLEX OPTIONS

      // expect activations and flex requests.
      // pv is scheduled regularly and evcs at any next tick
      // thus, we expect activations for both
      pvAgent.expectMessage(RequestFlexOptions(300))

      // send flex options again, now there's a cloud and thus less feed-in
      emAgent ! ProvideMinMaxFlexOptions(
        pvInput.getUuid,
        Kilowatts(-3d),
        Kilowatts(-3d),
        Kilowatts(0d),
      )

      // expecting flex options request, since we asked for it last time
      evcsAgent.expectMessage(RequestFlexOptions(300))

      emAgent ! ProvideMinMaxFlexOptions(
        evcsInput.getUuid,
        Kilowatts(2d),
        Kilowatts(-11d),
        Kilowatts(11d),
      )

      // FLEX CONTROL
      pvAgent.expectMessage(IssueNoControl(300))

      emAgent ! FlexCtrlCompletion(
        pvInput.getUuid,
        ComplexPower(Kilowatts(-3d), Kilovars(-0.06d)),
      )

      evcsAgent.expectMessageType[IssuePowerControl] match {
        case IssuePowerControl(300, setPower) =>
          setPower should approximate(Kilowatts(3.0))
      }

      scheduler.expectNoMessage()

      emAgent ! FlexCtrlCompletion(
        evcsInput.getUuid, // revoking tick 600
        ComplexPower(Kilowatts(3d), Kilovars(0.06d)),
      )

      // expect correct results
      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 300.toDateTime(simulationStartDate)
          emResult.getP should equalWithTolerance(0d.asMegaWatt)
          emResult.getQ should equalWithTolerance(0d.asMegaVar)
      }

      // no more activation, since evcs activation got revoked
      scheduler.expectMessage(Completion(emAgentActivation))
    }

  }

  "An EM-controlled EM agent" should {
    "be initialized correctly and run through some activations" in {
      val resultListener = TestProbe[ResultEvent]("ResultListener")

      val parentEmAgent = TestProbe[FlexResponse]("ParentEmAgent")

      val emAgent = spawn(
        EmAgent(
          emInput,
          modelConfig,
          outputConfig,
          "PRIORITIZED",
          simulationStartDate,
          parent = Right(parentEmAgent.ref),
          listener = Iterable(resultListener.ref),
        )
      )

      val pvAgent = TestProbe[FlexRequest]("PvAgent")
      emAgent ! RegisterParticipant(pvInput.getUuid, pvAgent.ref, pvInput)
      emAgent ! ScheduleFlexRequest(pvInput.getUuid, 0)

      val emAgentFlex =
        parentEmAgent.expectMessageType[RegisterParticipant] match {
          case RegisterParticipant(modelUuid, participant, inputModel) =>
            modelUuid shouldBe emInput.getUuid
            inputModel shouldBe emInput
            participant
        }
      parentEmAgent.expectMessage(ScheduleFlexRequest(emInput.getUuid, 0))

      val evcsAgent = TestProbe[FlexRequest]("EvcsAgent")
      emAgent ! RegisterParticipant(evcsInput.getUuid, evcsAgent.ref, evcsInput)
      emAgent ! ScheduleFlexRequest(evcsInput.getUuid, 0)

      // no additional scheduling message, since tick 0 has already been scheduled
      parentEmAgent.expectNoMessage()

      /* TICK 0 */
      emAgentFlex ! RequestFlexOptions(0)

      // expect activations and flex requests
      pvAgent.expectMessage(RequestFlexOptions(0))
      evcsAgent.expectMessage(RequestFlexOptions(0))

      // send flex options
      emAgent ! ProvideMinMaxFlexOptions(
        pvInput.getUuid,
        Kilowatts(-5d),
        Kilowatts(-5d),
        Kilowatts(0d),
      )

      pvAgent.expectNoMessage()
      evcsAgent.expectNoMessage()

      emAgent ! ProvideMinMaxFlexOptions(
        evcsInput.getUuid,
        Kilowatts(2d),
        Kilowatts(-11d),
        Kilowatts(11d),
      )

      resultListener.expectMessageType[FlexOptionsResultEvent] match {
        case FlexOptionsResultEvent(flexResult) =>
          flexResult.getInputModel shouldBe emInput.getUuid
          flexResult.getTime shouldBe 0.toDateTime(simulationStartDate)
          flexResult.getpRef() should equalWithTolerance(0d.asMegaWatt)
          flexResult.getpMin() should equalWithTolerance((-0.016d).asMegaWatt)
          flexResult.getpMax() should equalWithTolerance(0.006d.asMegaWatt)
      }

      parentEmAgent.expectMessageType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              referencePower,
              minPower,
              maxPower,
            ) =>
          modelUuid shouldBe emInput.getUuid
          referencePower shouldBe Kilowatts(0d)
          minPower shouldBe Kilowatts(-16d)
          maxPower shouldBe Kilowatts(6d) // hint: PV is not flexible
      }

      // issue power control and expect EmAgent to distribute it
      // we want max power = 6 kW
      emAgentFlex ! IssuePowerControl(0, Kilowatts(6d))

      // expect issue power control
      pvAgent.expectMessage(IssueNoControl(0))

      emAgent ! FlexCtrlCompletion(
        pvInput.getUuid,
        ComplexPower(Kilowatts(-5), Kilovars(-0.5)),
        requestAtTick = Some(600),
      )

      evcsAgent.expectMessageType[IssuePowerControl] match {
        case IssuePowerControl(0, setPower) =>
          setPower should approximate(Kilowatts(11.0))
      }

      parentEmAgent.expectNoMessage()

      emAgent ! FlexCtrlCompletion(
        evcsInput.getUuid,
        ComplexPower(Kilowatts(11), Kilovars(1.1)),
        requestAtTick = Some(300),
      )

      // expect correct results
      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 0.toDateTime(simulationStartDate)
          emResult.getP should equalWithTolerance(0.006d.asMegaWatt)
          emResult.getQ should equalWithTolerance(0.0006d.asMegaVar)
      }

      parentEmAgent.expectMessageType[FlexCtrlCompletion] match {
        case FlexCtrlCompletion(
              modelUuid,
              result,
              requestAtNextActivation,
              requestAtTick,
            ) =>
          modelUuid shouldBe emInput.getUuid
          result.p should approximate(Kilowatts(6))
          result.q should approximate(Kilovars(0.6))
          requestAtNextActivation shouldBe false
          requestAtTick shouldBe Some(300)
      }

      /* TICK 150 */
      // The mock parent EM now acts as if the situation changed before tick 300,
      // so that the flex control changes before new flex option calculations are due

      // no control means reference power of the latest flex options = 0 kW
      emAgentFlex ! IssueNoControl(150)

      // We already sent NoControl at last tick, so we're still at -5 kW
      pvAgent.expectNoMessage()

      // We need 5 kW to compensate PV feed-in
      evcsAgent.expectMessageType[IssuePowerControl] match {
        case IssuePowerControl(150, setPower) =>
          setPower should approximate(Kilowatts(5.0))
      }

      parentEmAgent.expectNoMessage()

      emAgent ! FlexCtrlCompletion(
        evcsInput.getUuid,
        ComplexPower(Kilowatts(5.0), Kilovars(0.5)),
        requestAtTick = Some(700),
      )

      // expect correct results
      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 150.toDateTime(simulationStartDate)
          emResult.getP should equalWithTolerance(0d.asMegaWatt)
          emResult.getQ should equalWithTolerance(0d.asMegaVar)
      }

      parentEmAgent.expectMessageType[FlexCtrlCompletion] match {
        case FlexCtrlCompletion(
              modelUuid,
              result,
              requestAtNextActivation,
              requestAtTick,
            ) =>
          modelUuid shouldBe emInput.getUuid
          result.p should approximate(Kilowatts(0))
          result.q should approximate(Kilovars(0))
          requestAtNextActivation shouldBe false
          requestAtTick shouldBe Some(600)
      }

    }
  }

}
