/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.em

import edu.ie3.datamodel.models.result.system.EmResult
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPower
import edu.ie3.simona.config.RuntimeConfig.EmRuntimeConfig
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.{
  FlexOptionsResultEvent,
  ParticipantResultEvent,
}
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage._
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.test.common.input.EmInputTestData
import edu.ie3.simona.test.matchers.{QuantityMatchers, SquantsMatchers}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.TimeUtil
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
    with QuantityMatchers
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
    scaling = 1,
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
      emAgent ! RegisterControlledAsset(pvAgent.ref, pvInput)
      emAgent ! ScheduleFlexActivation(pvInput.getUuid, INIT_SIM_TICK)

      val sa1 = scheduler.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe INIT_SIM_TICK
      sa1.unlockKey shouldBe None
      val emAgentActivation = sa1.actor

      val evcsAgent = TestProbe[FlexRequest]("EvcsAgent")
      emAgent ! RegisterControlledAsset(evcsAgent.ref, evcsInput)
      emAgent ! ScheduleFlexActivation(evcsInput.getUuid, INIT_SIM_TICK)

      // no additional scheduling message, since tick -1 has already been scheduled
      scheduler.expectNoMessage()

      /* TICK -1 */
      emAgentActivation ! Activation(INIT_SIM_TICK)

      // expect flex activations
      pvAgent.expectMessage(FlexActivation(INIT_SIM_TICK))
      evcsAgent.expectMessage(FlexActivation(INIT_SIM_TICK))

      // receive flex completions
      emAgent ! FlexCompletion(
        modelUuid = pvInput.getUuid,
        requestAtTick = Some(0),
      )

      scheduler.expectNoMessage()

      emAgent ! FlexCompletion(
        modelUuid = evcsInput.getUuid,
        requestAtTick = Some(0),
      )

      // expect no results for init
      resultListener.expectNoMessage()
      // expect completion from EmAgent
      scheduler.expectMessage(
        Completion(emAgentActivation, Some(0))
      )

      /* TICK 0 */
      emAgentActivation ! Activation(0)

      // expect flex activations
      pvAgent.expectMessage(FlexActivation(0))
      evcsAgent.expectMessage(FlexActivation(0))

      // send flex options
      emAgent ! ProvideMinMaxFlexOptions(
        pvInput.getUuid,
        Kilowatts(-5),
        Kilowatts(-5),
        Kilowatts(0),
      )

      pvAgent.expectNoMessage()
      evcsAgent.expectNoMessage()

      emAgent ! ProvideMinMaxFlexOptions(
        evcsInput.getUuid,
        Kilowatts(2),
        Kilowatts(-11),
        Kilowatts(11),
      )

      // receive flex control messages
      pvAgent.expectMessage(IssueNoControl(0))
      emAgent ! FlexResult(
        modelUuid = pvInput.getUuid,
        result = ComplexPower(Kilowatts(-5), Kilovars(-.5)),
      )
      emAgent ! FlexCompletion(
        modelUuid = pvInput.getUuid,
        requestAtTick = Some(600),
      )

      scheduler.expectNoMessage()

      evcsAgent.expectMessageType[IssuePowerControl] match {
        case IssuePowerControl(0, setPower) =>
          setPower should approximate(Kilowatts(5.0))
      }
      emAgent ! FlexResult(
        modelUuid = evcsInput.getUuid,
        result = ComplexPower(Kilowatts(5), Kilovars(.1)),
      )
      emAgent ! FlexCompletion(
        modelUuid = evcsInput.getUuid,
        requestAtTick = Some(300),
      )

      // expect correct results
      resultListener.expectMessageType[FlexOptionsResultEvent] match {
        case FlexOptionsResultEvent(flexResult) =>
          flexResult.getInputModel shouldBe emInput.getUuid
          flexResult.getTime shouldBe 0.toDateTime(simulationStartDate)
          flexResult.getpRef() should equalWithTolerance(0.asMegaWatt)
          flexResult.getpMin() should equalWithTolerance(-.016.asMegaWatt)
          flexResult.getpMax() should equalWithTolerance(.006.asMegaWatt)
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe simulationStartDate
          emResult.getP should equalWithTolerance(0.asMegaWatt)
          emResult.getQ should equalWithTolerance(-.0004.asMegaVar)
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

      evcsAgent.expectMessage(FlexActivation(300))

      // send flex options again, ev is fully charged
      emAgent ! ProvideMinMaxFlexOptions(
        evcsInput.getUuid,
        Kilowatts(0),
        Kilowatts(-11),
        Kilowatts(0),
      )

      // receive flex control messages
      evcsAgent.expectMessage(IssueNoControl(300))

      pvAgent.expectNoMessage()

      emAgent ! FlexResult(
        modelUuid = evcsInput.getUuid,
        result = ComplexPower(Kilowatts(0), Kilovars(0)),
      )
      emAgent ! FlexCompletion(modelUuid = evcsInput.getUuid)

      // expect correct results
      resultListener.expectMessageType[FlexOptionsResultEvent] match {
        case FlexOptionsResultEvent(flexResult) =>
          flexResult.getInputModel shouldBe emInput.getUuid
          flexResult.getTime shouldBe 300.toDateTime(simulationStartDate)
          flexResult.getpRef() should equalWithTolerance(-.005.asMegaWatt)
          flexResult.getpMin() should equalWithTolerance(-.016.asMegaWatt)
          flexResult.getpMax() should equalWithTolerance(-.005.asMegaWatt)
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 300.toDateTime(simulationStartDate)
          emResult.getP should equalWithTolerance(-.005.asMegaWatt)
          emResult.getQ should equalWithTolerance(-.0005.asMegaVar)
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
      emAgent ! RegisterControlledAsset(pvAgent.ref, pvInput)
      emAgent ! ScheduleFlexActivation(pvInput.getUuid, 0)

      val sa1 = scheduler.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe 0
      sa1.unlockKey shouldBe None
      val emAgentActivation = sa1.actor

      val evcsAgent = TestProbe[FlexRequest]("EvcsAgent")
      emAgent ! RegisterControlledAsset(evcsAgent.ref, evcsInput)
      emAgent ! ScheduleFlexActivation(evcsInput.getUuid, 0)

      // no additional scheduling message, since tick 0 has already been scheduled
      scheduler.expectNoMessage()

      // We skip initialization here for simplicity

      /* TICK 0 */
      emAgentActivation ! Activation(0)

      // expect flex activations
      pvAgent.expectMessage(FlexActivation(0))
      evcsAgent.expectMessage(FlexActivation(0))

      // send flex options
      emAgent ! ProvideMinMaxFlexOptions(
        pvInput.getUuid,
        Kilowatts(-5),
        Kilowatts(-5),
        Kilowatts(0),
      )

      pvAgent.expectNoMessage()
      evcsAgent.expectNoMessage()

      emAgent ! ProvideMinMaxFlexOptions(
        evcsInput.getUuid,
        Kilowatts(2),
        Kilowatts(-11),
        Kilowatts(11),
      )

      // receive flex control messages
      pvAgent.expectMessage(IssueNoControl(0))
      evcsAgent.expectMessageType[IssuePowerControl] match {
        case IssuePowerControl(0, setPower) =>
          setPower should approximate(Kilowatts(5.0))
      }

      // send completions
      emAgent ! FlexResult(
        modelUuid = pvInput.getUuid,
        result = ComplexPower(Kilowatts(-5), Kilovars(-.5)),
      )
      emAgent ! FlexCompletion(
        modelUuid = pvInput.getUuid,
        requestAtTick = Some(300),
      )

      emAgent ! FlexResult(
        modelUuid = evcsInput.getUuid,
        result = ComplexPower(Kilowatts(5), Kilovars(.1)),
      )

      scheduler.expectNoMessage()

      emAgent ! FlexCompletion(
        modelUuid = evcsInput.getUuid,
        requestAtTick = Some(600),
      )

      // expect correct results
      resultListener.expectMessageType[FlexOptionsResultEvent] match {
        case FlexOptionsResultEvent(flexResult) =>
          flexResult.getInputModel shouldBe emInput.getUuid
          flexResult.getTime shouldBe 0.toDateTime(simulationStartDate)
          flexResult.getpRef() should equalWithTolerance(0.asMegaWatt)
          flexResult.getpMin() should equalWithTolerance(-.016.asMegaWatt)
          flexResult.getpMax() should equalWithTolerance(.006.asMegaWatt)
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe simulationStartDate
          emResult.getP should equalWithTolerance(0.asMegaWatt)
          emResult.getQ should equalWithTolerance(-.0004.asMegaVar)
      }

      // expect completion from EmAgent
      scheduler.expectMessage(Completion(emAgentActivation, Some(300)))

      /* TICK 300 */
      emAgentActivation ! Activation(300)

      // expect activations and flex activations.
      // only pv agent has been scheduled for this tick,
      // thus evcs does not get activated
      evcsAgent.expectNoMessage()

      pvAgent.expectMessage(FlexActivation(300))

      // send flex options again, now there's a cloud and thus less feed-in
      emAgent ! ProvideMinMaxFlexOptions(
        pvInput.getUuid,
        Kilowatts(-3),
        Kilowatts(-3),
        Kilowatts(0),
      )

      // receive flex control messages
      pvAgent.expectMessage(IssueNoControl(300))

      emAgent ! FlexResult(
        modelUuid = pvInput.getUuid,
        result = ComplexPower(Kilowatts(-3), Kilovars(-.06)),
      )

      emAgent ! FlexCompletion(
        modelUuid = pvInput.getUuid
      )

      // evcs is now sent control too
      evcsAgent.expectMessageType[IssuePowerControl] match {
        case IssuePowerControl(300, setPower) =>
          setPower should approximate(Kilowatts(3.0))
      }

      scheduler.expectNoMessage()

      emAgent ! FlexResult(
        modelUuid = evcsInput.getUuid,
        result = ComplexPower(Kilowatts(3), Kilovars(.06)),
      )
      emAgent ! FlexCompletion(
        modelUuid = evcsInput.getUuid,
        requestAtTick = Some(800), // should overwrite tick 600
      )

      // expect correct results
      resultListener.expectMessageType[FlexOptionsResultEvent] match {
        case FlexOptionsResultEvent(flexResult) =>
          flexResult.getInputModel shouldBe emInput.getUuid
          flexResult.getTime shouldBe 300.toDateTime(simulationStartDate)
          flexResult.getpRef() should equalWithTolerance(0.asMegaWatt)
          flexResult.getpMin() should equalWithTolerance(-.014.asMegaWatt)
          flexResult.getpMax() should equalWithTolerance(.008.asMegaWatt)
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 300.toDateTime(simulationStartDate)
          emResult.getP should equalWithTolerance(0.asMegaWatt)
          emResult.getQ should equalWithTolerance(0.asMegaVar)
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
      emAgent ! RegisterControlledAsset(pvAgent.ref, pvInput)
      emAgent ! ScheduleFlexActivation(pvInput.getUuid, 0)

      val sa1 = scheduler.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe 0
      sa1.unlockKey shouldBe None
      val emAgentActivation = sa1.actor

      val evcsAgent = TestProbe[FlexRequest]("EvcsAgent")
      emAgent ! RegisterControlledAsset(evcsAgent.ref, evcsInput)
      emAgent ! ScheduleFlexActivation(evcsInput.getUuid, 0)

      // no additional scheduling message, since tick 0 has already been scheduled
      scheduler.expectNoMessage()

      // We skip initialization here for simplicity

      /* TICK 0 */
      emAgentActivation ! Activation(0)

      // expect flex activations
      pvAgent.expectMessage(FlexActivation(0))
      evcsAgent.expectMessage(FlexActivation(0))

      // send flex options
      emAgent ! ProvideMinMaxFlexOptions(
        pvInput.getUuid,
        Kilowatts(-5),
        Kilowatts(-5),
        Kilowatts(0),
      )

      pvAgent.expectNoMessage()
      evcsAgent.expectNoMessage()

      emAgent ! ProvideMinMaxFlexOptions(
        evcsInput.getUuid,
        Kilowatts(2),
        Kilowatts(-11),
        Kilowatts(11),
      )

      // receive flex control messages
      pvAgent.expectMessage(IssueNoControl(0))

      evcsAgent.expectMessageType[IssuePowerControl] match {
        case IssuePowerControl(0, setPower) =>
          setPower should approximate(Kilowatts(5.0))
      }

      // send completions
      emAgent ! FlexResult(
        modelUuid = pvInput.getUuid,
        result = ComplexPower(Kilowatts(-5), Kilovars(-.5)),
      )
      emAgent ! FlexCompletion(
        modelUuid = pvInput.getUuid,
        requestAtTick = Some(300),
      )

      emAgent ! FlexResult(
        modelUuid = evcsInput.getUuid,
        result = ComplexPower(Kilowatts(5), Kilovars(.1)),
      )

      scheduler.expectNoMessage()

      emAgent ! FlexCompletion(
        modelUuid = evcsInput.getUuid,
        requestAtNextActivation = true, // sending ChangingFlexOptions indicator
        requestAtTick = Some(600),
      )

      resultListener.expectMessageType[FlexOptionsResultEvent] match {
        case FlexOptionsResultEvent(flexResult) =>
          flexResult.getInputModel shouldBe emInput.getUuid
          flexResult.getTime shouldBe 0.toDateTime(simulationStartDate)
          flexResult.getpRef() should equalWithTolerance(0.asMegaWatt)
          flexResult.getpMin() should equalWithTolerance(-.016.asMegaWatt)
          flexResult.getpMax() should equalWithTolerance(.006.asMegaWatt)
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 0.toDateTime(simulationStartDate)
          emResult.getP should equalWithTolerance(0.asMegaWatt)
          emResult.getQ should equalWithTolerance(-.0004.asMegaVar)
      }

      // expect completion from EmAgent
      scheduler.expectMessage(Completion(emAgentActivation, Some(300)))

      /* TICK 300 */
      emAgentActivation ! Activation(300)

      // FLEX OPTIONS

      // expect activations and flex activations.
      // pv is scheduled regularly and evcs at any next tick
      // thus, we expect activations for both
      pvAgent.expectMessage(FlexActivation(300))

      // send flex options again, now there's a cloud and thus less feed-in
      emAgent ! ProvideMinMaxFlexOptions(
        pvInput.getUuid,
        Kilowatts(-3),
        Kilowatts(-3),
        Kilowatts(0),
      )

      // expecting flex options request, since we asked for it last time
      evcsAgent.expectMessage(FlexActivation(300))

      emAgent ! ProvideMinMaxFlexOptions(
        evcsInput.getUuid,
        Kilowatts(2),
        Kilowatts(-11),
        Kilowatts(11),
      )

      // FLEX CONTROL
      pvAgent.expectMessage(IssueNoControl(300))

      emAgent ! FlexResult(
        modelUuid = pvInput.getUuid,
        result = ComplexPower(Kilowatts(-3), Kilovars(-.06)),
      )
      emAgent ! FlexCompletion(
        modelUuid = pvInput.getUuid
      )

      evcsAgent.expectMessageType[IssuePowerControl] match {
        case IssuePowerControl(300, setPower) =>
          setPower should approximate(Kilowatts(3.0))
      }

      scheduler.expectNoMessage()

      emAgent ! FlexResult(
        modelUuid = evcsInput.getUuid,
        result = ComplexPower(Kilowatts(3), Kilovars(.06)),
      )
      emAgent ! FlexCompletion(
        modelUuid = evcsInput.getUuid // revoking tick 600
      )

      // expect correct results
      resultListener.expectMessageType[FlexOptionsResultEvent] match {
        case FlexOptionsResultEvent(flexResult) =>
          flexResult.getInputModel shouldBe emInput.getUuid
          flexResult.getTime shouldBe 300.toDateTime(simulationStartDate)
          flexResult.getpRef() should equalWithTolerance(0.asMegaWatt)
          flexResult.getpMin() should equalWithTolerance(-.014.asMegaWatt)
          flexResult.getpMax() should equalWithTolerance(.008.asMegaWatt)
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 300.toDateTime(simulationStartDate)
          emResult.getP should equalWithTolerance(0.asMegaWatt)
          emResult.getQ should equalWithTolerance(0.asMegaVar)
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
      emAgent ! RegisterControlledAsset(pvAgent.ref, pvInput)
      emAgent ! ScheduleFlexActivation(pvInput.getUuid, INIT_SIM_TICK)

      val emAgentFlex =
        parentEmAgent.expectMessageType[RegisterControlledAsset] match {
          case RegisterControlledAsset(participant, inputModel) =>
            inputModel shouldBe emInput
            participant
        }
      parentEmAgent.expectMessage(
        ScheduleFlexActivation(emInput.getUuid, INIT_SIM_TICK)
      )

      val evcsAgent = TestProbe[FlexRequest]("EvcsAgent")
      emAgent ! RegisterControlledAsset(evcsAgent.ref, evcsInput)
      emAgent ! ScheduleFlexActivation(evcsInput.getUuid, INIT_SIM_TICK)

      // no additional scheduling message, since tick -1 has already been scheduled
      parentEmAgent.expectNoMessage()

      /* TICK -1 */
      emAgentFlex ! FlexActivation(INIT_SIM_TICK)

      // expect flex activations
      pvAgent.expectMessage(FlexActivation(INIT_SIM_TICK))
      evcsAgent.expectMessage(FlexActivation(INIT_SIM_TICK))

      // receive flex completions
      emAgent ! FlexCompletion(
        modelUuid = pvInput.getUuid,
        requestAtTick = Some(0),
      )

      parentEmAgent.expectNoMessage()

      emAgent ! FlexCompletion(
        modelUuid = evcsInput.getUuid,
        requestAtTick = Some(0),
      )

      // expect no results for init
      resultListener.expectNoMessage()
      // expect completion from EmAgent
      parentEmAgent.expectMessage(
        FlexCompletion(
          modelUuid = emInput.getUuid,
          requestAtTick = Some(0),
        )
      )

      /* TICK 0 */
      emAgentFlex ! FlexActivation(0)

      // expect activations and flex requests
      pvAgent.expectMessage(FlexActivation(0))
      evcsAgent.expectMessage(FlexActivation(0))

      // send flex options
      emAgent ! ProvideMinMaxFlexOptions(
        pvInput.getUuid,
        Kilowatts(-5),
        Kilowatts(-5),
        Kilowatts(0),
      )

      pvAgent.expectNoMessage()
      evcsAgent.expectNoMessage()

      emAgent ! ProvideMinMaxFlexOptions(
        evcsInput.getUuid,
        Kilowatts(2),
        Kilowatts(-11),
        Kilowatts(11),
      )

      resultListener.expectMessageType[FlexOptionsResultEvent] match {
        case FlexOptionsResultEvent(flexResult) =>
          flexResult.getInputModel shouldBe emInput.getUuid
          flexResult.getTime shouldBe 0.toDateTime(simulationStartDate)
          flexResult.getpRef() should equalWithTolerance(0.asMegaWatt)
          flexResult.getpMin() should equalWithTolerance(-.016.asMegaWatt)
          flexResult.getpMax() should equalWithTolerance(.006.asMegaWatt)
      }

      parentEmAgent.expectMessageType[ProvideFlexOptions] match {
        case ProvideMinMaxFlexOptions(
              modelUuid,
              referencePower,
              minPower,
              maxPower,
            ) =>
          modelUuid shouldBe emInput.getUuid
          referencePower shouldBe Kilowatts(0)
          minPower shouldBe Kilowatts(-16)
          maxPower shouldBe Kilowatts(6) // hint: PV is not flexible
      }

      // issue power control and expect EmAgent to distribute it
      // we want max power = 6 kW
      emAgentFlex ! IssuePowerControl(0, Kilowatts(6))

      // expect issue power control
      pvAgent.expectMessage(IssueNoControl(0))

      emAgent ! FlexResult(
        modelUuid = pvInput.getUuid,
        result = ComplexPower(Kilowatts(-5), Kilovars(-.5)),
      )
      emAgent ! FlexCompletion(
        modelUuid = pvInput.getUuid,
        requestAtTick = Some(600),
      )

      evcsAgent.expectMessageType[IssuePowerControl] match {
        case IssuePowerControl(0, setPower) =>
          setPower should approximate(Kilowatts(11.0))
      }

      parentEmAgent.expectNoMessage()

      emAgent ! FlexResult(
        modelUuid = evcsInput.getUuid,
        result = ComplexPower(Kilowatts(11), Kilovars(1.1)),
      )
      emAgent ! FlexCompletion(
        modelUuid = evcsInput.getUuid,
        requestAtTick = Some(300),
      )

      // expect correct results
      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 0.toDateTime(simulationStartDate)
          emResult.getP should equalWithTolerance(.006.asMegaWatt)
          emResult.getQ should equalWithTolerance(.0006.asMegaVar)
      }

      parentEmAgent.expectMessageType[FlexResult] match {
        case FlexResult(modelUuid, result) =>
          modelUuid shouldBe emInput.getUuid
          result.p should approximate(Kilowatts(6))
          result.q should approximate(Kilovars(.6))
      }

      parentEmAgent.expectMessage(
        FlexCompletion(
          modelUuid = emInput.getUuid,
          requestAtTick = Some(300),
        )
      )

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

      emAgent ! FlexResult(
        modelUuid = evcsInput.getUuid,
        result = ComplexPower(Kilowatts(5.0), Kilovars(.5)),
      )
      emAgent ! FlexCompletion(
        modelUuid = evcsInput.getUuid,
        requestAtTick = Some(700),
      )

      // expect correct results
      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 150.toDateTime(simulationStartDate)
          emResult.getP should equalWithTolerance(0.asMegaWatt)
          emResult.getQ should equalWithTolerance(0.asMegaVar)
      }

      parentEmAgent.expectMessageType[FlexResult] match {
        case FlexResult(modelUuid, result) =>
          modelUuid shouldBe emInput.getUuid
          result.p should approximate(Kilowatts(0))
          result.q should approximate(Kilovars(0))
      }
      parentEmAgent.expectMessage(
        FlexCompletion(
          modelUuid = emInput.getUuid,
          requestAtTick = Some(600),
        )
      )

    }
  }

}
