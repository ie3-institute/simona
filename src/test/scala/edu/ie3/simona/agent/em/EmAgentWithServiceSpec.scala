/*
 * © 2025. TU Dortmund University,
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
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage._
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexOptions
import edu.ie3.simona.ontology.messages.services.EmMessage
import edu.ie3.simona.ontology.messages.services.EmMessage.{
  WrappedFlexRequest,
  WrappedFlexResponse,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegisterForEmDataService
import edu.ie3.simona.test.common.input.EmInputTestData
import edu.ie3.simona.test.matchers.SquantsMatchers
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
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
import java.util.UUID

class EmAgentWithServiceSpec
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
    scaling = 1,
    uuids = List("default"),
    aggregateFlex = "SELF_OPT_EXCL_REG",
    curtailRegenerative = false,
  )

  private implicit val activePowerTolerance: Power = Kilowatts(1e-10)
  private implicit val reactivePowerTolerance: ReactivePower = Kilovars(1e-10)

  "An EM-controlled EM agent with em service" should {

    "be initialized correctly and run through some activations" in {
      val resultListener = TestProbe[ResultEvent]("ResultListener")

      val parentEmAgent = TestProbe[FlexResponse]("ParentEmAgent")

      val service = TestProbe[EmMessage]("emService")
      val serviceRef = service.ref

      val emAgent = spawn(
        EmAgent(
          emInput,
          modelConfig,
          outputConfig,
          "PRIORITIZED",
          simulationStartDate,
          parent = Right(parentEmAgent.ref),
          listener = Iterable(resultListener.ref),
          Some(serviceRef),
        )
      )

      val pvAgent = TestProbe[FlexRequest]("PvAgent")
      emAgent ! RegisterControlledAsset(pvAgent.ref, pvInput)
      emAgent ! ScheduleFlexActivation(pvInput.getUuid, INIT_SIM_TICK)

      val emAgentFlex =
        service.expectMessageType[RegisterForEmDataService] match {
          case RegisterForEmDataService(
                modelUuid,
                requestingActor,
                flexAdapter,
                parentEm,
                parentUuid,
              ) =>
            modelUuid shouldBe emInput.getUuid
            requestingActor shouldBe emAgent
            parentEm shouldBe Some(parentEmAgent.ref)
            parentUuid shouldBe None

            flexAdapter
        }

      parentEmAgent
        .expectMessageType[RegisterControlledAsset]
        .inputModel shouldBe emInput

      service.expectMessage(
        WrappedFlexResponse(
          ScheduleFlexActivation(emInput.getUuid, INIT_SIM_TICK),
          Right(parentEmAgent.ref),
        )
      )

      val evcsAgent = TestProbe[FlexRequest]("EvcsAgent")
      emAgent ! RegisterControlledAsset(evcsAgent.ref, evcsInput)
      emAgent ! ScheduleFlexActivation(evcsInput.getUuid, INIT_SIM_TICK)

      // no additional scheduling message, since tick -1 has already been scheduled
      service.expectNoMessage()

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

      service.expectNoMessage()

      emAgent ! FlexCompletion(
        modelUuid = evcsInput.getUuid,
        requestAtTick = Some(0),
      )

      // expect no results for init
      resultListener.expectNoMessage()
      // expect completion from EmAgent
      service.expectMessage(
        WrappedFlexResponse(
          FlexCompletion(
            modelUuid = emInput.getUuid,
            requestAtTick = Some(0),
          ),
          Right(parentEmAgent.ref),
        )
      )

      /* TICK 0 */
      emAgentFlex ! FlexActivation(0)

      // expect activations and flex requests
      pvAgent.expectMessage(FlexActivation(0))
      evcsAgent.expectMessage(FlexActivation(0))

      // send flex options
      emAgent ! ProvideFlexOptions(
        pvInput.getUuid,
        MinMaxFlexOptions(
          Kilowatts(-5),
          Kilowatts(-5),
          Kilowatts(0),
        ),
      )

      pvAgent.expectNoMessage()
      evcsAgent.expectNoMessage()

      emAgent ! ProvideFlexOptions(
        evcsInput.getUuid,
        MinMaxFlexOptions(
          Kilowatts(2),
          Kilowatts(-11),
          Kilowatts(11),
        ),
      )

      resultListener.expectMessageType[FlexOptionsResultEvent] match {
        case FlexOptionsResultEvent(flexResult) =>
          flexResult.getInputModel shouldBe emInput.getUuid
          flexResult.getTime shouldBe 0.toDateTime(simulationStartDate)
          flexResult.getpRef() should equalWithTolerance(0.asMegaWatt)
          flexResult.getpMin() should equalWithTolerance(-.016.asMegaWatt)
          flexResult.getpMax() should equalWithTolerance(.006.asMegaWatt)
      }

      service.expectMessageType[WrappedFlexResponse] match {
        case WrappedFlexResponse(
              ProvideFlexOptions(
                modelUuid,
                MinMaxFlexOptions(
                  referencePower,
                  minPower,
                  maxPower,
                ),
              ),
              Right(receiver),
            ) =>
          modelUuid shouldBe emInput.getUuid
          referencePower shouldBe Kilowatts(0)
          minPower shouldBe Kilowatts(-16)
          maxPower shouldBe Kilowatts(6) // hint: PV is not flexible

          receiver shouldBe parentEmAgent.ref
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

      service.expectNoMessage()

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

      service.expectMessageType[WrappedFlexResponse] match {
        case WrappedFlexResponse(
              FlexResult(modelUuid, result),
              Right(receiver),
            ) =>
          modelUuid shouldBe emInput.getUuid
          result.p should approximate(Kilowatts(6))
          result.q should approximate(Kilovars(.6))

          receiver shouldBe parentEmAgent.ref
      }

      service.expectMessage(
        WrappedFlexResponse(
          FlexCompletion(
            modelUuid = emInput.getUuid,
            requestAtTick = Some(300),
          ),
          Right(parentEmAgent.ref),
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

      service.expectNoMessage()

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

      service.expectMessageType[WrappedFlexResponse] match {
        case WrappedFlexResponse(
              FlexResult(modelUuid, result),
              Right(receiver),
            ) =>
          modelUuid shouldBe emInput.getUuid
          result.p should approximate(Kilowatts(0))
          result.q should approximate(Kilovars(0))

          receiver shouldBe parentEmAgent.ref
      }
      service.expectMessage(
        WrappedFlexResponse(
          FlexCompletion(
            modelUuid = emInput.getUuid,
            requestAtTick = Some(600),
          ),
          Right(parentEmAgent.ref),
        )
      )

    }

    "communicate with parent em through em service" in {
      val resultListener = TestProbe[ResultEvent]("ResultListener")
      val scheduler = TestProbe[SchedulerMessage]("Scheduler")

      val service = TestProbe[EmMessage]("emService")
      val serviceRef = service.ref

      val parentEmInput = emInput
        .copy()
        .uuid(UUID.randomUUID())
        .id("parent")
        .build()

      val updatedEmInput = emInput.copy().parentEm(parentEmInput).build()

      val parentEmAgent = spawn(
        EmAgent(
          parentEmInput,
          modelConfig,
          outputConfig,
          "PROPORTIONAL",
          simulationStartDate,
          parent = Left(scheduler.ref),
          listener = Iterable(resultListener.ref),
          Some(serviceRef),
        )
      )

      val parentEmAgentFlex =
        service.expectMessageType[RegisterForEmDataService] match {
          case RegisterForEmDataService(
                modelUuid,
                requestingActor,
                flexAdapter,
                parentEm,
                parentUuid,
              ) =>
            modelUuid shouldBe parentEmInput.getUuid
            requestingActor shouldBe parentEmAgent
            parentEm shouldBe None
            parentUuid shouldBe None

            flexAdapter
        }

      val emAgent = spawn(
        EmAgent(
          updatedEmInput,
          modelConfig,
          outputConfig,
          "PRIORITIZED",
          simulationStartDate,
          parent = Right(parentEmAgent),
          listener = Iterable(resultListener.ref),
          Some(serviceRef),
        )
      )

      val pvAgent = TestProbe[FlexRequest]("PvAgent")
      emAgent ! RegisterControlledAsset(pvAgent.ref, pvInput)
      emAgent ! ScheduleFlexActivation(pvInput.getUuid, INIT_SIM_TICK)

      val emAgentFlex =
        service.expectMessageType[RegisterForEmDataService] match {
          case RegisterForEmDataService(
                modelUuid,
                requestingActor,
                flexAdapter,
                parentEm,
                parentUuid,
              ) =>
            modelUuid shouldBe updatedEmInput.getUuid
            requestingActor shouldBe emAgent
            parentEm shouldBe Some(parentEmAgent)
            parentUuid shouldBe Some(parentEmInput.getUuid)

            flexAdapter
        }

      service.expectMessage(
        WrappedFlexResponse(
          ScheduleFlexActivation(updatedEmInput.getUuid, INIT_SIM_TICK),
          Right(parentEmAgent),
        )
      )

      parentEmAgent ! ScheduleFlexActivation(
        updatedEmInput.getUuid,
        INIT_SIM_TICK,
      )

      service.expectMessage(
        WrappedFlexResponse(
          ScheduleFlexActivation(parentEmInput.getUuid, INIT_SIM_TICK),
          Left(parentEmInput.getUuid),
        )
      )

      val evcsAgent = TestProbe[FlexRequest]("EvcsAgent")
      emAgent ! RegisterControlledAsset(evcsAgent.ref, evcsInput)
      emAgent ! ScheduleFlexActivation(evcsInput.getUuid, INIT_SIM_TICK)

      // no additional scheduling message, since tick -1 has already been scheduled
      service.expectNoMessage()

      /* TICK -1 */
      parentEmAgentFlex ! FlexActivation(INIT_SIM_TICK)

      service.expectMessage(
        WrappedFlexRequest(
          FlexActivation(INIT_SIM_TICK),
          emAgentFlex,
        )
      )

      emAgentFlex ! FlexActivation(INIT_SIM_TICK)

      // expect flex activations
      pvAgent.expectMessage(FlexActivation(INIT_SIM_TICK))
      evcsAgent.expectMessage(FlexActivation(INIT_SIM_TICK))

      // receive flex completions
      emAgent ! FlexCompletion(
        modelUuid = pvInput.getUuid,
        requestAtTick = Some(0),
      )

      service.expectNoMessage()

      emAgent ! FlexCompletion(
        modelUuid = evcsInput.getUuid,
        requestAtTick = Some(0),
      )

      // expect no results for init
      resultListener.expectNoMessage()
      // expect completion from EmAgent
      service.expectMessage(
        WrappedFlexResponse(
          FlexCompletion(
            modelUuid = updatedEmInput.getUuid,
            requestAtTick = Some(0),
          ),
          Right(parentEmAgent),
        )
      )

      parentEmAgent ! FlexCompletion(
        modelUuid = updatedEmInput.getUuid,
        requestAtTick = Some(0),
      )

      service.expectMessage(
        WrappedFlexResponse(
          FlexCompletion(
            modelUuid = parentEmInput.getUuid,
            requestAtTick = Some(0),
          ),
          Left(parentEmInput.getUuid),
        )
      )

      /* TICK 0 */
      parentEmAgentFlex ! FlexActivation(0)

      service.expectMessage(
        WrappedFlexRequest(
          FlexActivation(0),
          emAgentFlex,
        )
      )

      emAgentFlex ! FlexActivation(0)

      // expect activations and flex requests
      pvAgent.expectMessage(FlexActivation(0))
      evcsAgent.expectMessage(FlexActivation(0))

      // send flex options
      emAgent ! ProvideFlexOptions(
        pvInput.getUuid,
        MinMaxFlexOptions(
          Kilowatts(-5),
          Kilowatts(-5),
          Kilowatts(0),
        ),
      )

      pvAgent.expectNoMessage()
      evcsAgent.expectNoMessage()

      emAgent ! ProvideFlexOptions(
        evcsInput.getUuid,
        MinMaxFlexOptions(
          Kilowatts(2),
          Kilowatts(-11),
          Kilowatts(11),
        ),
      )

      resultListener.expectMessageType[FlexOptionsResultEvent] match {
        case FlexOptionsResultEvent(flexResult) =>
          flexResult.getInputModel shouldBe updatedEmInput.getUuid
          flexResult.getTime shouldBe 0.toDateTime(simulationStartDate)
          flexResult.getpRef() should equalWithTolerance(0.asMegaWatt)
          flexResult.getpMin() should equalWithTolerance(-.016.asMegaWatt)
          flexResult.getpMax() should equalWithTolerance(.006.asMegaWatt)
      }

      service.expectMessageType[WrappedFlexResponse] match {
        case WrappedFlexResponse(
              ProvideFlexOptions(
                modelUuid,
                MinMaxFlexOptions(
                  referencePower,
                  minPower,
                  maxPower,
                ),
              ),
              Right(receiver),
            ) =>
          modelUuid shouldBe updatedEmInput.getUuid
          referencePower shouldBe Kilowatts(0)
          minPower shouldBe Kilowatts(-16)
          maxPower shouldBe Kilowatts(6) // hint: PV is not flexible

          receiver shouldBe parentEmAgent
      }

      parentEmAgent ! ProvideFlexOptions(
        updatedEmInput.getUuid,
        MinMaxFlexOptions(
          Kilowatts(0),
          Kilowatts(-16),
          Kilowatts(6),
        ),
      )

      service.expectMessageType[WrappedFlexResponse] match {
        case WrappedFlexResponse(
              ProvideFlexOptions(
                modelUuid,
                MinMaxFlexOptions(
                  referencePower,
                  minPower,
                  maxPower,
                ),
              ),
              Left(self),
            ) =>
          modelUuid shouldBe parentEmInput.getUuid
          referencePower shouldBe Kilowatts(0)
          minPower shouldBe Kilowatts(-16)
          maxPower shouldBe Kilowatts(6) // hint: PV is not flexible

          self shouldBe parentEmInput.getUuid
      }

      parentEmAgentFlex ! IssuePowerControl(0, Kilowatts(6))

      service.expectMessage(
        WrappedFlexRequest(
          IssuePowerControl(0, Kilowatts(6)),
          emAgentFlex,
        )
      )

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

      service.expectNoMessage()

      emAgent ! FlexResult(
        modelUuid = evcsInput.getUuid,
        result = ComplexPower(Kilowatts(11), Kilovars(1.1)),
      )
      emAgent ! FlexCompletion(
        modelUuid = evcsInput.getUuid,
        requestAtTick = Some(300),
      )

      // expect correct results
      resultListener.expectMessageType[FlexOptionsResultEvent] match {
        case FlexOptionsResultEvent(flexOptionsResult) =>
          flexOptionsResult.getpRef should equalWithTolerance(0.asMegaWatt)
          flexOptionsResult.getpMin should equalWithTolerance(-0.016.asMegaWatt)
          flexOptionsResult.getpMax should equalWithTolerance(0.006.asMegaWatt)
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe updatedEmInput.getUuid
          emResult.getTime shouldBe 0.toDateTime(simulationStartDate)
          emResult.getP should equalWithTolerance(.006.asMegaWatt)
          emResult.getQ should equalWithTolerance(.0006.asMegaVar)
      }

      service.expectMessageType[WrappedFlexResponse] match {
        case WrappedFlexResponse(
              FlexResult(modelUuid, result),
              Right(receiver),
            ) =>
          modelUuid shouldBe updatedEmInput.getUuid
          result.p should approximate(Kilowatts(6))
          result.q should approximate(Kilovars(.6))

          receiver shouldBe parentEmAgent
      }

      parentEmAgent ! FlexResult(
        updatedEmInput.getUuid,
        ComplexPower(
          Kilowatts(6),
          Kilovars(.6),
        ),
      )

      service.expectMessage(
        WrappedFlexResponse(
          FlexCompletion(
            modelUuid = updatedEmInput.getUuid,
            requestAtTick = Some(300),
          ),
          Right(parentEmAgent),
        )
      )

      parentEmAgentFlex ! IssueNoControl(150)

      // TODO: FIX
      // service.expectMessage(WrappedFlexRequest(IssueNoControl(150), emAgentFlex))

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

      service.expectNoMessage()

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
          emResult.getInputModel shouldBe updatedEmInput.getUuid
          emResult.getTime shouldBe 150.toDateTime(simulationStartDate)
          emResult.getP should equalWithTolerance(0.asMegaWatt)
          emResult.getQ should equalWithTolerance(0.asMegaVar)
      }

      service.expectMessageType[WrappedFlexResponse] match {
        case WrappedFlexResponse(
              FlexResult(modelUuid, result),
              Right(receiver),
            ) =>
          modelUuid shouldBe updatedEmInput.getUuid
          result.p should approximate(Kilowatts(0))
          result.q should approximate(Kilovars(0))

          receiver shouldBe parentEmAgent
      }

      parentEmAgent ! FlexResult(
        updatedEmInput.getUuid,
        ComplexPower(Kilowatts(0), Kilovars(0)),
      )

      service.expectMessage(
        WrappedFlexResponse(
          FlexCompletion(
            modelUuid = updatedEmInput.getUuid,
            requestAtTick = Some(600),
          ),
          Right(parentEmAgent),
        )
      )
    }
  }

}
