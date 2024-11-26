/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant2

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.GridAgentMessages.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
}
import edu.ie3.simona.agent.participant2.MockParticipantModel.{
  MockRequestMessage,
  MockResponseMessage,
  MockResult,
}
import edu.ie3.simona.agent.participant2.ParticipantAgent.{
  FinishParticipantSimulation,
  RequestAssetPowerMessage,
}
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.model.participant2.ParticipantModelShell
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexActivation,
  FlexCompletion,
  FlexRequest,
  FlexResponse,
  IssueNoControl,
  IssuePowerControl,
}
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.{Kilovars, ReactivePower}
import org.apache.pekko.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.apache.pekko.actor.typed.ActorRef
import squants.{Each, Power}
import squants.energy.Kilowatts

import java.time.ZonedDateTime

/** Test for [[ParticipantAgent]] and [[ParticipantModelShell]] using a mock
  * participant [[MockParticipantModel]]
  */
class ParticipantAgentSpec extends ScalaTestWithActorTestKit with UnitSpec {

  private implicit val simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z")

  private implicit val activePowerTolerance: Power = Kilowatts(1e-10)
  private implicit val reactivePowerTolerance: ReactivePower = Kilovars(1e-10)

  "A ParticipantAgent without secondary services" when {

    "not flex-controlled" should {

      "calculate operating point and results correctly with no additional ticks" in {

        val scheduler = createTestProbe[SchedulerMessage]()
        val gridAgent = createTestProbe[GridAgent.Request]()
        val resultListener = createTestProbe[ResultEvent]()
        val responseReceiver = createTestProbe[MockResponseMessage.type]()

        // receiving the activation adapter
        val receiveAdapter = createTestProbe[ActorRef[Activation]]()

        // no additional activation ticks
        val model = new MockParticipantModel(mockActivationTicks = Map.empty)
        val operationInterval = OperationInterval(8 * 3600, 20 * 3600)

        val participantAgent = spawn(
          ParticipantAgentMockFactory.create(
            ParticipantModelShell(
              model,
              operationInterval,
              simulationStartDate,
            ),
            ParticipantInputHandler(
              Map.empty
            ),
            ParticipantGridAdapter(
              gridAgent.ref,
              12 * 3600,
            ),
            Iterable(resultListener.ref),
            Left(scheduler.ref, receiveAdapter.ref),
          )
        )
        val activationRef =
          receiveAdapter.expectMessageType[ActorRef[Activation]]

        // TICK 0: Outside of operation interval

        participantAgent ! MockRequestMessage(0, responseReceiver.ref)
        responseReceiver.expectMessage(MockResponseMessage)

        activationRef ! Activation(0)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe simulationStartDate
            result.getP should equalWithTolerance(0.0.asMegaWatt)
            result.getQ should equalWithTolerance(0.0.asMegaVar)
        }

        scheduler.expectMessage(
          Completion(activationRef, Some(operationInterval.start))
        )

        // TICK 8 * 3600: Start of operation interval

        participantAgent ! MockRequestMessage(0, responseReceiver.ref)
        responseReceiver.expectMessage(MockResponseMessage)

        activationRef ! Activation(operationInterval.start)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.start.toDateTime
            result.getP should equalWithTolerance(0.006.asMegaWatt)
            result.getQ should equalWithTolerance(0.00290593262.asMegaVar)
        }

        scheduler.expectMessage(
          Completion(activationRef, Some(operationInterval.end))
        )

        // TICK 12 * 3600: GridAgent requests power

        participantAgent ! MockRequestMessage(0, responseReceiver.ref)
        responseReceiver.expectMessage(MockResponseMessage)

        // first request
        participantAgent ! RequestAssetPowerMessage(12 * 3600, Each(1), Each(0))

        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(2))
            q should approximate(Kilovars(0.968644209676))
        }

        // second request with same voltage
        participantAgent ! RequestAssetPowerMessage(12 * 3600, Each(1), Each(0))

        gridAgent.expectMessageType[AssetPowerUnchangedMessage] match {
          case AssetPowerUnchangedMessage(p, q) =>
            p should approximate(Kilowatts(2))
            q should approximate(Kilovars(0.968644209676))
        }

        // third request with different voltage
        participantAgent ! RequestAssetPowerMessage(
          12 * 3600,
          Each(0.98),
          Each(0),
        )

        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(2))
            // not voltage dependent
            q should approximate(Kilovars(0.968644209676))
        }

        participantAgent ! FinishParticipantSimulation(12 * 3600, 24 * 3600)

        // TICK 20 * 3600: Outside of operation interval (last tick)

        participantAgent ! MockRequestMessage(0, responseReceiver.ref)
        responseReceiver.expectMessage(MockResponseMessage)

        activationRef ! Activation(operationInterval.end)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.end.toDateTime
            result.getP should equalWithTolerance(0.0.asMegaWatt)
            result.getQ should equalWithTolerance(0.0.asMegaVar)
        }

        scheduler.expectMessage(Completion(activationRef))

        // TICK 24 * 3600: GridAgent requests power

        participantAgent ! MockRequestMessage(0, responseReceiver.ref)
        responseReceiver.expectMessage(MockResponseMessage)

        participantAgent ! RequestAssetPowerMessage(24 * 3600, Each(1), Each(0))

        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(4))
            q should approximate(Kilovars(1.93728841935))
        }

        participantAgent ! FinishParticipantSimulation(24 * 3600, 36 * 3600)

      }

      "calculate operating point and results correctly with additional ticks" in {

        val scheduler = createTestProbe[SchedulerMessage]()
        val gridAgent = createTestProbe[GridAgent.Request]()
        val resultListener = createTestProbe[ResultEvent]()

        // receiving the activation adapter
        val receiveAdapter = createTestProbe[ActorRef[Activation]]()

        // no additional activation ticks
        val model = new MockParticipantModel(mockActivationTicks =
          Map(
            0 * 3600L -> 4 * 3600L, // still before operation, is ignored
            8 * 3600L -> 12 * 3600L, // middle of operation
            12 * 3600L -> 22 * 3600L, // after operation, is ignored
          )
        )
        val operationInterval = OperationInterval(8 * 3600, 20 * 3600)

        val participantAgent = spawn(
          ParticipantAgentMockFactory.create(
            ParticipantModelShell(
              model,
              operationInterval,
              simulationStartDate,
            ),
            ParticipantInputHandler(
              Map.empty
            ),
            ParticipantGridAdapter(
              gridAgent.ref,
              12 * 3600,
            ),
            Iterable(resultListener.ref),
            Left(scheduler.ref, receiveAdapter.ref),
          )
        )
        val activationRef =
          receiveAdapter.expectMessageType[ActorRef[Activation]]

        // TICK 0: Outside of operation interval

        activationRef ! Activation(0)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe simulationStartDate
            result.getP should equalWithTolerance(0.0.asMegaWatt)
            result.getQ should equalWithTolerance(0.0.asMegaVar)
        }

        scheduler.expectMessage(
          Completion(activationRef, Some(operationInterval.start))
        )

        // TICK 8 * 3600: Start of operation interval

        activationRef ! Activation(operationInterval.start)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.start.toDateTime
            result.getP should equalWithTolerance(0.006.asMegaWatt)
            result.getQ should equalWithTolerance(0.00290593262.asMegaVar)
        }

        scheduler.expectMessage(
          Completion(activationRef, Some(12 * 3600))
        )

        // TICK 12 * 3600: Middle of operation interval and GridAgent requests power

        activationRef ! Activation(12 * 3600)

        participantAgent ! RequestAssetPowerMessage(12 * 3600, Each(1), Each(0))

        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(2))
            q should approximate(Kilovars(0.968644209676))
        }

        resultListener.expectNoMessage()
        scheduler.expectNoMessage()

        participantAgent ! FinishParticipantSimulation(12 * 3600, 24 * 3600)

        // calculation should start now
        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe (12 * 3600).toDateTime
            result.getP should equalWithTolerance(0.006.asMegaWatt)
            result.getQ should equalWithTolerance(0.00290593262.asMegaVar)
        }

        scheduler.expectMessage(
          Completion(activationRef, Some(operationInterval.end))
        )

        // TICK 20 * 3600: Outside of operation interval (last tick)

        activationRef ! Activation(operationInterval.end)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.end.toDateTime
            result.getP should equalWithTolerance(0.0.asMegaWatt)
            result.getQ should equalWithTolerance(0.0.asMegaVar)
        }

        scheduler.expectMessage(Completion(activationRef))

        // TICK 24 * 3600: GridAgent requests power

        participantAgent ! RequestAssetPowerMessage(24 * 3600, Each(1), Each(0))

        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(4))
            q should approximate(Kilovars(1.93728841935))
        }

        participantAgent ! FinishParticipantSimulation(24 * 3600, 36 * 3600)

      }

    }

    "flex-controlled" should {

      "calculate operating point and results correctly with no additional ticks" in {

        val em = createTestProbe[FlexResponse]()
        val gridAgent = createTestProbe[GridAgent.Request]()
        val resultListener = createTestProbe[ResultEvent]()

        // receiving the activation adapter
        val receiveAdapter = createTestProbe[ActorRef[FlexRequest]]()

        // no additional activation ticks
        val model = new MockParticipantModel(mockActivationTicks = Map.empty)
        val operationInterval = OperationInterval(8 * 3600, 20 * 3600)

        val participantAgent = spawn(
          ParticipantAgentMockFactory.create(
            ParticipantModelShell(
              model,
              operationInterval,
              simulationStartDate,
            ),
            ParticipantInputHandler(
              Map.empty
            ),
            ParticipantGridAdapter(
              gridAgent.ref,
              12 * 3600,
            ),
            Iterable(resultListener.ref),
            Right(em.ref, receiveAdapter.ref),
          )
        )
        val flexRef = receiveAdapter.expectMessageType[ActorRef[FlexRequest]]

        // TICK 0: Outside of operation interval

        flexRef ! FlexActivation(0)

        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(0))
            min should approximate(Kilowatts(0))
            max should approximate(Kilowatts(0))
        }

        flexRef ! IssueNoControl(0)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe simulationStartDate
            result.getP should equalWithTolerance(0.0.asMegaWatt)
            result.getQ should equalWithTolerance(0.0.asMegaVar)
        }

        em.expectMessage(
          FlexCompletion(
            model.uuid,
            requestAtTick = Some(operationInterval.start),
          )
        )

        // TICK 8 * 3600: Start of operation interval

        flexRef ! FlexActivation(operationInterval.start)

        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(1))
            min should approximate(Kilowatts(-1))
            max should approximate(Kilowatts(3))
        }

        flexRef ! IssuePowerControl(operationInterval.start, Kilowatts(3))

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.start.toDateTime
            result.getP should equalWithTolerance(0.003.asMegaWatt)
            result.getQ should equalWithTolerance(0.0014529663.asMegaVar)
        }

        em.expectMessage(
          FlexCompletion(
            model.uuid,
            requestAtTick = Some(operationInterval.end),
          )
        )

        // TICK 12 * 3600: GridAgent requests power

        participantAgent ! RequestAssetPowerMessage(12 * 3600, Each(1), Each(0))

        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(1))
            q should approximate(Kilovars(0.48432210483))
        }

        participantAgent ! FinishParticipantSimulation(12 * 3600, 24 * 3600)

        // TICK 20 * 3600: Outside of operation interval (last tick)

        flexRef ! FlexActivation(operationInterval.end)

        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(0))
            min should approximate(Kilowatts(0))
            max should approximate(Kilowatts(0))
        }

        flexRef ! IssueNoControl(operationInterval.end)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.end.toDateTime
            result.getP should equalWithTolerance(0.0.asMegaWatt)
            result.getQ should equalWithTolerance(0.0.asMegaVar)
        }

        em.expectMessage(FlexCompletion(model.uuid))

        // TICK 24 * 3600: GridAgent requests power

        participantAgent ! RequestAssetPowerMessage(24 * 3600, Each(1), Each(0))

        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(2))
            q should approximate(Kilovars(0.96864420966))
        }

        participantAgent ! FinishParticipantSimulation(24 * 3600, 36 * 3600)

      }

      "calculate operating point and results correctly with additional ticks" in {

        val em = createTestProbe[FlexResponse]()
        val gridAgent = createTestProbe[GridAgent.Request]()
        val resultListener = createTestProbe[ResultEvent]()

        // receiving the activation adapter
        val receiveAdapter = createTestProbe[ActorRef[FlexRequest]]()

        // no additional activation ticks
        val model = new MockParticipantModel(mockActivationTicks =
          Map(
            0 * 3600L -> 4 * 3600L, // still before operation, is ignored
            8 * 3600L -> 12 * 3600L, // middle of operation
            12 * 3600L -> 22 * 3600L, // after operation, is ignored
          )
        )
        val operationInterval = OperationInterval(8 * 3600, 20 * 3600)

        val participantAgent = spawn(
          ParticipantAgentMockFactory.create(
            ParticipantModelShell(
              model,
              operationInterval,
              simulationStartDate,
            ),
            ParticipantInputHandler(
              Map.empty
            ),
            ParticipantGridAdapter(
              gridAgent.ref,
              12 * 3600,
            ),
            Iterable(resultListener.ref),
            Right(em.ref, receiveAdapter.ref),
          )
        )
        val flexRef = receiveAdapter.expectMessageType[ActorRef[FlexRequest]]

        // TICK 0: Outside of operation interval

        flexRef ! FlexActivation(0)

        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(0))
            min should approximate(Kilowatts(0))
            max should approximate(Kilowatts(0))
        }

        flexRef ! IssueNoControl(0)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe simulationStartDate
            result.getP should equalWithTolerance(0.0.asMegaWatt)
            result.getQ should equalWithTolerance(0.0.asMegaVar)
        }

        em.expectMessage(
          FlexCompletion(
            model.uuid,
            requestAtTick = Some(operationInterval.start),
          )
        )

        // TICK 8 * 3600: Start of operation interval

        flexRef ! FlexActivation(operationInterval.start)

        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(1))
            min should approximate(Kilowatts(-1))
            max should approximate(Kilowatts(3))
        }

        flexRef ! IssuePowerControl(operationInterval.start, Kilowatts(3))

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.start.toDateTime
            result.getP should equalWithTolerance(0.003.asMegaWatt)
            result.getQ should equalWithTolerance(0.0014529663.asMegaVar)
        }

        em.expectMessage(
          FlexCompletion(
            model.uuid,
            requestAtTick = Some(12 * 3600),
          )
        )

        // TICK 12 * 3600: Middle of operation interval and GridAgent requests power

        flexRef ! FlexActivation(12 * 3600)

        participantAgent ! RequestAssetPowerMessage(12 * 3600, Each(1), Each(0))

        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(1))
            q should approximate(Kilovars(0.48432210483))
        }

        resultListener.expectNoMessage()
        em.expectNoMessage()

        participantAgent ! FinishParticipantSimulation(12 * 3600, 24 * 3600)

        // calculation should start now
        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(1))
            min should approximate(Kilowatts(-1))
            max should approximate(Kilowatts(3))
        }

        flexRef ! IssueNoControl(12 * 3600)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe (12 * 3600).toDateTime
            result.getP should equalWithTolerance(0.001.asMegaWatt)
            result.getQ should equalWithTolerance(0.0004843221.asMegaVar)
        }

        em.expectMessage(
          FlexCompletion(
            model.uuid,
            requestAtTick = Some(operationInterval.end),
          )
        )

        // TICK 20 * 3600: Outside of operation interval (last tick)

        flexRef ! FlexActivation(operationInterval.end)

        em.expectMessageType[ProvideMinMaxFlexOptions] match {
          case ProvideMinMaxFlexOptions(modelUuid, ref, min, max) =>
            modelUuid shouldBe model.uuid
            ref should approximate(Kilowatts(0))
            min should approximate(Kilowatts(0))
            max should approximate(Kilowatts(0))
        }

        flexRef ! IssueNoControl(operationInterval.end)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(result: MockResult) =>
            result.getInputModel shouldBe model.uuid
            result.getTime shouldBe operationInterval.end.toDateTime
            result.getP should equalWithTolerance(0.0.asMegaWatt)
            result.getQ should equalWithTolerance(0.0.asMegaVar)
        }

        em.expectMessage(FlexCompletion(model.uuid))

        // TICK 24 * 3600: GridAgent requests power

        participantAgent ! RequestAssetPowerMessage(24 * 3600, Each(1), Each(0))

        gridAgent.expectMessageType[AssetPowerChangedMessage] match {
          case AssetPowerChangedMessage(p, q) =>
            p should approximate(Kilowatts(0.6666666667))
            q should approximate(Kilovars(0.32288140322))
        }

        participantAgent ! FinishParticipantSimulation(24 * 3600, 36 * 3600)

      }

    }

  }

}
