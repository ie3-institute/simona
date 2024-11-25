/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant2

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.participant2.MockParticipantModel.MockResult
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.model.participant2.ParticipantModelShell
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.OperationInterval
import org.apache.pekko.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.apache.pekko.actor.typed.ActorRef

import java.time.ZonedDateTime

class ParticipantAgentSpec extends ScalaTestWithActorTestKit with UnitSpec {

  private implicit val simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z")

  "A ParticipantAgent without secondary services" should {

    "calculate operating point and results correctly with no additional ticks" in {

      val scheduler = createTestProbe[SchedulerMessage]()
      val gridAgent = createTestProbe[GridAgent.Request]()
      val resultListener = createTestProbe[ResultEvent]()

      // receiving the activation adapter
      val receiveAdapter = createTestProbe[ActorRef[Activation]]()

      // no additional activation ticks
      val model = new MockParticipantModel(mockActivationTicks = Map.empty)
      val operationInterval = OperationInterval(8 * 3600, 16 * 3600)

      spawn(
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
            3600,
          ),
          Iterable(resultListener.ref),
          Left(scheduler.ref, receiveAdapter.ref),
        )
      )
      val activationRef = receiveAdapter.expectMessageType[ActorRef[Activation]]

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
          result.getP should equalWithTolerance(0.005.asMegaWatt)
          result.getQ should equalWithTolerance(0.002421610524.asMegaVar)
      }

      scheduler.expectMessage(
        Completion(activationRef, Some(operationInterval.end))
      )

      // TICK 16 * 3600: Outside of operation interval (last tick)

      activationRef ! Activation(operationInterval.end)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(result: MockResult) =>
          result.getInputModel shouldBe model.uuid
          result.getTime shouldBe operationInterval.end.toDateTime
          result.getP should equalWithTolerance(0.0.asMegaWatt)
          result.getQ should equalWithTolerance(0.0.asMegaVar)
      }

      scheduler.expectMessage(Completion(activationRef))

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
          12 * 3600L -> 20 * 3600L, // after operation, is ignored
        )
      )
      val operationInterval = OperationInterval(8 * 3600, 16 * 3600)

      spawn(
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
            3600,
          ),
          Iterable(resultListener.ref),
          Left(scheduler.ref, receiveAdapter.ref),
        )
      )
      val activationRef = receiveAdapter.expectMessageType[ActorRef[Activation]]

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
          result.getP should equalWithTolerance(0.005.asMegaWatt)
          result.getQ should equalWithTolerance(0.002421610524.asMegaVar)
      }

      scheduler.expectMessage(
        Completion(activationRef, Some(12 * 3600))
      )

      // TICK 12 * 3600: Middle of operation interval

      activationRef ! Activation(12 * 3600)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(result: MockResult) =>
          result.getInputModel shouldBe model.uuid
          result.getTime shouldBe (12 * 3600).toDateTime
          result.getP should equalWithTolerance(0.005.asMegaWatt)
          result.getQ should equalWithTolerance(0.002421610524.asMegaVar)
      }

      scheduler.expectMessage(
        Completion(activationRef, Some(operationInterval.end))
      )

      // TICK 16 * 3600: Outside of operation interval (last tick)

      activationRef ! Activation(operationInterval.end)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(result: MockResult) =>
          result.getInputModel shouldBe model.uuid
          result.getTime shouldBe operationInterval.end.toDateTime
          result.getP should equalWithTolerance(0.0.asMegaWatt)
          result.getQ should equalWithTolerance(0.0.asMegaVar)
      }

      scheduler.expectMessage(Completion(activationRef))

    }

  }

}
