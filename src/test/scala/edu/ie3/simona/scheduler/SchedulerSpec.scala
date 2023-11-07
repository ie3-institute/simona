/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.adapter.TypedActorRefOps
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage._
import edu.ie3.simona.ontology.trigger.Trigger
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeTrigger
}
import edu.ie3.simona.scheduler.SchedulerSpec.RichTriggeredAgent
import edu.ie3.simona.scheduler.SimSchedulerSpec.fail
import edu.ie3.simona.util.SimonaConstants
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.mockito.Mockito.doReturn
import org.scalatest.matchers.should
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.mockito.MockitoSugar.mock

class SchedulerSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with should.Matchers {

  def createMockInitTrigger(): InitializeTrigger = {
    val mockTrigger = mock[InitializeTrigger]
    doReturn(SimonaConstants.INIT_SIM_TICK).when(mockTrigger).tick
    mockTrigger
  }

  "The Scheduler" should {

    "initialize as expected when receiving triggers before activation" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val triggeredAgent1 = TestProbe[TriggerWithIdMessage]("agent_1")
      val triggeredAgent2 = TestProbe[TriggerWithIdMessage]("agent_2")

      val initTrigger1 = createMockInitTrigger()
      scheduler ! ScheduleTriggerMessage(
        initTrigger1,
        triggeredAgent1.ref.toClassic
      )

      val initTrigger2 = createMockInitTrigger()
      scheduler ! ScheduleTriggerMessage(
        initTrigger2,
        triggeredAgent2.ref.toClassic
      )

      triggeredAgent1.expectNoMessage()
      triggeredAgent2.expectNoMessage()

      val triggerId = 0L
      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(INIT_SIM_TICK),
        triggerId
      )

      val receivedTrigger1 =
        triggeredAgent1.expectMessageType[TriggerWithIdMessage]
      receivedTrigger1.trigger shouldBe initTrigger1

      val receivedTrigger2 =
        triggeredAgent2.expectMessageType[TriggerWithIdMessage]
      receivedTrigger2.trigger shouldBe initTrigger2

      scheduler ! CompletionMessage(
        receivedTrigger1.triggerId,
        None
      )

      parent.expectNoMessage()

      scheduler ! CompletionMessage(
        receivedTrigger2.triggerId,
        None
      )

      parent.expectMessage(CompletionMessage(triggerId, None))
    }

    "initialize as expected when receiving triggers after init trigger" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val triggerId = 0L
      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(INIT_SIM_TICK),
        triggerId
      )

      val triggeredAgent1 = TestProbe[TriggerWithIdMessage]("agent_1")
      val triggeredAgent2 = TestProbe[TriggerWithIdMessage]("agent_2")

      val initTrigger1 = createMockInitTrigger()
      scheduler ! ScheduleTriggerMessage(
        initTrigger1,
        triggeredAgent1.ref.toClassic
      )

      val initTrigger2 = createMockInitTrigger()
      scheduler ! ScheduleTriggerMessage(
        initTrigger2,
        triggeredAgent2.ref.toClassic
      )

      // trigger are sent right away
      val receivedTrigger1 =
        triggeredAgent1.expectMessageType[TriggerWithIdMessage]
      receivedTrigger1.trigger shouldBe initTrigger1

      val receivedTrigger2 =
        triggeredAgent2.expectMessageType[TriggerWithIdMessage]
      receivedTrigger2.trigger shouldBe initTrigger2

      scheduler ! CompletionMessage(
        receivedTrigger1.triggerId,
        Some(
          ScheduleTriggerMessage(
            ActivityStartTrigger(0L),
            triggeredAgent1.ref.toClassic
          )
        )
      )

      parent.expectNoMessage()

      scheduler ! CompletionMessage(
        receivedTrigger2.triggerId,
        None
      )

      parent.expectMessage(
        CompletionMessage(
          triggerId,
          Some(
            ScheduleTriggerMessage(
              ActivityStartTrigger(0L),
              scheduler.ref.toClassic
            )
          )
        )
      )
    }

    "work as expected when scheduling two actors for different ticks" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val triggeredAgent1 = TestProbe[TriggerWithIdMessage]("agent_1")
      val triggeredAgent2 = TestProbe[TriggerWithIdMessage]("agent_2")

      val initTrigger1 = createMockInitTrigger()
      scheduler ! ScheduleTriggerMessage(
        initTrigger1,
        triggeredAgent1.ref.toClassic
      )

      val initTrigger2 = createMockInitTrigger()
      scheduler ! ScheduleTriggerMessage(
        initTrigger2,
        triggeredAgent2.ref.toClassic
      )

      /* ACTIVATE INIT TICK */
      val triggerId0 = 0L
      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(INIT_SIM_TICK),
        triggerId0
      )

      triggeredAgent1.expectTriggerAndComplete(
        scheduler,
        INIT_SIM_TICK,
        Some(0L)
      )

      parent.expectNoMessage()

      triggeredAgent2.expectTriggerAndComplete(
        scheduler,
        INIT_SIM_TICK,
        Some(0L)
      )

      parent.expectMessage(
        CompletionMessage(
          triggerId0,
          Some(
            ScheduleTriggerMessage(
              ActivityStartTrigger(0L),
              scheduler.toClassic
            )
          )
        )
      )

      triggeredAgent1.expectNoMessage()
      triggeredAgent2.expectNoMessage()

      /* ACTIVATE TICK 0 */
      val triggerId1 = 1L
      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(0L),
        triggerId1
      )

      triggeredAgent1.expectTriggerAndComplete(
        scheduler,
        0L,
        Some(300L)
      )

      parent.expectNoMessage()

      triggeredAgent2.expectTriggerAndComplete(
        scheduler,
        0L,
        Some(900L)
      )

      parent.expectMessage(
        CompletionMessage(
          triggerId1,
          Some(
            ScheduleTriggerMessage(
              ActivityStartTrigger(300L),
              scheduler.toClassic
            )
          )
        )
      )

      /* ACTIVATE TICK 300 */
      val triggerId2 = 2L
      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(300L),
        triggerId2
      )

      triggeredAgent1.expectTriggerAndComplete(
        scheduler,
        300L,
        Some(900L)
      )

      parent.expectMessage(
        CompletionMessage(
          triggerId2,
          Some(
            ScheduleTriggerMessage(
              ActivityStartTrigger(900L),
              scheduler.toClassic
            )
          )
        )
      )

      triggeredAgent1.expectNoMessage()
      triggeredAgent2.expectNoMessage()

      /* ACTIVATE TICK 900 */
      val triggerId3 = 3L
      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(900L),
        triggerId3
      )

      triggeredAgent1.expectTriggerAndComplete(
        scheduler,
        900L,
        Some(3600L)
      )

      parent.expectNoMessage()

      triggeredAgent2.expectTriggerAndComplete(
        scheduler,
        900L,
        Some(1800L)
      )

      parent.expectMessage(
        CompletionMessage(
          triggerId3,
          Some(
            ScheduleTriggerMessage(
              ActivityStartTrigger(1800L),
              scheduler.toClassic
            )
          )
        )
      )

      parent.expectNoMessage()
      triggeredAgent1.expectNoMessage()
      triggeredAgent2.expectNoMessage()
    }

    // TODO scheduling with parent, unlocking

    /* ERRORS */

    "stop when activated with wrong tick" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val triggeredAgent1 = TestProbe[TriggerWithIdMessage]("agent_1")

      scheduler ! ScheduleTriggerMessage(
        createMockInitTrigger(),
        triggeredAgent1.ref.toClassic
      )

      triggeredAgent1.expectNoMessage()

      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(0L),
        0L
      )

      // agent does not receive activation
      triggeredAgent1.expectNoMessage()
      parent.expectNoMessage()

      // scheduler stopped
      parent.expectTerminated(scheduler)
    }

    "stop when asked to schedule trigger for a past tick" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(0L),
        0L
      )

      val triggeredAgent1 = TestProbe[TriggerWithIdMessage]("agent_1")

      scheduler ! ScheduleTriggerMessage(
        createMockInitTrigger(),
        triggeredAgent1.ref.toClassic
      )

      // agent does not receive activation
      triggeredAgent1.expectNoMessage()
      parent.expectNoMessage()

      // scheduler stopped
      parent.expectTerminated(scheduler)
    }

    "stop when receiving completion message with unexpected trigger id" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(INIT_SIM_TICK),
        0L
      )

      val triggeredAgent1 = TestProbe[TriggerWithIdMessage]("agent_1")

      val initTrigger1 = createMockInitTrigger()
      scheduler ! ScheduleTriggerMessage(
        initTrigger1,
        triggeredAgent1.ref.toClassic
      )

      val receivedTrigger =
        triggeredAgent1.expectMessageType[TriggerWithIdMessage]
      receivedTrigger.trigger shouldBe initTrigger1

      // wrong triggerId
      scheduler ! CompletionMessage(receivedTrigger.triggerId + 1, None)

      parent.expectNoMessage()

      // scheduler stopped
      parent.expectTerminated(scheduler)
    }

    "stop when receiving unexpected message in active mode" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(INIT_SIM_TICK),
        0L
      )

      // scheduler is already active, can't handle activation a second time
      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(0L),
        1L
      )

      parent.expectNoMessage()

      // scheduler stopped
      parent.expectTerminated(scheduler)
    }

    "stop when receiving unexpected message in inactive mode" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      // scheduler is inactive, can't handle completion
      scheduler ! CompletionMessage(0L, None)

      parent.expectNoMessage()

      // scheduler stopped
      parent.expectTerminated(scheduler)
    }
  }
}

object SchedulerSpec {

  implicit class RichTriggeredAgent(
      private val triggeredAgent: TestProbe[TriggerWithIdMessage]
  ) {

    def expectTriggerAndComplete[T <: Trigger](
        scheduler: ActorRef[SchedulerMessage],
        expectedTick: Long,
        newTick: Option[Long] = None
    ): Unit = {
      val receivedTrigger =
        triggeredAgent.expectMessageType[TriggerWithIdMessage]

      receivedTrigger.trigger match {
        case trigger: T =>
          trigger.tick shouldBe expectedTick
        case unexpected =>
          fail(s"Received unexpected trigger $unexpected")
      }

      val newTrigger =
        newTick.map(tick =>
          ScheduleTriggerMessage(
            ActivityStartTrigger(tick),
            triggeredAgent.ref.toClassic
          )
        )

      scheduler ! CompletionMessage(
        receivedTrigger.triggerId,
        newTrigger
      )
    }

  }
}
