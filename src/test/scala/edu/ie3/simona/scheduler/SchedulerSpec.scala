/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.scaladsl.adapter.TypedActorRefOps
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage._
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeTrigger
}
import edu.ie3.simona.scheduler.ScheduleLock.{LockMsg, Unlock}
import edu.ie3.simona.util.ActorUtils.RichTriggeredAgent
import edu.ie3.simona.util.SimonaConstants
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.mockito.Mockito.doReturn
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.mockito.MockitoSugar.mock

import java.util.UUID

class SchedulerSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with should.Matchers {

  def createMockInitTrigger(): InitializeTrigger = {
    val mockTrigger = mock[InitializeTrigger]
    doReturn(SimonaConstants.INIT_SIM_TICK).when(mockTrigger).tick
    mockTrigger
  }

  "The Scheduler should work correctly" when {

    "receiving triggers before activation" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val agent1 = TestProbe[TriggerWithIdMessage]("agent_1")
      val agent2 = TestProbe[TriggerWithIdMessage]("agent_2")

      val initTrigger1 = createMockInitTrigger()
      scheduler ! ScheduleTriggerMessage(
        initTrigger1,
        agent1.ref.toClassic
      )

      parent.expectMessage(
        ScheduleTriggerMessage(
          ActivityStartTrigger(INIT_SIM_TICK),
          scheduler.toClassic
        )
      )

      val initTrigger2 = createMockInitTrigger()
      scheduler ! ScheduleTriggerMessage(
        initTrigger2,
        agent2.ref.toClassic
      )

      agent1.expectNoMessage()
      agent2.expectNoMessage()

      val triggerId = 0
      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(INIT_SIM_TICK),
        triggerId
      )

      val receivedTrigger1 =
        agent1.expectMessageType[TriggerWithIdMessage]
      receivedTrigger1.trigger shouldBe initTrigger1

      val receivedTrigger2 =
        agent2.expectMessageType[TriggerWithIdMessage]
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

    "receiving triggers after init trigger" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val triggerId = 0
      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(INIT_SIM_TICK),
        triggerId
      )

      val agent1 = TestProbe[TriggerWithIdMessage]("agent_1")
      val agent2 = TestProbe[TriggerWithIdMessage]("agent_2")

      val initTrigger1 = createMockInitTrigger()
      scheduler ! ScheduleTriggerMessage(
        initTrigger1,
        agent1.ref.toClassic
      )

      val initTrigger2 = createMockInitTrigger()
      scheduler ! ScheduleTriggerMessage(
        initTrigger2,
        agent2.ref.toClassic
      )

      // trigger are sent right away
      val receivedTrigger1 =
        agent1.expectMessageType[TriggerWithIdMessage]
      receivedTrigger1.trigger shouldBe initTrigger1

      val receivedTrigger2 =
        agent2.expectMessageType[TriggerWithIdMessage]
      receivedTrigger2.trigger shouldBe initTrigger2

      scheduler ! CompletionMessage(
        receivedTrigger1.triggerId,
        Some(
          ScheduleTriggerMessage(
            ActivityStartTrigger(0),
            agent1.ref.toClassic
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
              ActivityStartTrigger(0),
              scheduler.ref.toClassic
            )
          )
        )
      )
    }

    "scheduling two actors for different ticks" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val agent1 = TestProbe[TriggerWithIdMessage]("agent_1")
      val agent2 = TestProbe[TriggerWithIdMessage]("agent_2")

      val initTrigger1 = createMockInitTrigger()
      scheduler ! ScheduleTriggerMessage(
        initTrigger1,
        agent1.ref.toClassic
      )

      parent.expectMessage(
        ScheduleTriggerMessage(
          ActivityStartTrigger(INIT_SIM_TICK),
          scheduler.toClassic
        )
      )

      val initTrigger2 = createMockInitTrigger()
      scheduler ! ScheduleTriggerMessage(
        initTrigger2,
        agent2.ref.toClassic
      )

      /* ACTIVATE INIT TICK */
      val triggerId0 = 0
      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(INIT_SIM_TICK),
        triggerId0
      )

      agent1.expectTriggerAndComplete(
        scheduler,
        INIT_SIM_TICK,
        Some(0)
      )

      parent.expectNoMessage()

      agent2.expectTriggerAndComplete(
        scheduler,
        INIT_SIM_TICK,
        Some(0)
      )

      parent.expectMessage(
        CompletionMessage(
          triggerId0,
          Some(
            ScheduleTriggerMessage(
              ActivityStartTrigger(0),
              scheduler.toClassic
            )
          )
        )
      )

      agent1.expectNoMessage()
      agent2.expectNoMessage()

      /* ACTIVATE TICK 0 */
      val triggerId1 = 1
      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(0),
        triggerId1
      )

      agent1.expectTriggerAndComplete(
        scheduler,
        0,
        Some(300)
      )

      parent.expectNoMessage()

      agent2.expectTriggerAndComplete(
        scheduler,
        0,
        Some(900)
      )

      parent.expectMessage(
        CompletionMessage(
          triggerId1,
          Some(
            ScheduleTriggerMessage(
              ActivityStartTrigger(300),
              scheduler.toClassic
            )
          )
        )
      )

      /* ACTIVATE TICK 300 */
      val triggerId2 = 2
      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(300),
        triggerId2
      )

      agent1.expectTriggerAndComplete(
        scheduler,
        300,
        Some(900)
      )

      parent.expectMessage(
        CompletionMessage(
          triggerId2,
          Some(
            ScheduleTriggerMessage(
              ActivityStartTrigger(900),
              scheduler.toClassic
            )
          )
        )
      )

      agent1.expectNoMessage()
      agent2.expectNoMessage()

      /* ACTIVATE TICK 900 */
      val triggerId3 = 3
      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(900),
        triggerId3
      )

      agent1.expectTriggerAndComplete(
        scheduler,
        900,
        Some(3600)
      )

      parent.expectNoMessage()

      agent2.expectTriggerAndComplete(
        scheduler,
        900,
        Some(1800)
      )

      parent.expectMessage(
        CompletionMessage(
          triggerId3,
          Some(
            ScheduleTriggerMessage(
              ActivityStartTrigger(1800),
              scheduler.toClassic
            )
          )
        )
      )

      parent.expectNoMessage()
      agent1.expectNoMessage()
      agent2.expectNoMessage()
    }

    "five actors are getting triggered for ten ticks" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val triggeredAgents = (1 to 5)
        .map(i => TestProbe[TriggerWithIdMessage](s"agent_$i"))

      triggeredAgents.foreach(actor =>
        // send to init trigger to scheduler
        scheduler ! ScheduleTriggerMessage(
          createMockInitTrigger(),
          actor.ref.toClassic
        )
      )

      parent.expectMessage(
        ScheduleTriggerMessage(
          ActivityStartTrigger(INIT_SIM_TICK),
          scheduler.toClassic
        )
      )

      for (tick <- -1 to 8) {
        val triggerId = tick + 2
        scheduler ! TriggerWithIdMessage(ActivityStartTrigger(tick), triggerId)

        triggeredAgents.foreach {
          _.expectTriggerAndComplete(
            scheduler,
            tick,
            Some(tick + 1)
          )
        }

        parent.expectMessage(
          CompletionMessage(
            triggerId,
            Some(
              ScheduleTriggerMessage(
                ActivityStartTrigger(tick + 1),
                scheduler.ref.toClassic
              )
            )
          )
        )
      }
    }

    "unlocking a scheduling lock while active" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val agent1 = TestProbe[TriggerWithIdMessage]("agent_1")
      val agent2 = TestProbe[TriggerWithIdMessage]("agent_2")
      val lock = TestProbe[LockMsg]("lock")

      scheduler ! ScheduleTriggerMessage(
        ActivityStartTrigger(60),
        agent1.ref.toClassic
      )

      parent.expectMessage(
        ScheduleTriggerMessage(
          ActivityStartTrigger(60),
          scheduler.toClassic
        )
      )

      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(60),
        triggerId = 5
      )
      agent1.expectMessageType[TriggerWithIdMessage]

      val key = UUID.randomUUID()
      scheduler ! ScheduleTriggerMessage(
        ActivityStartTrigger(120),
        agent2.ref.toClassic,
        Some(lock.ref, key)
      )

      // no new scheduling when active
      parent.expectNoMessage()

      // lock should receive unlock message
      lock.expectMessage(Unlock(key))
    }

    "unlocking a scheduling lock while inactive and new earliest tick has not changed" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val agent1 = TestProbe[TriggerWithIdMessage]("agent_1")
      val lock = TestProbe[LockMsg]("lock")

      scheduler ! ScheduleTriggerMessage(
        ActivityStartTrigger(60),
        agent1.ref.toClassic
      )

      parent.expectMessage(
        ScheduleTriggerMessage(
          ActivityStartTrigger(60),
          scheduler.toClassic
        )
      )

      val key = UUID.randomUUID()
      scheduler ! ScheduleTriggerMessage(
        ActivityStartTrigger(60),
        agent1.ref.toClassic,
        Some(lock.ref, key)
      )

      // no new scheduling for same tick
      parent.expectNoMessage()

      // lock should receive unlock message
      lock.expectMessage(Unlock(key))
    }

    "forwarding unlock information to parent while inactive and new earliest tick has changed" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val agent1 = TestProbe[TriggerWithIdMessage]("agent_1")
      val lock = TestProbe[LockMsg]("lock")

      scheduler ! ScheduleTriggerMessage(
        ActivityStartTrigger(60),
        agent1.ref.toClassic
      )

      parent.expectMessage(
        ScheduleTriggerMessage(
          ActivityStartTrigger(60),
          scheduler.toClassic
        )
      )

      val key = UUID.randomUUID()
      scheduler ! ScheduleTriggerMessage(
        ActivityStartTrigger(59),
        agent1.ref.toClassic,
        Some(lock.ref, key)
      )

      // lock should not receive unlock message by scheduler
      lock.expectNoMessage()

      // responsibility of unlocking forwarded to parent
      parent.expectMessage(
        ScheduleTriggerMessage(
          ActivityStartTrigger(59),
          scheduler.toClassic,
          Some(lock.ref, key)
        )
      )
    }

  }

  "The Scheduler should fail and stop" when {

    "activated with wrong tick" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val agent1 = TestProbe[TriggerWithIdMessage]("agent_1")

      scheduler ! ScheduleTriggerMessage(
        createMockInitTrigger(),
        agent1.ref.toClassic
      )

      parent.expectMessageType[ScheduleTriggerMessage]
      agent1.expectNoMessage()

      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(0),
        0
      )

      // agent does not receive activation
      agent1.expectNoMessage()
      parent.expectNoMessage()

      // scheduler stopped
      parent.expectTerminated(scheduler)
    }

    "asked to schedule trigger for a past tick while inactive" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val agent1 = TestProbe[TriggerWithIdMessage]("agent_1")

      scheduler ! ScheduleTriggerMessage(
        ActivityStartTrigger(900),
        agent1.ref.toClassic
      )

      parent.expectMessage(
        ScheduleTriggerMessage(
          ActivityStartTrigger(900),
          scheduler.toClassic
        )
      )

      val triggerId = 1
      scheduler ! TriggerWithIdMessage(ActivityStartTrigger(900), triggerId)

      agent1.expectTriggerAndComplete(
        scheduler,
        900,
        Some(1800)
      )

      parent.expectMessage(
        CompletionMessage(
          triggerId,
          Some(
            ScheduleTriggerMessage(
              ActivityStartTrigger(1800),
              scheduler.ref.toClassic
            )
          )
        )
      )

      // now inactive again
      // can't schedule trigger with earlier tick than last tick (900) -> error
      scheduler ! ScheduleTriggerMessage(
        createMockInitTrigger(),
        agent1.ref.toClassic
      )

      // agent does not receive activation
      agent1.expectNoMessage()
      parent.expectNoMessage()

      // scheduler stopped
      parent.expectTerminated(scheduler)
    }

    "asked to schedule trigger for a past tick while active" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(0),
        0
      )

      val agent1 = TestProbe[TriggerWithIdMessage]("agent_1")

      // can't schedule trigger for earlier tick than current active -> error
      scheduler ! ScheduleTriggerMessage(
        createMockInitTrigger(),
        agent1.ref.toClassic
      )

      // agent does not receive activation
      agent1.expectNoMessage()
      parent.expectNoMessage()

      // scheduler stopped
      parent.expectTerminated(scheduler)
    }

    "receiving completion message with unexpected trigger id" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(INIT_SIM_TICK),
        0
      )

      val agent1 = TestProbe[TriggerWithIdMessage]("agent_1")

      val initTrigger1 = createMockInitTrigger()
      scheduler ! ScheduleTriggerMessage(
        initTrigger1,
        agent1.ref.toClassic
      )

      val receivedTrigger =
        agent1.expectMessageType[TriggerWithIdMessage]
      receivedTrigger.trigger shouldBe initTrigger1

      // wrong triggerId
      scheduler ! CompletionMessage(receivedTrigger.triggerId + 1, None)

      parent.expectNoMessage()

      // scheduler stopped
      parent.expectTerminated(scheduler)
    }

    "receiving unexpected message while active" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(INIT_SIM_TICK),
        0
      )

      // scheduler is already active, can't handle activation a second time
      scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(0),
        1
      )

      parent.expectNoMessage()

      // scheduler stopped
      parent.expectTerminated(scheduler)
    }

    "receiving unexpected message while inactive" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      // scheduler is inactive, can't handle completion
      scheduler ! CompletionMessage(0, None)

      parent.expectNoMessage()

      // scheduler stopped
      parent.expectTerminated(scheduler)
    }
  }

}

object SchedulerSpec {}
