/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock.{LockMsg, ScheduleKey, Unlock}
import edu.ie3.simona.util.ActorUtils.RichTriggeredAgent
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.UUID

class SchedulerSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with should.Matchers {

  "The Scheduler should work correctly" when {

    "receiving triggers before activation" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val agent1 = TestProbe[Activation]("agent_1")
      val agent2 = TestProbe[Activation]("agent_2")

      scheduler ! ScheduleActivation(agent1.ref, INIT_SIM_TICK)

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe INIT_SIM_TICK
      val schedulerActivation = sa1.actor

      scheduler ! ScheduleActivation(agent2.ref, INIT_SIM_TICK)

      agent1.expectNoMessage()
      agent2.expectNoMessage()

      schedulerActivation ! Activation(INIT_SIM_TICK)

      agent1.expectMessage(Activation(INIT_SIM_TICK))
      agent2.expectMessage(Activation(INIT_SIM_TICK))

      scheduler ! Completion(agent1.ref)

      parent.expectNoMessage()

      scheduler ! Completion(agent2.ref)

      parent.expectMessage(Completion(schedulerActivation))
    }

    "receiving triggers after init trigger" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val agent1 = TestProbe[Activation]("agent_1")
      val agent2 = TestProbe[Activation]("agent_2")

      scheduler ! ScheduleActivation(agent1.ref, INIT_SIM_TICK)

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe INIT_SIM_TICK
      val schedulerActivation = sa1.actor

      agent1.expectNoMessage()

      schedulerActivation ! Activation(INIT_SIM_TICK)

      agent1.expectMessage(Activation(INIT_SIM_TICK))
      agent2.expectNoMessage()

      scheduler ! ScheduleActivation(
        agent2.ref,
        INIT_SIM_TICK
      )

      // trigger is sent right away
      agent2.expectMessage(Activation(INIT_SIM_TICK))

      scheduler ! Completion(agent2.ref)

      parent.expectNoMessage()

      scheduler ! Completion(agent1.ref)

      parent.expectMessage(Completion(schedulerActivation))
    }

    "scheduling two actors for different ticks" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val agent1 = TestProbe[Activation]("agent_1")
      val agent2 = TestProbe[Activation]("agent_2")

      scheduler ! ScheduleActivation(
        agent1.ref,
        INIT_SIM_TICK
      )

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe INIT_SIM_TICK
      val schedulerActivation = sa1.actor

      scheduler ! ScheduleActivation(
        agent2.ref,
        INIT_SIM_TICK
      )

      /* ACTIVATE INIT TICK */
      schedulerActivation ! Activation(INIT_SIM_TICK)

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

      parent.expectMessage(Completion(schedulerActivation, Some(0)))

      agent1.expectNoMessage()
      agent2.expectNoMessage()

      /* ACTIVATE TICK 0 */
      schedulerActivation ! Activation(0)

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

      parent.expectMessage(Completion(schedulerActivation, Some(300)))

      /* ACTIVATE TICK 300 */
      schedulerActivation ! Activation(300)

      agent1.expectTriggerAndComplete(
        scheduler,
        300,
        Some(900)
      )

      parent.expectMessage(Completion(schedulerActivation, Some(900)))

      agent1.expectNoMessage()
      agent2.expectNoMessage()

      /* ACTIVATE TICK 900 */
      schedulerActivation ! Activation(900)

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

      parent.expectMessage(Completion(schedulerActivation, Some(1800)))

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
        .map(i => TestProbe[Activation](s"agent_$i"))

      triggeredAgents.foreach(actor =>
        // send to init trigger to scheduler
        scheduler ! ScheduleActivation(
          actor.ref,
          INIT_SIM_TICK
        )
      )

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe INIT_SIM_TICK
      val schedulerActivation = sa1.actor

      for (tick <- -1 to 8) {
        schedulerActivation ! Activation(tick)

        triggeredAgents.foreach {
          _.expectTriggerAndComplete(
            scheduler,
            tick,
            Some(tick + 1)
          )
        }

        parent.expectMessage(Completion(schedulerActivation, Some(tick + 1)))
      }
    }

    "unlocking a scheduling lock while active" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val agent1 = TestProbe[Activation]("agent_1")
      val agent2 = TestProbe[Activation]("agent_2")
      val lock = TestProbe[LockMsg]("lock")

      scheduler ! ScheduleActivation(agent1.ref, 60)

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe 60
      val schedulerActivation = sa1.actor

      schedulerActivation ! Activation(60)
      agent1.expectMessage(Activation(60))

      val key = UUID.randomUUID()
      scheduler ! ScheduleActivation(
        agent2.ref,
        120,
        Some(ScheduleKey(lock.ref, key))
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

      val agent1 = TestProbe[Activation]("agent_1")
      val lock = TestProbe[LockMsg]("lock")

      scheduler ! ScheduleActivation(agent1.ref, 60)

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe 60

      val key = UUID.randomUUID()
      scheduler ! ScheduleActivation(
        agent1.ref,
        60,
        Some(ScheduleKey(lock.ref, key))
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

      val agent1 = TestProbe[Activation]("agent_1")
      val lock = TestProbe[LockMsg]("lock")

      scheduler ! ScheduleActivation(agent1.ref, 60)

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe 60
      val schedulerActivation = sa1.actor

      val key = UUID.randomUUID()
      scheduler ! ScheduleActivation(
        agent1.ref,
        59,
        Some(ScheduleKey(lock.ref, key))
      )

      // lock should not receive unlock message by scheduler
      lock.expectNoMessage()

      // responsibility of unlocking forwarded to parent
      parent.expectMessage(
        ScheduleActivation(
          schedulerActivation,
          59,
          Some(ScheduleKey(lock.ref, key))
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

      val agent1 = TestProbe[Activation]("agent_1")

      scheduler ! ScheduleActivation(agent1.ref, INIT_SIM_TICK)

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe INIT_SIM_TICK
      val schedulerActivation = sa1.actor

      schedulerActivation ! Activation(0)

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

      val agent1 = TestProbe[Activation]("agent_1")

      scheduler ! ScheduleActivation(agent1.ref, 900)

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe 900
      val schedulerActivation = sa1.actor

      schedulerActivation ! Activation(900)

      agent1.expectTriggerAndComplete(
        scheduler,
        900,
        Some(1800)
      )

      parent.expectMessage(Completion(schedulerActivation, Some(1800)))

      // now inactive again
      // can't schedule trigger with earlier tick than last tick (900) -> error
      scheduler ! ScheduleActivation(agent1.ref, INIT_SIM_TICK)

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

      val agent1 = TestProbe[Activation]("agent_1")

      scheduler ! ScheduleActivation(agent1.ref, 0)

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe 0
      val schedulerActivation = sa1.actor

      // activate tick 0
      schedulerActivation ! Activation(0)
      agent1.expectMessage(Activation(0))

      // can't schedule trigger for earlier tick than current active -> error
      scheduler ! ScheduleActivation(agent1.ref, INIT_SIM_TICK)

      // agent does not receive activation
      agent1.expectNoMessage()
      parent.expectNoMessage()

      // scheduler stopped
      parent.expectTerminated(scheduler)
    }

    "receiving unexpected message while active" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val agent1 = TestProbe[Activation]("agent_1")

      scheduler ! ScheduleActivation(agent1.ref, INIT_SIM_TICK)

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe INIT_SIM_TICK
      val schedulerActivation = sa1.actor

      schedulerActivation ! Activation(INIT_SIM_TICK)

      // scheduler is already active, can't handle activation a second time
      schedulerActivation ! Activation(0)

      parent.expectNoMessage()

      // scheduler stopped
      parent.expectTerminated(scheduler)
    }

    "receiving unexpected message while inactive" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val agent1 = TestProbe[Activation]("agent_1")

      scheduler ! ScheduleActivation(agent1.ref, INIT_SIM_TICK)

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe INIT_SIM_TICK
      val schedulerActivation = sa1.actor

      schedulerActivation ! Activation(INIT_SIM_TICK)

      agent1.expectTriggerAndComplete(
        scheduler,
        INIT_SIM_TICK,
        Some(0)
      )

      parent.expectMessage(Completion(schedulerActivation.ref, Some(0)))

      // scheduler is inactive, can't handle completion
      scheduler ! Completion(agent1.ref, None)

      parent.expectNoMessage()

      // scheduler stopped
      parent.expectTerminated(scheduler)
    }
  }

}
