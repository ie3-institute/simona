/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock.{LockMsg, ScheduleKey, Unlock}
import edu.ie3.simona.util.ActorUtils.RichActivatedActor
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.UUID

class SchedulerSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with should.Matchers {

  "The Scheduler should work correctly" when {

    "receiving activation scheduling before activation" in {
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

      // TICK -1
      schedulerActivation ! Activation(INIT_SIM_TICK)

      agent1.expectMessage(Activation(INIT_SIM_TICK))
      agent2.expectMessage(Activation(INIT_SIM_TICK))

      scheduler ! Completion(agent1.ref)

      parent.expectNoMessage()

      scheduler ! Completion(agent2.ref)

      parent.expectMessage(Completion(schedulerActivation))
    }

    "receiving activation scheduling after init activation" in {
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

      // TICK -1
      schedulerActivation ! Activation(INIT_SIM_TICK)

      agent1.expectMessage(Activation(INIT_SIM_TICK))
      agent2.expectNoMessage()

      scheduler ! ScheduleActivation(
        agent2.ref,
        INIT_SIM_TICK,
      )

      // activation is sent right away
      agent2.expectMessage(Activation(INIT_SIM_TICK))

      scheduler ! Completion(agent2.ref)

      parent.expectNoMessage()

      scheduler ! Completion(agent1.ref)

      parent.expectMessage(Completion(schedulerActivation))
    }

    "scheduling with parent when earliest tick changes" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val agent1 = TestProbe[Activation]("agent_1")
      val agent2 = TestProbe[Activation]("agent_2")

      scheduler ! ScheduleActivation(agent1.ref, 10)

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe 10
      val schedulerActivation = sa1.actor

      scheduler ! ScheduleActivation(agent2.ref, INIT_SIM_TICK)
      parent.expectMessage(
        ScheduleActivation(schedulerActivation, INIT_SIM_TICK)
      )

      scheduler ! ScheduleActivation(agent2.ref, 5)
      parent.expectMessage(ScheduleActivation(schedulerActivation, 5))

      scheduler ! ScheduleActivation(agent2.ref, 11)
      // expect activation for earliest tick (of agent 1)
      parent.expectMessage(ScheduleActivation(schedulerActivation, 10))

      scheduler ! ScheduleActivation(agent2.ref, 20)
      // no update, 10 is still earliest
      parent.expectNoMessage()

      scheduler ! ScheduleActivation(agent2.ref, 10)
      parent.expectNoMessage()

      agent1.expectNoMessage()

      // TICK -1
      schedulerActivation ! Activation(10)

      agent1.expectMessage(Activation(10))
      agent2.expectMessage(Activation(10))
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
        INIT_SIM_TICK,
      )

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe INIT_SIM_TICK
      val schedulerActivation = sa1.actor

      scheduler ! ScheduleActivation(
        agent2.ref,
        INIT_SIM_TICK,
      )

      // TICK -1
      schedulerActivation ! Activation(INIT_SIM_TICK)

      agent1.expectActivationAndComplete(
        scheduler,
        INIT_SIM_TICK,
        Some(0),
      )

      parent.expectNoMessage()

      agent2.expectActivationAndComplete(
        scheduler,
        INIT_SIM_TICK,
        Some(0),
      )

      parent.expectMessage(Completion(schedulerActivation, Some(0)))

      agent1.expectNoMessage()
      agent2.expectNoMessage()

      // TICK 0
      schedulerActivation ! Activation(0)

      agent1.expectActivationAndComplete(
        scheduler,
        0,
        Some(300),
      )

      parent.expectNoMessage()

      agent2.expectActivationAndComplete(
        scheduler,
        0,
        Some(900),
      )

      parent.expectMessage(Completion(schedulerActivation, Some(300)))

      // TICK 300
      schedulerActivation ! Activation(300)

      agent1.expectActivationAndComplete(
        scheduler,
        300,
        Some(900),
      )

      parent.expectMessage(Completion(schedulerActivation, Some(900)))

      agent1.expectNoMessage()
      agent2.expectNoMessage()

      // TICK 900
      schedulerActivation ! Activation(900)

      agent1.expectActivationAndComplete(
        scheduler,
        900,
        Some(3600),
      )

      parent.expectNoMessage()

      agent2.expectActivationAndComplete(
        scheduler,
        900,
        Some(1800),
      )

      parent.expectMessage(Completion(schedulerActivation, Some(1800)))

      parent.expectNoMessage()
      agent1.expectNoMessage()
      agent2.expectNoMessage()
    }

    "five actors are getting activated for ten ticks" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val activatedAgents = (1 to 5)
        .map(i => TestProbe[Activation](s"agent_$i"))

      activatedAgents.foreach(actor =>
        // send to init activation to scheduler
        scheduler ! ScheduleActivation(
          actor.ref,
          INIT_SIM_TICK,
        )
      )

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe INIT_SIM_TICK
      val schedulerActivation = sa1.actor

      for (tick <- -1 to 8) {
        schedulerActivation ! Activation(tick)

        activatedAgents.foreach {
          _.expectActivationAndComplete(
            scheduler,
            tick,
            Some(tick + 1),
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

      // TICK 60
      schedulerActivation ! Activation(60)
      agent1.expectMessage(Activation(60))

      val key = UUID.randomUUID()
      scheduler ! ScheduleActivation(
        agent2.ref,
        120,
        Some(ScheduleKey(lock.ref, key)),
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
        Some(ScheduleKey(lock.ref, key)),
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
        Some(ScheduleKey(lock.ref, key)),
      )

      // lock should not receive unlock message by scheduler
      lock.expectNoMessage()

      // responsibility of unlocking forwarded to parent
      parent.expectMessage(
        ScheduleActivation(
          schedulerActivation,
          59,
          Some(ScheduleKey(lock.ref, key)),
        )
      )
    }

  }

  "The Scheduler should fail and stop" when {

    "activated if no activation is scheduled" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val agent1 = TestProbe[Activation]("agent_1")

      scheduler ! ScheduleActivation(agent1.ref, INIT_SIM_TICK)

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe INIT_SIM_TICK
      val schedulerActivation = sa1.actor

      // TICK -1
      schedulerActivation ! Activation(INIT_SIM_TICK)
      agent1.expectActivationAndComplete(scheduler, INIT_SIM_TICK, None)
      parent.expectMessage(Completion(schedulerActivation, None))

      // No activation scheduled, this should fail now
      schedulerActivation ! Activation(0)

      // scheduler stopped
      parent.expectTerminated(scheduler)
    }

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

      // TICK 0
      schedulerActivation ! Activation(0)

      // agent does not receive activation
      agent1.expectNoMessage()
      parent.expectNoMessage()

      // scheduler stopped
      parent.expectTerminated(scheduler)
    }

    "asked to schedule activation for the last tick while inactive" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val agent1 = TestProbe[Activation]("agent_1")

      scheduler ! ScheduleActivation(agent1.ref, 900)

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe 900
      val schedulerActivation = sa1.actor

      // TICK 900
      schedulerActivation ! Activation(900)

      agent1.expectActivationAndComplete(
        scheduler,
        900,
        Some(1800),
      )

      parent.expectMessage(Completion(schedulerActivation, Some(1800)))

      // now inactive again
      // can't schedule activation with tick equal to last tick (900) -> error
      // same result with any other past tick
      scheduler ! ScheduleActivation(agent1.ref, 900)

      // agent does not receive activation
      agent1.expectNoMessage()
      parent.expectNoMessage()

      // scheduler stopped
      parent.expectTerminated(scheduler)
    }

    "asked to schedule activation for a past tick while active" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val agent1 = TestProbe[Activation]("agent_1")

      scheduler ! ScheduleActivation(agent1.ref, 0)

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe 0
      val schedulerActivation = sa1.actor

      // TICK 0
      schedulerActivation ! Activation(0)
      agent1.expectMessage(Activation(0))

      // can't schedule activation for earlier tick than current active -> error
      scheduler ! ScheduleActivation(agent1.ref, INIT_SIM_TICK)

      // agent does not receive activation
      agent1.expectNoMessage()
      parent.expectNoMessage()

      // scheduler stopped
      parent.expectTerminated(scheduler)
    }

    "receiving unexpected completion message" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref)
      )

      val agent1 = TestProbe[Activation]("agent_1")
      val agent2 = TestProbe[Activation]("agent_1")

      scheduler ! ScheduleActivation(agent1.ref, 0)

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe 0
      val schedulerActivation = sa1.actor

      // TICK 0
      schedulerActivation ! Activation(0)
      agent1.expectMessage(Activation(0))

      // receiving completion for wrong actor
      scheduler ! Completion(agent2.ref)

      // parent does not receive completion
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

      // TICK -1
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

      // TICK -1
      schedulerActivation ! Activation(INIT_SIM_TICK)

      agent1.expectActivationAndComplete(
        scheduler,
        INIT_SIM_TICK,
        Some(0),
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
