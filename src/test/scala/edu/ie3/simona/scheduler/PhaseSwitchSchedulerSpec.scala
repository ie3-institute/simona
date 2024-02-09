/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.core.PhaseSwitchCore
import edu.ie3.simona.util.ActorUtils.RichActivatedActor
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike

/** A lot of functions are already tested in [[SchedulerSpec]] and don't need to
  * be repeated here
  */
class PhaseSwitchSchedulerSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with should.Matchers {

  "The Scheduler with PhaseSwitchCore should work correctly" when {

    "receiving initial activation scheduling before activation" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref, PhaseSwitchCore)
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
      parent.expectNoMessage()

      // TICK -1, phase 0
      schedulerActivation ! Activation(INIT_SIM_TICK)

      agent1.expectMessage(Activation(INIT_SIM_TICK))
      agent2.expectNoMessage()

      // TICK -1, phase 1
      scheduler ! Completion(agent1.ref)

      agent1.expectNoMessage()
      agent2.expectMessage(Activation(INIT_SIM_TICK))

      parent.expectNoMessage()

      scheduler ! Completion(agent2.ref)

      parent.expectMessage(Completion(schedulerActivation))
    }

    "receiving activation scheduling in a different order than initially" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref, PhaseSwitchCore)
      )

      val agent1 = TestProbe[Activation]("agent_1")
      val agent2 = TestProbe[Activation]("agent_2")

      scheduler ! ScheduleActivation(agent1.ref, INIT_SIM_TICK)

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe INIT_SIM_TICK
      val schedulerActivation = sa1.actor

      scheduler ! ScheduleActivation(agent2.ref, INIT_SIM_TICK)

      // TICK -1, phase 0
      schedulerActivation ! Activation(INIT_SIM_TICK)
      agent1.expectMessage(Activation(INIT_SIM_TICK))

      // TICK -1, phase 1
      scheduler ! Completion(agent1.ref)
      agent2.expectMessage(Activation(INIT_SIM_TICK))

      scheduler ! Completion(agent2.ref, Some(0))
      parent.expectMessage(Completion(schedulerActivation, Some(0)))

      // scheduling first actor after the second actor for the same tick
      scheduler ! ScheduleActivation(agent1.ref, 0)
      parent.expectNoMessage()

      // TICK 0, phase 0
      schedulerActivation ! Activation(0)

      agent1.expectMessage(Activation(0))
      agent2.expectNoMessage()

      // TICK -1, phase 1
      scheduler ! Completion(agent1.ref)

      agent1.expectNoMessage()
      agent2.expectMessage(Activation(0))

      scheduler ! Completion(agent2.ref)

      parent.expectMessage(Completion(schedulerActivation))
    }

    "receiving activation scheduling after init activation" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref, PhaseSwitchCore)
      )

      val agent1 = TestProbe[Activation]("agent_1")
      val agent2 = TestProbe[Activation]("agent_2")

      scheduler ! ScheduleActivation(agent1.ref, INIT_SIM_TICK)

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe INIT_SIM_TICK
      val schedulerActivation = sa1.actor

      agent1.expectNoMessage()

      // TICK -1, phase 0
      schedulerActivation ! Activation(INIT_SIM_TICK)

      agent1.expectMessage(Activation(INIT_SIM_TICK))
      agent2.expectNoMessage()

      scheduler ! ScheduleActivation(
        agent2.ref,
        INIT_SIM_TICK,
      )

      // waiting for next phase
      agent2.expectNoMessage()
      parent.expectNoMessage()

      // TICK -1, phase 1
      scheduler ! Completion(agent1.ref)

      agent2.expectMessage(Activation(INIT_SIM_TICK))

      scheduler ! Completion(agent2.ref)

      parent.expectMessage(Completion(schedulerActivation))
    }

    "scheduling with parent when earliest tick changes" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref, PhaseSwitchCore)
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

      // TICK 10, phase 0
      schedulerActivation ! Activation(10)

      agent1.expectMessage(Activation(10))
      agent2.expectNoMessage()

      // TICK 10, phase 1
      scheduler ! Completion(agent1.ref)

      agent2.expectMessage(Activation(10))
    }

    "scheduling two actors for different ticks" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref, PhaseSwitchCore)
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

      agent2.expectNoMessage()
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
        Some(900),
      )

      parent.expectNoMessage()

      agent2.expectActivationAndComplete(
        scheduler,
        0,
        Some(300),
      )

      parent.expectMessage(Completion(schedulerActivation, Some(300)))

      // TICK 300
      schedulerActivation ! Activation(300)

      agent2.expectActivationAndComplete(
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

  }

  "The Scheduler should fail and stop" when {

    "activated with wrong tick" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref, PhaseSwitchCore)
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

    "asked to schedule activation for a past tick while inactive" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref, PhaseSwitchCore)
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
      // can't schedule activation with earlier tick than last tick (900) -> error
      scheduler ! ScheduleActivation(agent1.ref, INIT_SIM_TICK)

      // agent does not receive activation
      agent1.expectNoMessage()
      parent.expectNoMessage()

      // scheduler stopped
      parent.expectTerminated(scheduler)
    }

    "asked to schedule activation for a past tick while active" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref, PhaseSwitchCore)
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

    "asked to schedule activation for a past phase in the current tick" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref, PhaseSwitchCore)
      )

      val agent1 = TestProbe[Activation]("agent_1")
      val agent2 = TestProbe[Activation]("agent_2")

      scheduler ! ScheduleActivation(agent1.ref, 0)

      val sa1 = parent.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe 0
      val schedulerActivation = sa1.actor

      scheduler ! ScheduleActivation(agent2.ref, 0)

      // TICK 0, phase 0
      schedulerActivation ! Activation(0)
      agent1.expectMessage(Activation(0))

      // TICK 0, phase 1
      scheduler ! Completion(agent1.ref)
      agent2.expectMessage(Activation(0))

      // schedule first actor again, this is not allowed
      scheduler ! ScheduleActivation(agent1.ref, 0)

      // agent does not receive activation
      agent1.expectNoMessage()
      parent.expectNoMessage()

      // scheduler stopped
      parent.expectTerminated(scheduler)
    }

    "receiving unexpected completion message" in {
      val parent = TestProbe[SchedulerMessage]("parent")
      val scheduler = spawn(
        Scheduler(parent.ref, PhaseSwitchCore)
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
  }
}
