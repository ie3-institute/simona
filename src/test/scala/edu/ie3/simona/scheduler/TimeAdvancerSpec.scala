/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe
}
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.RuntimeEvent._
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage._
import edu.ie3.simona.ontology.trigger.Trigger.ActivityStartTrigger
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike

/** Also tests [[RuntimeNotifier]]
  */
class TimeAdvancerSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with should.Matchers {

  "The TimeAdvancer should work correctly" when {

    "started checkWindow but without pauseTick" in {
      val simulation = TestProbe[SchedulerMessage]("simulation")
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref.toClassic,
          Some(listener.ref),
          Some(900),
          7200
        )
      )

      val trig1 = ActivityStartTrigger(INIT_SIM_TICK)
      timeAdvancer ! ScheduleTriggerMessage(
        trig1,
        scheduler.ref.toClassic
      )

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      // start simulation
      timeAdvancer ! StartScheduleMessage()

      // tick -1 is activated
      val tm1 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm1.trigger shouldBe trig1
      listener.expectMessage(Initializing)

      // tick -1 is completed
      val trig2 = ActivityStartTrigger(0)
      timeAdvancer ! CompletionMessage(
        tm1.triggerId,
        Some(ScheduleTriggerMessage(trig2, scheduler.ref.toClassic))
      )
      listener.expectMessageType[InitComplete]

      // tick 0 is activated automatically
      val tm2 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm2.trigger shouldBe trig2
      listener.expectMessage(Simulating(0, 7200))

      // tick 0 is completed
      val trig3 = ActivityStartTrigger(3600)
      timeAdvancer ! CompletionMessage(
        tm2.triggerId,
        Some(ScheduleTriggerMessage(trig3, scheduler.ref.toClassic))
      )
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 900
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 1800
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 2700

      // tick 3600 is activated automatically
      val tm3 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm3.trigger shouldBe trig3
      listener.expectNoMessage()

      // tick 3600 is completed
      val trig4 = ActivityStartTrigger(7200)
      timeAdvancer ! CompletionMessage(
        tm3.triggerId,
        Some(ScheduleTriggerMessage(trig4, scheduler.ref.toClassic))
      )
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 3600
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 4500
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 5400
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 6300

      // tick 7200 is activated automatically
      val tm4 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm4.trigger shouldBe trig4
      listener.expectNoMessage()

      // tick 7200 is completed
      timeAdvancer ! CompletionMessage(
        tm4.triggerId,
        None
      )
      val doneMsg = listener.expectMessageType[Done]
      doneMsg.tick shouldBe 7200
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe false

      simulation.expectMessage(SimulationSuccessfulMessage)
    }

    "started without checkWindow and pauseTick" in {
      val simulation = TestProbe[SchedulerMessage]("simulation")
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(simulation.ref.toClassic, Some(listener.ref), None, 3600)
      )

      val trig1 = ActivityStartTrigger(INIT_SIM_TICK)
      timeAdvancer ! ScheduleTriggerMessage(
        trig1,
        scheduler.ref.toClassic
      )

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      // start simulation
      timeAdvancer ! StartScheduleMessage()

      // tick -1 is activated
      val tm1 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm1.trigger shouldBe trig1
      listener.expectMessage(Initializing)

      // tick -1 is completed
      val trig2 = ActivityStartTrigger(0)
      timeAdvancer ! CompletionMessage(
        tm1.triggerId,
        Some(ScheduleTriggerMessage(trig2, scheduler.ref.toClassic))
      )
      listener.expectMessageType[InitComplete]

      // tick 0 is activated automatically
      val tm2 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm2.trigger shouldBe trig2
      listener.expectMessage(Simulating(0, 3600))

      // tick 0 is completed
      val trig3 = ActivityStartTrigger(3600)
      timeAdvancer ! CompletionMessage(
        tm2.triggerId,
        Some(ScheduleTriggerMessage(trig3, scheduler.ref.toClassic))
      )

      // tick 3600 is activated automatically
      val tm3 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm3.trigger shouldBe trig3
      listener.expectNoMessage()

      // tick 3600 is completed
      timeAdvancer ! CompletionMessage(
        tm3.triggerId
      )

      val doneMsg = listener.expectMessageType[Done]
      doneMsg.tick shouldBe 3600
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe false

      simulation.expectMessage(SimulationSuccessfulMessage)
    }

    "paused and started after initialization" in {
      val simulation = TestProbe[SchedulerMessage]("simulation")
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref.toClassic,
          Some(listener.ref),
          Some(900),
          3600
        )
      )
      val trig1 = ActivityStartTrigger(INIT_SIM_TICK)
      timeAdvancer ! ScheduleTriggerMessage(
        trig1,
        scheduler.ref.toClassic
      )

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      // start simulation
      timeAdvancer ! StartScheduleMessage(Some(INIT_SIM_TICK))

      // tick -1 is activated
      val tm1 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm1.trigger shouldBe trig1
      listener.expectMessage(Initializing)

      // tick -1 is completed
      val trig2 = ActivityStartTrigger(3600)
      timeAdvancer ! CompletionMessage(
        tm1.triggerId,
        Some(ScheduleTriggerMessage(trig2, scheduler.ref.toClassic))
      )
      listener.expectMessageType[InitComplete]
      listener.expectMessageType[Ready].tick shouldBe INIT_SIM_TICK

      // simulation should be paused
      scheduler.expectNoMessage()
      listener.expectNoMessage()

      // start again
      timeAdvancer ! StartScheduleMessage()

      // tick 3600 is activated
      val tm2 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm2.trigger shouldBe trig2
      listener.expectMessage(Simulating(0, 3600))

      // tick 3600 is completed
      timeAdvancer ! CompletionMessage(
        tm2.triggerId,
        None
      )
      // check window events should only come now, since we paused at -1 before
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 900
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 1800
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 2700

      val doneMsg = listener.expectMessageType[Done]
      doneMsg.tick shouldBe 3600
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe false

      simulation.expectMessage(SimulationSuccessfulMessage)
    }

    "paused and started and there is a gap between StartSchedule tick and next activation tick" in {
      val simulation = TestProbe[SchedulerMessage]("simulation")
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref.toClassic,
          Some(listener.ref),
          Some(900),
          5400
        )
      )
      val trig1 = ActivityStartTrigger(INIT_SIM_TICK)
      timeAdvancer ! ScheduleTriggerMessage(
        trig1,
        scheduler.ref.toClassic
      )

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      // start simulation
      timeAdvancer ! StartScheduleMessage(Some(3600))

      // tick -1 is activated
      val tm1 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm1.trigger shouldBe trig1
      listener.expectMessage(Initializing)

      // tick -1 is completed
      val trig2 = ActivityStartTrigger(0)
      timeAdvancer ! CompletionMessage(
        tm1.triggerId,
        Some(ScheduleTriggerMessage(trig2, scheduler.ref.toClassic))
      )
      listener.expectMessageType[InitComplete]

      // tick 0 is activated automatically
      val tm2 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm2.trigger shouldBe trig2
      listener.expectMessage(Simulating(0, 3600))

      // tick 0 is completed
      val trig3 = ActivityStartTrigger(5400)
      timeAdvancer ! CompletionMessage(
        tm2.triggerId,
        Some(ScheduleTriggerMessage(trig3, scheduler.ref.toClassic))
      )
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 900
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 1800
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 2700
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 3600
      listener.expectMessageType[Ready].tick shouldBe 3600

      // simulation should be paused
      scheduler.expectNoMessage()
      listener.expectNoMessage()

      // start again
      timeAdvancer ! StartScheduleMessage()

      // tick 5400 is activated
      val tm3 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm3.trigger shouldBe trig3
      listener.expectMessage(Simulating(3601, 5400))

      // tick 5400 is completed
      timeAdvancer ! CompletionMessage(
        tm3.triggerId,
        None
      )
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 4500
      val doneMsg = listener.expectMessageType[Done]
      doneMsg.tick shouldBe 5400
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe false

      simulation.expectMessage(SimulationSuccessfulMessage)
    }

    "paused and endTick - pauseScheduleAtTick == 1" in {
      val simulation = TestProbe[SchedulerMessage]("simulation")
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref.toClassic,
          Some(listener.ref),
          Some(900),
          3600
        )
      )
      val trig1 = ActivityStartTrigger(INIT_SIM_TICK)
      timeAdvancer ! ScheduleTriggerMessage(
        trig1,
        scheduler.ref.toClassic
      )

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      // start simulation
      timeAdvancer ! StartScheduleMessage(Some(3599))

      // tick -1 is activated
      val tm1 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm1.trigger shouldBe trig1
      listener.expectMessage(Initializing)

      // tick -1 is completed
      val trig2 = ActivityStartTrigger(0)
      timeAdvancer ! CompletionMessage(
        tm1.triggerId,
        Some(ScheduleTriggerMessage(trig2, scheduler.ref.toClassic))
      )
      listener.expectMessageType[InitComplete]

      // tick 0 is activated automatically
      val tm2 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm2.trigger shouldBe trig2
      listener.expectMessage(Simulating(0, 3599))

      // tick 0 is completed
      val trig3 = ActivityStartTrigger(3600)
      timeAdvancer ! CompletionMessage(
        tm2.triggerId,
        Some(ScheduleTriggerMessage(trig3, scheduler.ref.toClassic))
      )
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 900
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 1800
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 2700
      listener.expectMessageType[Ready].tick shouldBe 3599

      // simulation should be paused
      scheduler.expectNoMessage()
      listener.expectNoMessage()

      // start again
      timeAdvancer ! StartScheduleMessage()

      // tick 3600 is activated
      val tm3 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm3.trigger shouldBe trig3
      listener.expectMessage(Simulating(3600, 3600))

      // tick 3600 is completed
      timeAdvancer ! CompletionMessage(
        tm3.triggerId
      )
      val doneMsg = listener.expectMessageType[Done]
      doneMsg.tick shouldBe 3600
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe false

      simulation.expectMessage(SimulationSuccessfulMessage)
    }

    "activation has been scheduled after endTick" in {
      val simulation = TestProbe[SchedulerMessage]("simulation")
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref.toClassic,
          Some(listener.ref),
          Some(1800),
          3600
        )
      )
      val trig1 = ActivityStartTrigger(0)
      timeAdvancer ! ScheduleTriggerMessage(
        trig1,
        scheduler.ref.toClassic
      )

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      // start simulation
      timeAdvancer ! StartScheduleMessage()

      // tick 0 is activated
      val tm1 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm1.trigger shouldBe trig1
      listener.expectMessage(Simulating(0, 3600))

      // tick 0 is completed
      val trig2 = ActivityStartTrigger(3601)
      timeAdvancer ! CompletionMessage(
        tm1.triggerId,
        Some(ScheduleTriggerMessage(trig2, scheduler.ref.toClassic))
      )
      listener.expectMessageType[InitComplete]
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 1800

      // tick 3601 should not be activated!
      scheduler.expectNoMessage()

      val doneMsg = listener.expectMessageType[Done]
      doneMsg.tick shouldBe 3600
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe false

      simulation.expectMessage(SimulationSuccessfulMessage)
    }

    "no next trigger has been supplied" in {
      val simulation = TestProbe[SchedulerMessage]("simulation")
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref.toClassic,
          Some(listener.ref),
          Some(900),
          3600
        )
      )
      val trig1 = ActivityStartTrigger(0)
      timeAdvancer ! ScheduleTriggerMessage(
        trig1,
        scheduler.ref.toClassic
      )

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      // start simulation
      timeAdvancer ! StartScheduleMessage()

      // tick 0 is activated
      val tm1 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm1.trigger shouldBe trig1
      listener.expectMessage(Simulating(0, 3600))

      // tick 0 is completed
      timeAdvancer ! CompletionMessage(
        tm1.triggerId,
        None
      )
      listener.expectMessageType[InitComplete]
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 900
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 1800
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 2700

      // scheduler should not be activated!
      scheduler.expectNoMessage()

      val doneMsg = listener.expectMessageType[Done]
      doneMsg.tick shouldBe 3600
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe false

      simulation.expectMessage(SimulationSuccessfulMessage)
    }

    "endTick < pauseScheduleAtTick" in {
      val simulation = TestProbe[SchedulerMessage]("simulation")
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref.toClassic,
          Some(listener.ref),
          Some(1800),
          3600
        )
      )
      val trig1 = ActivityStartTrigger(0)
      timeAdvancer ! ScheduleTriggerMessage(
        trig1,
        scheduler.ref.toClassic
      )

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      // start simulation
      timeAdvancer ! StartScheduleMessage(Some(7200))

      // tick 0 is activated
      val tm1 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm1.trigger shouldBe trig1
      listener.expectMessage(Simulating(0, 3600))

      // tick 0 is completed
      timeAdvancer ! CompletionMessage(
        tm1.triggerId,
        None
      )
      listener.expectMessageType[InitComplete]
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 1800

      // scheduler should not be activated!
      scheduler.expectNoMessage()

      val doneMsg = listener.expectMessageType[Done]
      doneMsg.tick shouldBe 3600
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe false

      simulation.expectMessage(SimulationSuccessfulMessage)
    }

    "endTick == pauseScheduleAtTick" in {
      val simulation = TestProbe[SchedulerMessage]("simulation")
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref.toClassic,
          Some(listener.ref),
          Some(900),
          1800
        )
      )
      val trig1 = ActivityStartTrigger(0)
      timeAdvancer ! ScheduleTriggerMessage(
        trig1,
        scheduler.ref.toClassic
      )

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      // start simulation
      timeAdvancer ! StartScheduleMessage(Some(1800))

      // tick 0 is activated
      val tm1 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm1.trigger shouldBe trig1
      listener.expectMessage(Simulating(0, 1800))

      // tick 0 is completed
      timeAdvancer ! CompletionMessage(
        tm1.triggerId,
        None
      )
      listener.expectMessageType[InitComplete]
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 900

      // scheduler should not be activated!
      scheduler.expectNoMessage()

      val doneMsg = listener.expectMessageType[Done]
      doneMsg.tick shouldBe 1800
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe false

      simulation.expectMessage(SimulationSuccessfulMessage)
    }
  }

  "The TimeAdvancer should fail and stop" when {

    "wrong next tick has been supplied" in {
      val simulation = TestProbe[SchedulerMessage]("simulation")
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref.toClassic,
          Some(listener.ref),
          Some(900),
          1800
        )
      )
      val trig1 = ActivityStartTrigger(0)
      timeAdvancer ! ScheduleTriggerMessage(
        trig1,
        scheduler.ref.toClassic
      )

      // start simulation
      timeAdvancer ! StartScheduleMessage(Some(1800))

      // tick 0 is activated
      val tm1 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm1.trigger shouldBe trig1
      listener.expectMessage(Simulating(0, 1800))

      // tick 0 is completed
      // INIT_SIM_TICK is earlier than 0, should fail
      val trig2 = ActivityStartTrigger(INIT_SIM_TICK)
      timeAdvancer ! CompletionMessage(
        tm1.triggerId,
        Some(ScheduleTriggerMessage(trig2, scheduler.ref.toClassic))
      )
      listener.expectMessageType[Error].errMsg should include(
        "tick -1, although current active tick was 0"
      )
      val doneMsg = listener.expectMessageType[Done]
      doneMsg.tick shouldBe 0
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe true

      // scheduler should not be activated!
      scheduler.expectNoMessage()

      scheduler.expectTerminated(timeAdvancer)

      simulation.expectMessage(SimulationFailureMessage)
    }

    "activation is completed with wrong triggerId" in {
      val simulation = TestProbe[SchedulerMessage]("simulation")
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref.toClassic,
          Some(listener.ref),
          Some(900),
          1800
        )
      )
      val trig1 = ActivityStartTrigger(0)
      timeAdvancer ! ScheduleTriggerMessage(
        trig1,
        scheduler.ref.toClassic
      )

      // start simulation
      timeAdvancer ! StartScheduleMessage()

      // tick 0 is activated
      val tm1 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm1.trigger shouldBe trig1
      listener.expectMessage(Simulating(0, 1800))

      // tick 0 is completed
      val trig2 = ActivityStartTrigger(INIT_SIM_TICK)
      timeAdvancer ! CompletionMessage(
        tm1.triggerId + 1, // WRONG trigger id
        Some(ScheduleTriggerMessage(trig2, scheduler.ref.toClassic))
      )
      listener.expectMessageType[Error].errMsg should include("was expected")
      val doneMsg = listener.expectMessageType[Done]
      doneMsg.tick shouldBe 0
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe true

      // scheduler should not be activated!
      scheduler.expectNoMessage()

      scheduler.expectTerminated(timeAdvancer)

      simulation.expectMessage(SimulationFailureMessage)
    }

    "receiving error message while uninitialized" in {
      val simulation = TestProbe[SchedulerMessage]("simulation")
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(simulation.ref.toClassic, Some(listener.ref), None, 1800)
      )

      // Send stop message
      timeAdvancer ! Stop("Test message")

      // we cannot check the console, thus just check if time advancer died
      scheduler.expectTerminated(timeAdvancer)

      simulation.expectMessage(SimulationFailureMessage)
    }

    "receiving error message while inactive" in {
      val simulation = TestProbe[SchedulerMessage]("simulation")
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(simulation.ref.toClassic, Some(listener.ref), None, 1800)
      )
      val trig1 = ActivityStartTrigger(1)
      timeAdvancer ! ScheduleTriggerMessage(
        trig1,
        scheduler.ref.toClassic
      )

      // Send stop message
      timeAdvancer ! Stop("Test message")

      listener.expectMessageType[Error].errMsg should include("Test message")
      val doneMsg = listener.expectMessageType[Done]
      doneMsg.tick shouldBe 1
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe true

      scheduler.expectTerminated(timeAdvancer)

      simulation.expectMessage(SimulationFailureMessage)
    }

    "receiving error message while active" in {
      val simulation = TestProbe[SchedulerMessage]("simulation")
      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref.toClassic,
          Some(listener.ref),
          Some(900),
          1800
        )
      )
      val trig1 = ActivityStartTrigger(0)
      timeAdvancer ! ScheduleTriggerMessage(
        trig1,
        scheduler.ref.toClassic
      )

      // start simulation
      timeAdvancer ! StartScheduleMessage()

      // tick 0 is activated
      val tm1 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm1.trigger shouldBe trig1
      listener.expectMessage(Simulating(0, 1800))

      // Send stop message
      timeAdvancer ! Stop("Test message")

      listener.expectMessageType[Error].errMsg should include("Test message")
      val doneMsg = listener.expectMessageType[Done]
      doneMsg.tick shouldBe 0
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe true

      scheduler.expectTerminated(timeAdvancer)

      simulation.expectMessage(SimulationFailureMessage)
    }

  }
}
