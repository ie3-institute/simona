/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.scaladsl.adapter.TypedActorRefOps
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.RuntimeEvent._
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessageTyped.{
  Completion,
  ScheduleActivation
}
import edu.ie3.simona.scheduler.TimeAdvancer.{StartSimMessage, Stop}
import edu.ie3.simona.sim.SimMessage
import edu.ie3.simona.sim.SimMessage.{
  SimulationFailureMessage,
  SimulationSuccessfulMessage
}
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
      val simulation = TestProbe[SimMessage]("simulation")
      val scheduler = TestProbe[Activation]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref.toClassic,
          Some(listener.ref),
          Some(900),
          7200
        )
      )

      timeAdvancer ! ScheduleActivation(scheduler.ref, INIT_SIM_TICK)

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      // start simulation
      timeAdvancer ! StartSimMessage()

      // tick -1 is activated
      scheduler.expectMessage(Activation(INIT_SIM_TICK))
      listener.expectMessage(Initializing)

      // tick -1 is completed
      timeAdvancer ! Completion(scheduler.ref, Some(0))
      listener.expectMessageType[InitComplete]

      // tick 0 is activated automatically
      scheduler.expectMessage(Activation(0))
      listener.expectMessage(Simulating(0, 7200))

      // tick 0 is completed
      timeAdvancer ! Completion(scheduler.ref, Some(3600))
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 900
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 1800
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 2700

      // tick 3600 is activated automatically
      scheduler.expectMessage(Activation(3600))
      listener.expectNoMessage()

      // tick 3600 is completed
      timeAdvancer ! Completion(scheduler.ref, Some(7200))
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 3600
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 4500
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 5400
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 6300

      // tick 7200 is activated automatically
      scheduler.expectMessage(Activation(7200))
      listener.expectNoMessage()

      // tick 7200 is completed
      timeAdvancer ! Completion(scheduler.ref)
      val doneMsg = listener.expectMessageType[Done]
      doneMsg.tick shouldBe 7200
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe false

      simulation.expectMessage(SimulationSuccessfulMessage)
    }

    "started without checkWindow and pauseTick" in {
      val simulation = TestProbe[SimMessage]("simulation")
      val scheduler = TestProbe[Activation]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(simulation.ref.toClassic, Some(listener.ref), None, 3600)
      )

      timeAdvancer ! ScheduleActivation(scheduler.ref, INIT_SIM_TICK)

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      // start simulation
      timeAdvancer ! StartSimMessage()

      // tick -1 is activated
      scheduler.expectMessage(Activation(INIT_SIM_TICK))
      listener.expectMessage(Initializing)

      // tick -1 is completed
      timeAdvancer ! Completion(scheduler.ref, Some(0))
      listener.expectMessageType[InitComplete]

      // tick 0 is activated automatically
      scheduler.expectMessage(Activation(0))
      listener.expectMessage(Simulating(0, 3600))

      // tick 0 is completed
      timeAdvancer ! Completion(scheduler.ref, Some(3600))

      // tick 3600 is activated automatically
      scheduler.expectMessage(Activation(3600))
      listener.expectNoMessage()

      // tick 3600 is completed
      timeAdvancer ! Completion(scheduler.ref)

      val doneMsg = listener.expectMessageType[Done]
      doneMsg.tick shouldBe 3600
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe false

      simulation.expectMessage(SimulationSuccessfulMessage)
    }

    "paused and started after initialization" in {
      val simulation = TestProbe[SimMessage]("simulation")
      val scheduler = TestProbe[Activation]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref.toClassic,
          Some(listener.ref),
          Some(900),
          3600
        )
      )

      timeAdvancer ! ScheduleActivation(scheduler.ref, INIT_SIM_TICK)

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      // start simulation
      timeAdvancer ! StartSimMessage(Some(INIT_SIM_TICK))

      // tick -1 is activated
      scheduler.expectMessage(Activation(INIT_SIM_TICK))
      listener.expectMessage(Initializing)

      // tick -1 is completed
      timeAdvancer ! Completion(scheduler.ref, Some(3600))
      listener.expectMessageType[InitComplete]
      listener.expectMessageType[Ready].tick shouldBe INIT_SIM_TICK

      // simulation should be paused
      scheduler.expectNoMessage()
      listener.expectNoMessage()

      // start again
      timeAdvancer ! StartSimMessage()

      // tick 3600 is activated
      scheduler.expectMessage(Activation(3600))
      listener.expectMessage(Simulating(0, 3600))

      // tick 3600 is completed
      timeAdvancer ! Completion(scheduler.ref)
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
      val simulation = TestProbe[SimMessage]("simulation")
      val scheduler = TestProbe[Activation]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref.toClassic,
          Some(listener.ref),
          Some(900),
          5400
        )
      )
      timeAdvancer ! ScheduleActivation(scheduler.ref, INIT_SIM_TICK)

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      // start simulation
      timeAdvancer ! StartSimMessage(Some(3600))

      // tick -1 is activated
      scheduler.expectMessage(Activation(INIT_SIM_TICK))
      listener.expectMessage(Initializing)

      // tick -1 is completed
      timeAdvancer ! Completion(scheduler.ref, Some(0))
      listener.expectMessageType[InitComplete]

      // tick 0 is activated automatically
      scheduler.expectMessage(Activation(0))
      listener.expectMessage(Simulating(0, 3600))

      // tick 0 is completed
      timeAdvancer ! Completion(scheduler.ref, Some(5400))
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 900
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 1800
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 2700
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 3600
      listener.expectMessageType[Ready].tick shouldBe 3600

      // simulation should be paused
      scheduler.expectNoMessage()
      listener.expectNoMessage()

      // start again
      timeAdvancer ! StartSimMessage()

      // tick 5400 is activated
      scheduler.expectMessage(Activation(5400))
      listener.expectMessage(Simulating(3601, 5400))

      // tick 5400 is completed
      timeAdvancer ! Completion(scheduler.ref)
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 4500
      val doneMsg = listener.expectMessageType[Done]
      doneMsg.tick shouldBe 5400
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe false

      simulation.expectMessage(SimulationSuccessfulMessage)
    }

    "paused and endTick - pauseScheduleAtTick == 1" in {
      val simulation = TestProbe[SimMessage]("simulation")
      val scheduler = TestProbe[Activation]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref.toClassic,
          Some(listener.ref),
          Some(900),
          3600
        )
      )
      timeAdvancer ! ScheduleActivation(scheduler.ref, INIT_SIM_TICK)

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      // start simulation
      timeAdvancer ! StartSimMessage(Some(3599))

      // tick -1 is activated
      scheduler.expectMessage(Activation(INIT_SIM_TICK))
      listener.expectMessage(Initializing)

      // tick -1 is completed
      timeAdvancer ! Completion(scheduler.ref, Some(0))
      listener.expectMessageType[InitComplete]

      // tick 0 is activated automatically
      scheduler.expectMessage(Activation(0))
      listener.expectMessage(Simulating(0, 3599))

      // tick 0 is completed
      timeAdvancer ! Completion(scheduler.ref, Some(3600))
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 900
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 1800
      listener.expectMessageType[CheckWindowPassed].tick shouldBe 2700
      listener.expectMessageType[Ready].tick shouldBe 3599

      // simulation should be paused
      scheduler.expectNoMessage()
      listener.expectNoMessage()

      // start again
      timeAdvancer ! StartSimMessage()

      // tick 3600 is activated
      scheduler.expectMessage(Activation(3600))
      listener.expectMessage(Simulating(3600, 3600))

      // tick 3600 is completed
      timeAdvancer ! Completion(scheduler.ref)
      val doneMsg = listener.expectMessageType[Done]
      doneMsg.tick shouldBe 3600
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe false

      simulation.expectMessage(SimulationSuccessfulMessage)
    }

    "activation has been scheduled after endTick" in {
      val simulation = TestProbe[SimMessage]("simulation")
      val scheduler = TestProbe[Activation]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref.toClassic,
          Some(listener.ref),
          Some(1800),
          3600
        )
      )
      timeAdvancer ! ScheduleActivation(scheduler.ref, 0)

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      // start simulation
      timeAdvancer ! StartSimMessage()

      // tick 0 is activated
      scheduler.expectMessage(Activation(0))
      listener.expectMessage(Simulating(0, 3600))

      // tick 0 is completed
      timeAdvancer ! Completion(scheduler.ref, Some(3601))
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
      val simulation = TestProbe[SimMessage]("simulation")
      val scheduler = TestProbe[Activation]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref.toClassic,
          Some(listener.ref),
          Some(900),
          3600
        )
      )
      timeAdvancer ! ScheduleActivation(scheduler.ref, 0)

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      // start simulation
      timeAdvancer ! StartSimMessage()

      // tick 0 is activated
      scheduler.expectMessage(Activation(0))
      listener.expectMessage(Simulating(0, 3600))

      // tick 0 is completed
      timeAdvancer ! Completion(scheduler.ref)
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
      val simulation = TestProbe[SimMessage]("simulation")
      val scheduler = TestProbe[Activation]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref.toClassic,
          Some(listener.ref),
          Some(1800),
          3600
        )
      )
      timeAdvancer ! ScheduleActivation(scheduler.ref, 0)

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      // start simulation
      timeAdvancer ! StartSimMessage(Some(7200))

      // tick 0 is activated
      scheduler.expectMessage(Activation(0))
      listener.expectMessage(Simulating(0, 3600))

      // tick 0 is completed
      timeAdvancer ! Completion(scheduler.ref)
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
      val simulation = TestProbe[SimMessage]("simulation")
      val scheduler = TestProbe[Activation]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref.toClassic,
          Some(listener.ref),
          Some(900),
          1800
        )
      )
      timeAdvancer ! ScheduleActivation(scheduler.ref, 0)

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      // start simulation
      timeAdvancer ! StartSimMessage(Some(1800))

      // tick 0 is activated
      scheduler.expectMessage(Activation(0))
      listener.expectMessage(Simulating(0, 1800))

      // tick 0 is completed
      timeAdvancer ! Completion(scheduler.ref)
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
      val simulation = TestProbe[SimMessage]("simulation")
      val scheduler = TestProbe[Activation]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref.toClassic,
          Some(listener.ref),
          Some(900),
          1800
        )
      )
      timeAdvancer ! ScheduleActivation(scheduler.ref, 0)

      // start simulation
      timeAdvancer ! StartSimMessage(Some(1800))

      // tick 0 is activated
      scheduler.expectMessage(Activation(0))
      listener.expectMessage(Simulating(0, 1800))

      // tick 0 is completed
      // INIT_SIM_TICK is earlier than 0, should fail
      timeAdvancer ! Completion(scheduler.ref, Some(INIT_SIM_TICK))
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

    "receiving error message while uninitialized" in {
      val simulation = TestProbe[SimMessage]("simulation")
      val scheduler = TestProbe[Activation]("scheduler")
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
      val simulation = TestProbe[SimMessage]("simulation")
      val scheduler = TestProbe[Activation]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(simulation.ref.toClassic, Some(listener.ref), None, 1800)
      )
      timeAdvancer ! ScheduleActivation(scheduler.ref, 1)

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
      val simulation = TestProbe[SimMessage]("simulation")
      val scheduler = TestProbe[Activation]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref.toClassic,
          Some(listener.ref),
          Some(900),
          1800
        )
      )
      timeAdvancer ! ScheduleActivation(scheduler.ref, 0)

      // start simulation
      timeAdvancer ! StartSimMessage()

      // tick 0 is activated
      scheduler.expectMessage(Activation(0))
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
