/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.RuntimeEvent._
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.scheduler.TimeAdvancer.Start
import edu.ie3.simona.sim.SimonaSim
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike

/** Also tests [[RuntimeNotifier]]
  */
class TimeAdvancerSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with should.Matchers {

  /** A high duration in milliseconds for RuntimeEvent duration checking. The
    * tests should under no circumstance take longer than ten minutes.
    */
  private val maxEventDuration: Long = 10 * 60 * 1000

  "The TimeAdvancer should work correctly" when {

    "started with checkWindow" in {
      val simulation = TestProbe[SimonaSim.Request]("simulation")
      val scheduler = TestProbe[Activation]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref,
          Some(listener.ref),
          Some(900),
          7200,
        )
      )

      timeAdvancer ! ScheduleActivation(scheduler.ref, INIT_SIM_TICK)

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      // start simulation
      timeAdvancer ! Start

      // tick -1 is activated
      scheduler.expectMessage(Activation(INIT_SIM_TICK))
      listener.expectMessage(Initializing)

      // tick -1 is completed
      timeAdvancer ! Completion(scheduler.ref, Some(0))
      listener.expectMessageType[InitComplete] match {
        case InitComplete(duration) =>
          duration should (be >= 0L and be < maxEventDuration)
      }

      // tick 0 is activated
      scheduler.expectMessage(Activation(0))
      listener.expectMessage(Simulating(0, 7200))

      // tick 0 is completed
      timeAdvancer ! Completion(scheduler.ref, Some(3600))
      listener.expectMessageType[CheckWindowPassed] match {
        case CheckWindowPassed(tick, duration) =>
          tick shouldBe 900
          duration should (be >= 0L and be < maxEventDuration)
      }
      listener.expectMessageType[CheckWindowPassed] match {
        case CheckWindowPassed(tick, duration) =>
          tick shouldBe 1800
          duration should (be >= 0L and be < maxEventDuration)
      }
      listener.expectMessageType[CheckWindowPassed] match {
        case CheckWindowPassed(tick, duration) =>
          tick shouldBe 2700
          duration should (be >= 0L and be < maxEventDuration)
      }

      // tick 3600 is activated
      scheduler.expectMessage(Activation(3600))
      listener.expectNoMessage()

      // tick 3600 is completed
      timeAdvancer ! Completion(scheduler.ref, Some(7200))
      listener.expectMessageType[CheckWindowPassed] match {
        case CheckWindowPassed(tick, duration) =>
          tick shouldBe 3600
          duration should (be >= 0L and be < maxEventDuration)
      }
      listener.expectMessageType[CheckWindowPassed] match {
        case CheckWindowPassed(tick, duration) =>
          tick shouldBe 4500
          duration should (be >= 0L and be < maxEventDuration)
      }
      listener.expectMessageType[CheckWindowPassed] match {
        case CheckWindowPassed(tick, duration) =>
          tick shouldBe 5400
          duration should (be >= 0L and be < maxEventDuration)
      }
      listener.expectMessageType[CheckWindowPassed] match {
        case CheckWindowPassed(tick, duration) =>
          tick shouldBe 6300
          duration should (be >= 0L and be < maxEventDuration)
      }

      // tick 7200 is activated
      scheduler.expectMessage(Activation(7200))
      listener.expectNoMessage()

      // tick 7200 is completed
      timeAdvancer ! Completion(scheduler.ref)
      listener.expectMessageType[Done] match {
        case Done(tick, duration, errorInSim) =>
          tick shouldBe 7200
          duration should (be >= 0L and be < maxEventDuration)
          errorInSim shouldBe false
      }

      simulation.expectMessage(SimonaSim.SimulationEnded)
    }

    "activation has been scheduled after endTick" in {
      val simulation = TestProbe[SimonaSim.Request]("simulation")
      val scheduler = TestProbe[Activation]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref,
          Some(listener.ref),
          Some(1800),
          3600,
        )
      )
      timeAdvancer ! ScheduleActivation(scheduler.ref, 0)

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      // start simulation
      timeAdvancer ! Start

      // tick 0 is activated
      scheduler.expectMessage(Activation(0))
      listener.expectMessage(Simulating(0, 3600))

      // tick 0 is completed
      timeAdvancer ! Completion(scheduler.ref, Some(3601))
      listener.expectMessageType[InitComplete] match {
        case InitComplete(duration) =>
          duration should (be >= 0L and be < maxEventDuration)
      }
      listener.expectMessageType[CheckWindowPassed] match {
        case CheckWindowPassed(tick, duration) =>
          tick shouldBe 1800
          duration should (be >= 0L and be < maxEventDuration)
      }

      // tick 3601 should not be activated!
      scheduler.expectNoMessage()

      listener.expectMessageType[Done] match {
        case Done(tick, duration, errorInSim) =>
          tick shouldBe 3600
          duration should (be >= 0L and be < maxEventDuration)
          errorInSim shouldBe false
      }

      simulation.expectMessage(SimonaSim.SimulationEnded)
    }

    "no next trigger has been supplied" in {
      val simulation = TestProbe[SimonaSim.Request]("simulation")
      val scheduler = TestProbe[Activation]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref,
          Some(listener.ref),
          Some(900),
          3600,
        )
      )
      timeAdvancer ! ScheduleActivation(scheduler.ref, 0)

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      // start simulation
      timeAdvancer ! Start

      // tick 0 is activated
      scheduler.expectMessage(Activation(0))
      listener.expectMessage(Simulating(0, 3600))

      // tick 0 is completed
      timeAdvancer ! Completion(scheduler.ref)
      listener.expectMessageType[InitComplete] match {
        case InitComplete(duration) =>
          duration should (be >= 0L and be < maxEventDuration)
      }
      listener.expectMessageType[CheckWindowPassed] match {
        case CheckWindowPassed(tick, duration) =>
          tick shouldBe 900
          duration should (be >= 0L and be < maxEventDuration)
      }
      listener.expectMessageType[CheckWindowPassed] match {
        case CheckWindowPassed(tick, duration) =>
          tick shouldBe 1800
          duration should (be >= 0L and be < maxEventDuration)
      }
      listener.expectMessageType[CheckWindowPassed] match {
        case CheckWindowPassed(tick, duration) =>
          tick shouldBe 2700
          duration should (be >= 0L and be < maxEventDuration)
      }

      // scheduler should not be activated!
      scheduler.expectNoMessage()

      listener.expectMessageType[Done] match {
        case Done(tick, duration, errorInSim) =>
          tick shouldBe 3600
          duration should (be >= 0L and be < maxEventDuration)
          errorInSim shouldBe false
      }

      simulation.expectMessage(SimonaSim.SimulationEnded)
    }

  }

  "The TimeAdvancer should fail and stop" when {

    "wrong next tick has been supplied" in {
      val simulation = TestProbe[SimonaSim.Request]("simulation")
      val scheduler = TestProbe[Activation]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(
          simulation.ref,
          Some(listener.ref),
          Some(900),
          1800,
        )
      )
      timeAdvancer ! ScheduleActivation(scheduler.ref, 0)

      // start simulation
      timeAdvancer ! Start

      // tick 0 is activated
      scheduler.expectMessage(Activation(0))
      listener.expectMessage(Simulating(0, 1800))

      // tick 0 is completed
      // INIT_SIM_TICK is earlier than 0, should fail
      timeAdvancer ! Completion(scheduler.ref, Some(INIT_SIM_TICK))
      listener.expectMessageType[Error].errMsg should include(
        "tick -1, although current active tick was 0"
      )
      listener.expectMessageType[Done] match {
        case Done(tick, duration, errorInSim) =>
          tick shouldBe 0
          duration should (be >= 0L and be < maxEventDuration)
          errorInSim shouldBe true
      }

      // scheduler should not be activated!
      scheduler.expectNoMessage()

      scheduler.expectTerminated(timeAdvancer)

      // No SimulationEnded message
      simulation.expectNoMessage()
    }

  }
}
