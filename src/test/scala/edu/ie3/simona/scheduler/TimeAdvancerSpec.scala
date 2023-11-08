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
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage._
import edu.ie3.simona.ontology.trigger.Trigger.ActivityStartTrigger
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike

class TimeAdvancerSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with should.Matchers {

  "The TimeAdvancer" should {

    "finish simulation correctly without pauseTick" in {

      val scheduler = TestProbe[SchedulerMessage]("scheduler")
      val listener = TestProbe[RuntimeEvent]("listener")

      val timeAdvancer = spawn(
        TimeAdvancer(Some(listener.ref), 900, 7200)
      )
      timeAdvancer ! ScheduleTriggerMessage(
        ActivityStartTrigger(INIT_SIM_TICK),
        scheduler.ref.toClassic
      )

      listener.expectNoMessage()
      scheduler.expectNoMessage()

      timeAdvancer ! StartScheduleMessage()
      listener.expectMessage(Initializing)
      val tm1 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm1.trigger shouldBe ActivityStartTrigger(-1)

      val trig2 = ActivityStartTrigger(0)
      timeAdvancer ! CompletionMessage(
        tm1.triggerId,
        Some(ScheduleTriggerMessage(trig2, scheduler.ref.toClassic))
      )
      // tick 0 is activated
      val tm2 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm2.trigger shouldBe trig2
      listener.expectMessageType[InitComplete]
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

      // tick 3600 is activated
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

      // tick 7200 is activated
      val tm4 = scheduler.expectMessageType[TriggerWithIdMessage]
      tm4.trigger shouldBe trig4
      listener.expectNoMessage()

      // tick 7200 is completed
      timeAdvancer ! CompletionMessage(
        tm3.triggerId,
        None
      )
      val doneMsg = listener.expectMessageType[Done]
      doneMsg.tick shouldBe 7200
      doneMsg.noOfFailedPF shouldBe 0
      doneMsg.errorInSim shouldBe false

    }

  }
}
