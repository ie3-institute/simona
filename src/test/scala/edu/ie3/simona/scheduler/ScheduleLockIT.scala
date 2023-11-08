/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.scaladsl.adapter.TypedActorRefOps
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.trigger.Trigger.ActivityStartTrigger
import edu.ie3.simona.scheduler.ScheduleLock.InitLock
import edu.ie3.simona.util.ActorUtils.RichTriggeredAgent
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.UUID

class ScheduleLockIT
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with should.Matchers {

  "The ScheduleLock in conjunction with schedulers" should {

    "work as expected when schedulers active" in {
      val timeAdvancer = TestProbe[SchedulerMessage]("timeAdvancer")
      val agent1 = TestProbe[TriggerWithIdMessage]("agent1")
      val agent2 = TestProbe[TriggerWithIdMessage]("agent2")

      val key = UUID.randomUUID()

      val parentScheduler = spawn(
        Scheduler(timeAdvancer.ref),
        "parentScheduler"
      )
      val childScheduler = spawn(
        Scheduler(parentScheduler),
        "childScheduler"
      )

      // first, we normally schedule some activation for our agent1
      childScheduler ! ScheduleTriggerMessage(
        ActivityStartTrigger(30),
        agent1.ref.toClassic
      )
      timeAdvancer.expectMessage(
        ScheduleTriggerMessage(
          ActivityStartTrigger(30),
          parentScheduler.toClassic
        )
      )

      // create and initialize lock
      val scheduleLock = spawn(
        ScheduleLock(Set(key)),
        "lock"
      )
      parentScheduler ! ScheduleTriggerMessage(
        InitLock(parentScheduler, 30),
        scheduleLock.toClassic
      )
      agent1.expectNoMessage()

      // activate the scheduler, lock should now initialize
      val triggerId = 3
      parentScheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(30),
        triggerId
      )

      // schedule activation for agent2, lock should get unlocked
      childScheduler ! ScheduleTriggerMessage(
        ActivityStartTrigger(30),
        agent2.ref.toClassic,
        Some(scheduleLock, key)
      )

      // because of activated agents, child/parentScheduler should not be able to complete yet
      timeAdvancer.expectNoMessage()

      // completing agent activations
      agent1.expectTriggerAndComplete(
        childScheduler,
        30
      )
      agent2.expectTriggerAndComplete(
        childScheduler,
        30
      )

      timeAdvancer.expectMessage(
        CompletionMessage(
          triggerId
        )
      )
    }

    "work as expected when schedulers inactive" in {
      val timeAdvancer = TestProbe[SchedulerMessage]("timeAdvancer")
      val agent = TestProbe[TriggerWithIdMessage]("agent")

      val key = UUID.randomUUID()

      val parentScheduler = spawn(
        Scheduler(timeAdvancer.ref)
      )
      val childScheduler = spawn(
        Scheduler(parentScheduler)
      )

      // first, we normally schedule some activation
      childScheduler ! ScheduleTriggerMessage(
        ActivityStartTrigger(30),
        agent.ref.toClassic
      )
      timeAdvancer.expectMessage(
        ScheduleTriggerMessage(
          ActivityStartTrigger(30),
          parentScheduler.toClassic
        )
      )

      // create and initialize lock
      val scheduleLock = spawn(
        ScheduleLock(Set(key))
      )
      parentScheduler ! ScheduleTriggerMessage(
        InitLock(parentScheduler, 30),
        scheduleLock.toClassic
      )

      // activate the scheduler, lock should now initialize
      val triggerId = 3
      parentScheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(30),
        triggerId
      )

      // completing the agent activation
      agent.expectTriggerAndComplete(
        childScheduler,
        30
      )

      // because of the lock, child/parentScheduler should not be able to complete yet
      timeAdvancer.expectNoMessage()

      // schedule activation for agent, should unlock the lock
      childScheduler ! ScheduleTriggerMessage(
        ActivityStartTrigger(40),
        agent.ref.toClassic,
        Some(scheduleLock, key)
      )

      timeAdvancer.expectMessage(
        CompletionMessage(
          triggerId,
          Some(
            ScheduleTriggerMessage(
              ActivityStartTrigger(40),
              parentScheduler.toClassic
            )
          )
        )
      )
    }
  }

}
