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
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.trigger.Trigger.ActivityStartTrigger
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.util.ActorUtils.RichTriggeredAgent
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike

class ScheduleLockIT
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with should.Matchers
    with TestSpawnerTyped {

  "The ScheduleLock in conjunction with schedulers" should {

    "work as expected when schedulers active" in {
      val timeAdvancer = TestProbe[SchedulerMessage]("timeAdvancer")
      val agent1 = TestProbe[TriggerWithIdMessage]("agent1")
      val agent2 = TestProbe[TriggerWithIdMessage]("agent2")

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
      val sa1 = timeAdvancer.expectMessageType[ScheduleTriggerMessage]
      sa1.trigger shouldBe ActivityStartTrigger(30)
      val lockActivation = sa1.actorToBeScheduled

      // create and initialize lock
      val scheduleKey =
        ScheduleLock.singleKey(TSpawner, parentScheduler, 30)
      agent1.expectNoMessage()

      // activate the scheduler, lock should now initialize
      lockActivation ! TriggerWithIdMessage(ActivityStartTrigger(30), 7)

      // schedule activation for agent2, lock should get unlocked
      childScheduler ! ScheduleTriggerMessage(
        ActivityStartTrigger(30),
        agent2.ref.toClassic,
        Some(scheduleKey)
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

      timeAdvancer.expectMessage(CompletionMessage(7))
    }

    "work as expected when schedulers inactive" in {
      val timeAdvancer = TestProbe[SchedulerMessage]("timeAdvancer")
      val agent = TestProbe[TriggerWithIdMessage]("agent")

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
      val sa1 = timeAdvancer.expectMessageType[ScheduleTriggerMessage]
      sa1.trigger shouldBe ActivityStartTrigger(30)
      val lockActivation = sa1.actorToBeScheduled

      // create and initialize lock
      val scheduleKey = ScheduleLock.singleKey(TSpawner, parentScheduler, 30)
      agent.expectNoMessage()

      // activate the scheduler, lock should now initialize
      lockActivation ! TriggerWithIdMessage(ActivityStartTrigger(30), 7)

      // completing the agent activation
      agent.expectTriggerAndComplete(
        childScheduler,
        30
      )

      // because of the lock, parentScheduler should not be able to complete yet
      timeAdvancer.expectNoMessage()

      // schedule activation for agent, should unlock the lock
      childScheduler ! ScheduleTriggerMessage(
        ActivityStartTrigger(40),
        agent.ref.toClassic,
        Some(scheduleKey)
      )

      timeAdvancer.expectMessage(
        CompletionMessage(
          7,
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
