/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessageTyped}
import edu.ie3.simona.ontology.messages.SchedulerMessageTyped.{
  Completion,
  ScheduleActivation
}
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
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
      val timeAdvancer = TestProbe[SchedulerMessageTyped]("timeAdvancer")
      val agent1 = TestProbe[Activation]("agent1")
      val agent2 = TestProbe[Activation]("agent2")

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
      childScheduler ! ScheduleActivation(
        agent1.ref,
        30
      )
      val sa1 = timeAdvancer.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe 30
      val lockActivation = sa1.actor

      // create and initialize lock
      val scheduleLock = spawn(
        ScheduleLock(parentScheduler, Set(key), 30),
        "lock"
      )
      agent1.expectNoMessage()

      // activate the scheduler, lock should now initialize
      lockActivation ! Activation(30)

      // schedule activation for agent2, lock should get unlocked
      childScheduler ! ScheduleActivation(
        agent2.ref,
        30,
        Some(ScheduleKey(scheduleLock, key))
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

      timeAdvancer.expectMessage(Completion(lockActivation))
    }

    "work as expected when schedulers inactive" in {
      val timeAdvancer = TestProbe[SchedulerMessageTyped]("timeAdvancer")
      val agent = TestProbe[Activation]("agent")

      val key = UUID.randomUUID()

      val parentScheduler = spawn(
        Scheduler(timeAdvancer.ref)
      )
      val childScheduler = spawn(
        Scheduler(parentScheduler)
      )

      // first, we normally schedule some activation
      childScheduler ! ScheduleActivation(
        agent.ref,
        30
      )
      val sa1 = timeAdvancer.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe 30
      val lockActivation = sa1.actor

      // create and initialize lock
      val scheduleLock = spawn(
        ScheduleLock(parentScheduler, Set(key), 30),
        "lock"
      )
      agent.expectNoMessage()

      // activate the scheduler, lock should now initialize
      lockActivation ! Activation(30)

      // completing the agent activation
      agent.expectTriggerAndComplete(
        childScheduler,
        30
      )

      // because of the lock, child/parentScheduler should not be able to complete yet
      timeAdvancer.expectNoMessage()

      // schedule activation for agent, should unlock the lock
      childScheduler ! ScheduleActivation(
        agent.ref,
        40,
        Some(ScheduleKey(scheduleLock, key))
      )

      timeAdvancer.expectMessage(Completion(lockActivation, Some(40)))

    }
  }

}
