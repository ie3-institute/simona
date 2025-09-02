/*
 * © 2023. TU Dortmund University,
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
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.util.ActorUtils.RichActivatedActor
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
      val agent1 = TestProbe[Activation]("agent1")
      val agent2 = TestProbe[Activation]("agent2")

      val parentScheduler = spawn(
        Scheduler(timeAdvancer.ref),
        "parentScheduler",
      )
      val childScheduler = spawn(
        Scheduler(parentScheduler),
        "childScheduler",
      )

      // first, we normally schedule some activation for our agent1
      childScheduler ! ScheduleActivation(
        agent1.ref,
        30,
      )
      val sa1 = timeAdvancer.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe 30
      val lockActivation = sa1.actor

      // create and initialize lock
      val scheduleKey =
        ScheduleLock.singleKey(TSpawner, parentScheduler, 30)
      agent1.expectNoMessage()

      // activate the scheduler, lock should now initialize
      lockActivation ! Activation(30)

      // schedule activation for agent2, lock should get unlocked
      childScheduler ! ScheduleActivation(
        agent2.ref,
        30,
        Some(scheduleKey),
      )

      // because of activated agents, child/parentScheduler should not be able to complete yet
      timeAdvancer.expectNoMessage()

      // completing agent activations
      agent1.expectActivationAndComplete(
        childScheduler,
        30,
      )
      agent2.expectActivationAndComplete(
        childScheduler,
        30,
      )

      timeAdvancer.expectMessage(Completion(lockActivation))
    }

    "work as expected when schedulers inactive" in {
      val timeAdvancer = TestProbe[SchedulerMessage]("timeAdvancer")
      val agent = TestProbe[Activation]("agent")

      val parentScheduler = spawn(
        Scheduler(timeAdvancer.ref)
      )
      val childScheduler = spawn(
        Scheduler(parentScheduler)
      )

      // first, we normally schedule some activation
      childScheduler ! ScheduleActivation(
        agent.ref,
        30,
      )
      val sa1 = timeAdvancer.expectMessageType[ScheduleActivation]
      sa1.tick shouldBe 30
      val lockActivation = sa1.actor

      // create and initialize lock
      val scheduleKey = ScheduleLock.singleKey(TSpawner, parentScheduler, 30)
      agent.expectNoMessage()

      // activate the scheduler, lock should now initialize
      lockActivation ! Activation(30)

      // completing the agent activation
      agent.expectActivationAndComplete(
        childScheduler,
        30,
      )

      // because of the lock, parentScheduler should not be able to complete yet
      timeAdvancer.expectNoMessage()

      // schedule activation for agent, should unlock the lock
      childScheduler ! ScheduleActivation(
        agent.ref,
        40,
        Some(scheduleKey),
      )

      timeAdvancer.expectMessage(Completion(lockActivation, Some(40)))

    }
  }

}
