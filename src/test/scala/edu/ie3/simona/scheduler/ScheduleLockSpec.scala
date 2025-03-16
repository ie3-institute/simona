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
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.test.common.TestSpawnerTyped
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike

class ScheduleLockSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with should.Matchers
    with TestSpawnerTyped {

  "The ScheduleLock" should {

    "work as expected" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")

      val scheduleLocks =
        ScheduleLock.multiKey(TSpawner, scheduler.ref, 300, 2).toIndexedSeq

      val sa = scheduler.expectMessageType[ScheduleActivation]
      sa.tick shouldBe 300
      sa.unlockKey shouldBe None
      val lockActivation = sa.actor

      // initialize lock
      lockActivation ! Activation(300)

      // use one of both keys
      scheduleLocks(0).unlock()
      scheduler.expectNoMessage()

      // use second key, should unlock now
      scheduleLocks(1).unlock()

      scheduler.expectMessage(Completion(lockActivation))
      scheduler.expectTerminated(scheduleLocks(1).lock)
    }

    "stashes unlock messages when not yet initialized" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")

      val scheduleLock =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, 300)

      val sa = scheduler.expectMessageType[ScheduleActivation]
      sa.tick shouldBe 300
      sa.unlockKey shouldBe None
      val lockActivation = sa.actor

      // use key
      scheduleLock.unlock()
      scheduler.expectNoMessage()

      // initialize lock, which should unlock right away now
      lockActivation ! Activation(300)

      scheduler.expectMessage(Completion(lockActivation))
      scheduler.expectTerminated(scheduleLock.lock)
    }

    "fails if activated with wrong tick" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")

      val scheduleLock = ScheduleLock.singleKey(TSpawner, scheduler.ref, 300)

      val sa = scheduler.expectMessageType[ScheduleActivation]
      sa.tick shouldBe 300
      sa.unlockKey shouldBe None
      val lockActivation = sa.actor

      // initialize lock with wrong tick
      lockActivation ! Activation(301)

      // use second key, won't unlock now
      scheduleLock.unlock()

      scheduler.expectNoMessage()
      scheduler.expectTerminated(lockActivation)
    }
  }

}
