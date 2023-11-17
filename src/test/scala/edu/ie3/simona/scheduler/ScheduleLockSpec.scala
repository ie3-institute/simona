/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.{ActorRef, Behavior}
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation
}
import edu.ie3.simona.scheduler.ScheduleLock.{Spawner, Unlock}
import edu.ie3.simona.test.common.TestSpawnerTyped
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.UUID

class ScheduleLockSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with should.Matchers
    with TestSpawnerTyped {

  "The ScheduleLock" should {

    "work as expected" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")

      val scheduleLocks =
        ScheduleLock.multiKey(TSpawner, scheduler.ref, 300, 2).toSeq

      val sa = scheduler.expectMessageType[ScheduleActivation]
      sa.tick shouldBe 300
      sa.unlockKey shouldBe None
      val lockActivation = sa.actor

      // initialize lock
      lockActivation ! Activation(300)

      // use one of both keys
      scheduleLocks(1).unlock()
      scheduler.expectNoMessage()

      // use second key, should unlock now
      scheduleLocks(2).unlock()

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
  }

}
