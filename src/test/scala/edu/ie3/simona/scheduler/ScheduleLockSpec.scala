/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessageTyped
import edu.ie3.simona.ontology.messages.SchedulerMessageTyped.{
  Completion,
  ScheduleActivation
}
import edu.ie3.simona.scheduler.ScheduleLock.Unlock
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.UUID

class ScheduleLockSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with should.Matchers {

  "The ScheduleLock" should {

    "work as expected" in {
      val scheduler = TestProbe[SchedulerMessageTyped]("scheduler")

      val key1 = UUID.randomUUID()
      val key2 = UUID.randomUUID()

      val scheduleLock = spawn(
        ScheduleLock(scheduler.ref, Set(key1, key2), 300)
      )

      val sa = scheduler.expectMessageType[ScheduleActivation]
      sa.tick shouldBe 300
      sa.unlockKey shouldBe None
      val lockActivation = sa.actor

      // initialize lock
      lockActivation ! Activation(300)

      // unlock one of both keys
      scheduleLock ! Unlock(key2)
      scheduler.expectNoMessage()

      // unlock second key, should unlock now
      scheduleLock ! Unlock(key1)

      scheduler.expectMessage(Completion(lockActivation))
      scheduler.expectTerminated(scheduleLock)
    }

    "stashes unlock messages when not yet initialized" in {
      val scheduler = TestProbe[SchedulerMessageTyped]("scheduler")

      val key = UUID.randomUUID()

      val scheduleLock = spawn(
        ScheduleLock(scheduler.ref, Set(key), 300)
      )

      val sa = scheduler.expectMessageType[ScheduleActivation]
      sa.tick shouldBe 300
      sa.unlockKey shouldBe None
      val lockActivation = sa.actor

      // unlock one of both keys
      scheduleLock ! Unlock(key)
      scheduler.expectNoMessage()

      // initialize lock, which should unlock right away now
      lockActivation ! Activation(300)

      scheduler.expectMessage(Completion(lockActivation))
      scheduler.expectTerminated(scheduleLock)
    }
  }

}
