/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.scheduler.ScheduleLock.{InitLock, Unlock}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.UUID

class ScheduleLockSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with should.Matchers {

  "The ScheduleLock" should {

    "work as expected" in {
      val scheduler = TestProbe[SchedulerMessage]("scheduler")

      val key1 = UUID.randomUUID()
      val key2 = UUID.randomUUID()

      val scheduleLock = spawn(
        ScheduleLock(Set(key1, key2))
      )

      // initialize lock
      val triggerId = 3
      scheduleLock ! TriggerWithIdMessage(
        InitLock(scheduler.ref, 300),
        triggerId
      )

      // unlock one of both keys
      scheduleLock ! Unlock(key2)
      scheduler.expectNoMessage()

      // unlock second key, should unlock now
      scheduleLock ! Unlock(key1)
      scheduler.expectMessage(CompletionMessage(triggerId))
      scheduler.expectTerminated(scheduleLock)
    }
  }

}
