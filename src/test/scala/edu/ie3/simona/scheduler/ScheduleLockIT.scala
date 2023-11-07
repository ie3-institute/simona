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
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.trigger.Trigger.ActivityStartTrigger
import edu.ie3.simona.scheduler.ScheduleLock.{InitLock, Unlock}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.UUID

class ScheduleLockIT
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with should.Matchers {

  "The ScheduleLock in conjunction with schedulers" should {

    "work as expected when schedulers inactive" in {
      val rootScheduler = TestProbe[SchedulerMessage]("rootScheduler")
      val agent = TestProbe[Object]("agent")

      val key = UUID.randomUUID()

      val parentScheduler = spawn(
        Scheduler(rootScheduler.ref)
      )
      val childScheduler = spawn(
        Scheduler(parentScheduler)
      )

      // first, we normally schedule some activation
      childScheduler ! ScheduleTriggerMessage(
        ActivityStartTrigger(30),
        agent.ref.toClassic
      )
      rootScheduler.expectMessage(
        ScheduleTriggerMessage(
          ActivityStartTrigger(30),
          parentScheduler.toClassic
        )
      )

      val scheduleLock = spawn(
        ScheduleLock(Set(key))
      )

      // initialize lock
      val triggerId = 3
      scheduleLock ! TriggerWithIdMessage(
        InitLock(parentScheduler, 300),
        triggerId
      )

      // unlock second key, should unlock now
      scheduleLock ! Unlock(key)

      // TODO
    }
  }

  "work as expected when schedulers active" in {}

}
