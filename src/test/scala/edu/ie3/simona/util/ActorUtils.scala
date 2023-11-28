/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import org.apache.pekko.actor.testkit.typed.scaladsl.FishingOutcomes.fail
import org.apache.pekko.actor.testkit.typed.scaladsl.TestProbe
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.trigger.Trigger
import edu.ie3.simona.ontology.trigger.Trigger.ActivityStartTrigger
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

object ActorUtils {
  implicit class RichTriggeredAgent(
      private val triggeredAgent: TestProbe[TriggerWithIdMessage]
  ) {

    def expectTriggerAndComplete[T <: Trigger](
        scheduler: ActorRef[SchedulerMessage],
        expectedTick: Long,
        newTick: Option[Long] = None
    ): Unit = {
      val receivedTrigger =
        triggeredAgent.expectMessageType[TriggerWithIdMessage]

      receivedTrigger.trigger match {
        case trigger: T =>
          trigger.tick shouldBe expectedTick
        case unexpected =>
          fail(s"Received unexpected trigger $unexpected")
      }

      val newTrigger =
        newTick.map(tick =>
          ScheduleTriggerMessage(
            ActivityStartTrigger(tick),
            triggeredAgent.ref.toClassic
          )
        )

      scheduler ! CompletionMessage(
        receivedTrigger.triggerId,
        newTrigger
      )
    }

  }
}
