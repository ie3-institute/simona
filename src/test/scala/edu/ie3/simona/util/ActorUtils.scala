/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import akka.actor.testkit.typed.scaladsl.TestProbe
import akka.actor.typed.ActorRef
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

object ActorUtils {
  implicit class RichTriggeredAgent(
      private val triggeredAgent: TestProbe[Activation]
  ) {

    def expectTriggerAndComplete(
        scheduler: ActorRef[SchedulerMessage],
        expectedTick: Long,
        newTick: Option[Long] = None
    ): Unit = {
      val receivedTrigger =
        triggeredAgent.expectMessageType[Activation]

      receivedTrigger.tick shouldBe expectedTick

      scheduler ! Completion(
        triggeredAgent.ref,
        newTick
      )
    }

  }
}
