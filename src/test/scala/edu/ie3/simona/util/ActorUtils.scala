/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import org.apache.pekko.actor.testkit.typed.scaladsl.TestProbe
import org.apache.pekko.actor.typed.ActorRef
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import org.scalatest.matchers.should.Matchers.shouldBe

object ActorUtils {
  implicit class RichActivatedActor(
      private val triggeredActor: TestProbe[Activation]
  ) {

    def expectActivationAndComplete(
        scheduler: ActorRef[SchedulerMessage],
        expectedTick: Long,
        newTick: Option[Long] = None,
    ): Unit = {
      val receivedTrigger =
        triggeredActor.expectMessageType[Activation]

      receivedTrigger.tick shouldBe expectedTick

      scheduler ! Completion(
        triggeredActor.ref,
        newTick,
      )
    }

  }
}
