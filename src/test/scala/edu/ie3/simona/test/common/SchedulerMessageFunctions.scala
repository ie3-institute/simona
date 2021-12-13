/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import akka.testkit.TestKit
import edu.ie3.simona.ontology.messages.SchedulerMessage.CompletionMessage
import edu.ie3.simona.ontology.trigger.Trigger
import org.scalatest.wordspec.AnyWordSpecLike

/** //ToDo: Class Description
  *
  * @version 0.1
  * @since 2019-08-11
  */
trait SchedulerMessageFunctions extends TestKit with AnyWordSpecLike {

  def expectCompletionMessage(): CompletionMessage = {
    expectMsgPF() {
      case msg @ CompletionMessage(_, _, _) => msg
      case x =>
        fail(
          s"Unexpected message $x received when expecting CompletionMessage!"
        )
    }
  }

  def getTriggersFromCompletionMessage(
      msg: CompletionMessage
  ): Vector[Trigger] = {
    msg match {
      case CompletionMessage(_, _, newTriggers) =>
        newTriggers
          .map(schedulerTriggerMessageSeq =>
            schedulerTriggerMessageSeq.foldLeft(Vector.empty[Trigger])(
              (vector, scheduleTriggerMessage) => {
                vector :+ scheduleTriggerMessage.trigger
              }
            )
          )
          .getOrElse(fail("Something went wrong during trigger extraction!"))
      case _ => fail("No completion message provided!")
    }
  }

}
