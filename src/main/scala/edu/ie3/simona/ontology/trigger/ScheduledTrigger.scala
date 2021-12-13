/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.trigger

import edu.ie3.simona.akka.SimonaActorRef

import java.util.Comparator
import edu.ie3.simona.ontology.messages.SchedulerMessage.TriggerWithIdMessage
import edu.ie3.simona.ontology.trigger.ScheduledTrigger.ScheduledTriggerComparator

/** Wrapper class for trigger, that are already scheduled for execution @
  * [[TriggerWithIdMessage.trigger.tick]] in the
  * [[edu.ie3.simona.scheduler.main.SimScheduler]]
  *
  * @param triggerWithIdMessage
  *   the trigger that has to be scheduled
  * @param agent
  *   the agent that wants to be scheduled
  */
final case class ScheduledTrigger(
    triggerWithIdMessage: TriggerWithIdMessage,
    agent: SimonaActorRef
) extends Ordered[ScheduledTrigger] {

  def compare(that: ScheduledTrigger): Int = {
    ScheduledTriggerComparator.compare(this, that)
  }
}

object ScheduledTrigger {

  object ScheduledTriggerComparator extends Comparator[ScheduledTrigger] {

    override def compare(
        sTrigger1: ScheduledTrigger,
        sTrigger2: ScheduledTrigger
    ): Int = {
      compare(sTrigger1.triggerWithIdMessage, sTrigger2.triggerWithIdMessage)
    }

    /** Ordering of scheduled trigger. The ordering is always based on the
      * instance of [[TriggerWithIdMessage]] it is holding.
      *
      * First order criteria is the tick, if both ticks are equal, second order
      * criteria is the triggerId. Both order criteria led to an ascending
      * ordering in an ordered list.
      *
      * @param thisMessage
      *   first message that should be compared
      * @param thatMessage
      *   second message that should be compared
      * @return
      *   if both criteria a equal the return code 0 or -1 or 1 otherwise
      */
    def compare(
        thisMessage: TriggerWithIdMessage,
        thatMessage: TriggerWithIdMessage
    ): Int = {
      thisMessage.trigger.tick.compareTo(thatMessage.trigger.tick) match {
        case 0 =>
          // both ticks are equal, order by triggerId
          thisMessage.triggerId.compareTo(thatMessage.triggerId)
        case unequal => unequal
      }
    }
  }

}
