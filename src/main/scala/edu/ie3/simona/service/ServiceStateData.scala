/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service

import akka.actor.ActorRef
import edu.ie3.simona.ontology.messages.SchedulerMessage.ScheduleTriggerMessage
import edu.ie3.simona.ontology.trigger.Trigger.ActivityStartTrigger
import edu.ie3.util.scala.collection.immutable.SortedDistinctSeq

trait ServiceStateData

object ServiceStateData {

  /** Data that is required to initialize a SimonaService
    */
  trait InitializeServiceStateData extends ServiceStateData

  trait ServiceBaseStateData extends ServiceStateData

  /** Indicate that the service is initialized
    */
  trait ServiceActivationBaseStateData extends ServiceBaseStateData {
    val maybeNextActivationTick: Option[Long]
    val activationTicks: SortedDistinctSeq[Long]

    /** Get the next upcoming tick and removes it from the list of scheduled
      * ticks
      *
      * @return
      *   The next upcoming tick and the remaining ones
      */
    def popNextTick: (Option[Long], SortedDistinctSeq[Long]) =
      activationTicks.pop
  }

  object ServiceActivationBaseStateData {

    /** Build an optional [[Seq]] of [[ScheduleTriggerMessage]] s based on the
      * given optional next tick and the sender
      */
    val tickToScheduleTriggerMessages
        : (Option[Long], ActorRef) => Option[Seq[ScheduleTriggerMessage]] =
      (maybeTick, sender) =>
        maybeTick.map(tick =>
          Seq(ScheduleTriggerMessage(ActivityStartTrigger(tick), sender))
        )
  }
}
