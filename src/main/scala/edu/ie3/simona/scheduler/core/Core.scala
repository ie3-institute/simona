/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler.core

import edu.ie3.simona.ontology.messages.Activation
import org.apache.pekko.actor.typed.ActorRef

/** TODO ScalaDoc everywhere
  */
object Core {

  trait CoreFactory {
    def create(): InactiveCore
  }

  trait InactiveCore {
    def checkActivation(newTick: Long): Boolean

    def activate(): ActiveCore

    def checkSchedule(newTick: Long): Boolean

    def handleSchedule(
        actor: ActorRef[Activation],
        newTick: Long
    ): (Option[Long], InactiveCore)
  }

  trait ActiveCore {

    def activeTick: Long

    def checkCompletion(actor: ActorRef[Activation]): Boolean

    def handleCompletion(actor: ActorRef[Activation]): ActiveCore

    def maybeComplete(): Option[(Option[Long], InactiveCore)]

    def checkSchedule(newTick: Long): Boolean

    def handleSchedule(actor: ActorRef[Activation], newTick: Long): ActiveCore

    def takeNewActivations(): (Iterable[ActorRef[Activation]], ActiveCore)
  }

}
