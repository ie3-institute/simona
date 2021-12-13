/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.akka

import akka.actor.{ActorContext, ActorRef}
import edu.ie3.simona.akka.SimonaActorRef.LocalActorRef

object SimonaActorRefUtils {

  implicit class RichActorContext(private val context: ActorContext)
      extends AnyVal {

    /** Lets context watch the given actor ref, if possible
      *
      * @param ref
      *   The SimonaActorRef to watch
      */
    def watch(ref: SimonaActorRef): Unit = ref.watchedBy(context)

    /** Lets context unwatch the given actor ref, if possible
      *
      * @param ref
      *   The SimonaActorRef to unwatch
      */
    def unwatch(ref: SimonaActorRef): Unit = ref.unwatchedBy(context)

    /** Lets context stop the given actor ref
      *
      * @param ref
      *   The SimonaActorRef to stop
      */
    def stop(ref: SimonaActorRef): Unit = ref.stoppedBy(context)
  }
}
