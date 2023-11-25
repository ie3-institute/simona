/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.actor

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}

object ActorUtil {
  def stopOnError[M](
      ctx: ActorContext[M],
      msg: String
  ): Behavior[M] = {
    ctx.log.error(s"$msg. Stopping.")
    Behaviors.stopped
  }
}
