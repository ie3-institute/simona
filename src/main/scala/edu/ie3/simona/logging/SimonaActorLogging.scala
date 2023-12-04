/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.logging

import org.apache.pekko.actor.{Actor, ActorLogging}
import org.apache.pekko.event.LoggingAdapter

trait SimonaActorLogging extends ActorLogging with SimonaLogging {
  this: Actor =>

  override final val log: LoggingAdapter =
    SimonaLogging.createAdapter(context.system, this, actorName)

}
