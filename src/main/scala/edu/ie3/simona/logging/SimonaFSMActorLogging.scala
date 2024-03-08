/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.logging

import org.apache.pekko.actor.{ActorLogging, FSM}
import org.apache.pekko.event.LoggingAdapter

trait SimonaFSMActorLogging extends ActorLogging with SimonaLogging {
  this: FSM[_, _] =>

  override final val log: LoggingAdapter =
    SimonaLogging.createAdapter(
      context.system,
      () => stateName,
      this,
      actorName,
    )

}
