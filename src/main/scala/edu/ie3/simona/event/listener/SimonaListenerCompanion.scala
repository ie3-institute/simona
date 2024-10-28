/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import org.apache.pekko.actor.Props

import scala.reflect.ClassTag

/** Companion trait to [[[[edu.ie3.simona.event.listener.SimonaListener]]
  */
trait SimonaListenerCompanion {
  def props[A <: SimonaListenerWithFilter: ClassTag](
      eventsToProcess: Option[Seq[String]] = None
  ): Props
}
