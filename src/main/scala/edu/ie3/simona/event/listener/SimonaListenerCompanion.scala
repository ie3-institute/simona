/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import akka.actor.Props

import scala.reflect.ClassTag

/** Companion trait to [[[[edu.ie3.simona.event.listener.SimonaListener]]
  */
trait SimonaListenerCompanion {
  def props[A <: SimonaListenerWithFilter: ClassTag](
      eventsToProcess: Option[List[String]] = None
  ): Props
}
