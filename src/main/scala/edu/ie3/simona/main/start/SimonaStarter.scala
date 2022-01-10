/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.main.start

import akka.actor.ActorSystem

/** Trait that shall be extended for different modes of starting and operating
  * Simona
  */
trait SimonaStarter {

  def start(
      runAndExit: ActorSystem => Unit
  ): Unit

}
