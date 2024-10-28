/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service

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
}
