/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event

/** Trait that should be mixed into each event in [[edu.ie3.simona.event]] to
  * provide an easy access to it's name
  */
trait Event {

  def id: String = this.getClass.getSimpleName

}
