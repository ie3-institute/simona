/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

/** Trait to group all states, that belong to a thermal model
  */
trait ThermalModelState {

  /** Simulation instance, since that state is valid
    */
  val tick: Long
}
