/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.simona.util.ParsableEnumeration

/** Denoting difference referencing scenarios for scaling load model output
  */
object LoadReferenceType extends ParsableEnumeration {

  /** Scale the load model behaviour so that the rated power of the load model
    * serves as the maximum power consumption
    */
  val ACTIVE_POWER: Value = Value("power")

  /** Scale the load model behaviour so that the aggregate annual energy
    * consumption corresponds to the energy set by the model input
    */
  val ENERGY_CONSUMPTION: Value = Value("energy")

}
