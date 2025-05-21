/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexOptions

/** Offers method for aggregating flex options from connected agents which will
  * then be provided to a superior EmAgent or sent out as a flex result
  */
trait EmAggregateFlex {

  /** Aggregates flex options of connected devices to one flex options object
    * that describes the flexibility of this EmAgent.
    *
    * @param flexOptions
    *   The flex options of all connected agents.
    * @return
    *   The aggregated flex options.
    */
  def aggregateFlexOptions(
      flexOptions: Iterable[
        (? <: AssetInput, MinMaxFlexOptions)
      ]
  ): MinMaxFlexOptions

}
