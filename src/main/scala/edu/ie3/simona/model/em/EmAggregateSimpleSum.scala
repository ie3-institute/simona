/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexOptions

/** Aggregates reference, minimum and maximum power by just simply summing up
  * each value.
  */
object EmAggregateSimpleSum extends EmAggregateFlex {

  override def aggregateFlexOptions(
      flexOptions: Iterable[
        (? <: AssetInput, MinMaxFlexOptions)
      ]
  ): MinMaxFlexOptions =
    flexOptions.map { case (_, flex: MinMaxFlexOptions) =>
      flex
    }.flexSum

}
