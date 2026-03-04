/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions

/** Aggregates [[EnergyBoundariesFlexOptions]] by simply concatenating all
  * [[edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.AssetEnergyBoundaries]]
  * without simplifying or merging any flexibility options.
  */
object EmAggregateEnergyBoundariesSimple
    extends EmAggregateFlex[EnergyBoundariesFlexOptions] {

  override def aggregateFlexOptions(
      flexOptions: Iterable[
        (? <: AssetInput, EnergyBoundariesFlexOptions)
      ]
  ): EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      energyBoundaries = flexOptions.flatMap { case (_, fo) =>
        fo.energyBoundaries
      }.toSeq
    )

}
