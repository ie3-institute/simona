/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.em

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideMinMaxFlexOptions
import squants.energy.Kilowatts

// TODO provide test
object EmAggregateSimpleSum extends EmAggregateFlex {

  override def aggregateFlexOptions(
      flexOptions: Iterable[
        (_ <: AssetInput, ProvideMinMaxFlexOptions)
      ]
  ): (squants.Power, squants.Power, squants.Power) = {
    flexOptions.foldLeft(
      (Kilowatts(0d), Kilowatts(0d), Kilowatts(0d))
    ) {
      case (
            (sumRef, sumMin, sumMax),
            (_, ProvideMinMaxFlexOptions(_, addRef, addMin, addMax))
          ) =>
        (
          sumRef + addRef,
          sumMin + addMin,
          sumMax + addMax
        )
    }
  }
}
