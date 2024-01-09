/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.em

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideMinMaxFlexOptions
import squants.energy.Kilowatts

object EmAggregateSelfOpt extends EmAggregateFlex {

  override def aggregateFlexOptions(
      flexOptions: Iterable[
        (_ <: AssetInput, ProvideMinMaxFlexOptions)
      ]
  ): (squants.Power, squants.Power, squants.Power) = {
    val (minSum, maxSum) =
      flexOptions.foldLeft((Kilowatts(0d), Kilowatts(0d))) {
        case (
              (sumMin, sumMax),
              (_, ProvideMinMaxFlexOptions(_, _, addMin, addMax))
            ) =>
          (
            sumMin + addMin,
            sumMax + addMax
          )
      }

    // take the closest power possible to zero
    val aggregateRef = minSum.max(maxSum.min(Kilowatts(0d)))

    (aggregateRef, minSum, maxSum)
  }
}
