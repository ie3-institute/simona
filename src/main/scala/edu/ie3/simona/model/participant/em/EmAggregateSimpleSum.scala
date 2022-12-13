/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.em

import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import tech.units.indriya.ComparableQuantity

import javax.measure.quantity.Power

// TODO provide test
object EmAggregateSimpleSum extends EmAggregateFlex {

  override def aggregateFlexOptions(
      flexOptions: Iterable[
        (_ <: SystemParticipantInput, ProvideMinMaxFlexOptions)
      ]
  ): (
      ComparableQuantity[Power],
      ComparableQuantity[Power],
      ComparableQuantity[Power]
  ) = {
    flexOptions.foldLeft(
      (zeroKW, zeroKW, zeroKW)
    ) {
      case (
            (sumRef, sumMin, sumMax),
            (_, ProvideMinMaxFlexOptions(_, addRef, addMin, addMax))
          ) =>
        (
          sumRef.add(addRef),
          sumMin.add(addMin),
          sumMax.add(addMax)
        )
    }
  }
}
