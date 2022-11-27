/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.em

import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.quantities.QuantityUtils.RichQuantity
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import tech.units.indriya.ComparableQuantity

import javax.measure.quantity.Power

object EmAggregateSelfOpt extends EmAggregateFlex {

  override def aggregateFlexOptions(
      flexOptions: Iterable[ProvideMinMaxFlexOptions]
  ): (
      ComparableQuantity[Power],
      ComparableQuantity[Power],
      ComparableQuantity[Power]
  ) = {
    val (minSum, maxSum) = flexOptions.foldLeft((zeroKW, zeroKW)) {
      case (
            (sumMin, sumMax),
            ProvideMinMaxFlexOptions(_, _, addMin, addMax)
          ) =>
        (
          sumMin.add(addMin),
          sumMax.add(addMax)
        )
    }

    // take the closest power possible to zero
    val aggregateRef = minSum.max(maxSum.min(zeroKW))

    (aggregateRef, minSum, maxSum)
  }
}
