/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.em

import edu.ie3.datamodel.models.input.system.{PvInput, SystemParticipantInput}
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.quantities.QuantityUtils.RichQuantity
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import tech.units.indriya.ComparableQuantity

import javax.measure.quantity.Power

/** Aggregates flex reference power with the target of reaching 0kW, while
  * excluding positive PV potential from the calculation
  */
object EmAggregateSelfOptExclPv extends EmAggregateFlex {

  override def aggregateFlexOptions(
      flexOptions: Iterable[
        (_ <: SystemParticipantInput, ProvideMinMaxFlexOptions)
      ]
  ): (
      ComparableQuantity[Power],
      ComparableQuantity[Power],
      ComparableQuantity[Power]
  ) = {
    val (minSum, maxSum, maxExclPv) =
      flexOptions.foldLeft((zeroKW, zeroKW, zeroKW)) {
        case (
              (sumMin, sumMax, sumMaxExclPv),
              (spi, ProvideMinMaxFlexOptions(_, _, addMin, addMax))
            ) =>
          (
            sumMin.add(addMin),
            sumMax.add(addMax),
            spi match {
              case _: PvInput =>
                sumMaxExclPv.add(addMin)
              case _ => sumMaxExclPv.add(addMax)
            }
          )
      }

    // take the closest power possible to zero
    val aggregateRef = minSum.max(maxExclPv.min(zeroKW))

    (aggregateRef, minSum, maxSum)
  }
}
