/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.em

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import squants.energy.Kilowatts

/** Aggregates flex reference power with the target of reaching 0kW, while
  * excluding positive PV potential from the calculation
  */
object EmAggregateSelfOptExclPv extends EmAggregateFlex {

  override def aggregateFlexOptions(
      flexOptions: Iterable[
        (_ <: AssetInput, ProvideMinMaxFlexOptions)
      ]
  ): (squants.Power, squants.Power, squants.Power) = {
    val (minSum, maxSum, maxExclPv) =
      flexOptions.foldLeft((Kilowatts(0d), Kilowatts(0d), Kilowatts(0d))) {
        case (
              (sumMin, sumMax, sumMaxExclPv),
              (inputModel, ProvideMinMaxFlexOptions(_, _, addMin, addMax))
            ) =>
          (
            sumMin + addMin,
            sumMax + addMax,
            inputModel match {
              case _: PvInput =>
                sumMaxExclPv + addMin
              case _ => sumMaxExclPv + addMax
            }
          )
      }

    // take the closest power possible to zero
    val aggregateRef = minSum.max(maxExclPv.min(Kilowatts(0d)))

    (aggregateRef, minSum, maxSum)
  }
}
