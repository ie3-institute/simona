/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.scala.quantities.DefaultQuantities._
import squants.Power

/** Aggregates flex reference power with the target of reaching 0kW, while
  * optionally excluding positive PV potential from the calculation
  *
  * @param pvFlex
  *   Whether to include positive PV flexibility in reference sum calculation
  */
final case class EmAggregateSelfOpt(pvFlex: Boolean) extends EmAggregateFlex {

  override def aggregateFlexOptions(
      flexOptions: Iterable[
        (_ <: AssetInput, ProvideMinMaxFlexOptions)
      ]
  ): (Power, Power, Power) = {
    val (minSum, maxSum) =
      flexOptions.foldLeft((zeroKW, zeroKW)) {
        case (
              (sumMin, sumMax),
              (_, ProvideMinMaxFlexOptions(_, _, addMin, addMax)),
            ) =>
          (
            sumMin + addMin,
            sumMax + addMax,
          )
      }

    val maxRefSum =
      if (pvFlex)
        maxSum
      else
        flexOptions.foldLeft(zeroKW) {
          case (
                maxSumExclPv,
                (inputModel, ProvideMinMaxFlexOptions(_, _, addMin, addMax)),
              ) =>
            inputModel match {
              case _: PvInput =>
                maxSumExclPv + addMin
              case _ => maxSumExclPv + addMax
            }
        }

    // take the closest power possible to zero
    val refAgg = minSum.max(maxRefSum.min(zeroKW))

    (refAgg, minSum, maxSum)
  }
}
