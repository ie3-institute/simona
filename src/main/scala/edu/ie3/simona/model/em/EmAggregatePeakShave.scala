/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.datamodel.models.input.system.{PvInput, StorageInput, WecInput}
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.scala.quantities.DefaultQuantities._
import squants.Power
import squants.energy.{Kilowatts, Power}

/** Aggregates flex reference power with the target of reaching 0kW, while
  * optionally excluding positive flex potential of PV/WEC from the calculation
  *
  * @param curtailRegenerative
  *   Whether to include positive flexibility of PV/WEC in reference sum
  *   calculation
  */
final case class EmAggregatePeakShave(
                                       curtailRegenerative: Boolean,
                                       includeStorageInMax: Boolean
                                     )
    extends EmAggregateFlex {

  override def aggregateFlexOptions(
      flexOptions: Iterable[
        (_ <: AssetInput, ProvideMinMaxFlexOptions)
      ]
  ): (Power, Power, Power) = {
    val genLimit = flexOptions.collect {
      case (pv: PvInput, _) => Kilowatts(0.3*pv.getsRated().getValue.doubleValue())
    }.foldLeft(zeroKW)(_ + _)

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
      if (curtailRegenerative)
        maxSum
      else
        flexOptions.foldLeft(zeroKW) {
          case (
                maxSumExclReg,
                (inputModel, ProvideMinMaxFlexOptions(_, _, addMin, addMax)),
              ) =>
            inputModel match {
              case _: PvInput | _: WecInput =>
                maxSumExclReg + addMin
              case _: StorageInput =>
                if (includeStorageInMax)
                  maxSumExclReg + addMax
                else
                  maxSumExclReg
              case _ => maxSumExclReg + addMax
            }
        }

    // take the closest power possible to zero
    val refAgg = minSum.max(maxRefSum.min(zeroKW)).max(-genLimit)

    (refAgg, minSum, maxSum)
  }
}
