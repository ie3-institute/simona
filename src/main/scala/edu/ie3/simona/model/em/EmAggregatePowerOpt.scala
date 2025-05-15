/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.datamodel.models.input.system.{PvInput, WecInput}
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexOptions
import edu.ie3.util.scala.quantities.DefaultQuantities.*
import squants.Power

import java.lang.Math.signum

/** Aggregates flex reference power with the goal of not exceed a defined target
  * limit, while optionally excluding positive flex potential of PV/WEC from the
  * calculation. If the target limit can't be met, the closes possible operation
  * point should be chosen.
  *
  * @param targetPowerAbs
  *   absolute power target value that should be not be exceeded
  * @param curtailRegenerative
  *   Whether to include positive flexibility of PV/WEC in reference sum
  *   calculation
  */
final case class EmAggregatePowerOpt(
    targetPowerAbs: Power = zeroKW,
    curtailRegenerative: Boolean,
) extends EmAggregateFlex {

  override def aggregateFlexOptions(
      flexOptions: Iterable[
        (? <: AssetInput, MinMaxFlexOptions)
      ]
  ): MinMaxFlexOptions = {
    val (minSum, refSum, maxSum) =
      flexOptions.foldLeft((zeroKW, zeroKW, zeroKW)) {
        case (
              (sumMin, sumRef, sumMax),
              (_, MinMaxFlexOptions(addRef, addMin, addMax)),
            ) =>
          (
            sumMin + addMin,
            sumRef + addRef,
            sumMax + addMax,
          )
      }

    val maxRefSum =
      if curtailRegenerative then maxSum
      else
        flexOptions.foldLeft(zeroKW) {
          case (
                maxSumExclReg,
                (inputModel, MinMaxFlexOptions(_, addMin, addMax)),
              ) =>
            inputModel match {
              case _: PvInput | _: WecInput =>
                maxSumExclReg + addMin
              case _ => maxSumExclReg + addMax
            }
        }

    val targetAbs = if targetPowerAbs.abs < refSum.abs then {
      targetPowerAbs * signum(refSum.toKilowatts)
    } else refSum

    val refAgg = minSum.max(maxRefSum.min(targetAbs))

    MinMaxFlexOptions(refAgg, minSum, maxSum)
  }
}
