package edu.ie3.simona.model.em
import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.datamodel.models.input.system.{PvInput, WecInput}
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import squants.Power

final case class EmAggregatePeakShave(

                                     )
  extends EmAggregateFlex {


  /** Aggregates flex options of connected devices to one flex options object
   * that describes the flexibility of this EmAgent
   *
   * @param flexOptions
   * the flex options of all connected agents
   * @return
   * aggregated reference, minimum and maximum power
   */
  override def aggregateFlexOptions(
                                     flexOptions: Iterable[
                                       (_ <: AssetInput, ProvideMinMaxFlexOptions)
                                     ]): (Power, Power, Power) = {
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
      flexOptions.foldLeft(zeroKW) {
        case (
          maxSumExclReg,
          (inputModel, ProvideMinMaxFlexOptions(_, _, addMin, addMax)),
          ) =>
          inputModel match {
            case _: PvInput | _: WecInput =>
              maxSumExclReg + addMin
            case _ => maxSumExclReg + addMax
          }
      }


    // take the closest power possible to zero
    val refAgg = minSum.max(maxRefSum.min(zeroKW))

    (refAgg, minSum, maxSum)
  }
}
