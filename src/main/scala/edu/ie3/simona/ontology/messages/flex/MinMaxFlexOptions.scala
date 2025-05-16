/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.util.scala.quantities.DefaultQuantities.*
import squants.Power

/** A [[FlexOptions]] type that provides interval-based flexibility in form of
  * reference, minimum and maximum power of an asset. It is possible that the
  * power values are either all negative or all positive, meaning that feed-in
  * or load is mandatory.
  *
  * @param ref
  *   The reference active power that the flex options provider would
  *   produce/consume regularly at the current tick, i.e. if it was not
  *   flex-controlled.
  * @param min
  *   The minimum active power that the flex options provider allows at the
  *   current tick.
  * @param max
  *   The maximum active power that the flex options provider allows at the
  *   current tick.
  */
final case class MinMaxFlexOptions(
    ref: Power,
    min: Power,
    max: Power,
) extends FlexOptions {

  def +(rhs: MinMaxFlexOptions): MinMaxFlexOptions =
    MinMaxFlexOptions(
      ref + rhs.ref,
      min + rhs.min,
      max + rhs.max,
    )

}

object MinMaxFlexOptions {

  extension (flexOptions: Iterable[MinMaxFlexOptions]) {
    def flexSum: MinMaxFlexOptions =
      flexOptions.foldLeft(MinMaxFlexOptions(zeroKW, zeroKW, zeroKW)) {
        case (sumOptions, addOptions) =>
          sumOptions + addOptions
      }
  }

  /** Creates [[MinMaxFlexOptions]] with sanity checks regarding the power
    * values.
    *
    * @param ref
    *   The reference active power that the flex options provider would
    *   produce/consume regularly at the current tick, i.e. if it was not
    *   flex-controlled.
    * @param min
    *   The minimum active power that the flex options provider allows at the
    *   current tick.
    * @param max
    *   The maximum active power that the flex options provider allows at the
    *   current tick.
    * @return
    *   The [[MinMaxFlexOptions]].
    */
  def apply(
      ref: Power,
      min: Power,
      max: Power,
  ): MinMaxFlexOptions = {
    if min > ref then
      throw new CriticalFailureException(
        s"Minimum power $min is greater than reference power $ref"
      )

    if ref > max then
      throw new CriticalFailureException(
        s"Reference power $ref is greater than maximum power $max"
      )

    new MinMaxFlexOptions(ref, min, max)
  }

  /** Creates [[MinMaxFlexOptions]] that do not allow any flexibility, meaning
    * that min = ref = max power.
    *
    * @param power
    *   The active power that the flex provider requires.
    * @return
    *   The corresponding [[MinMaxFlexOptions]].
    */
  def noFlexOption(
      power: Power
  ): MinMaxFlexOptions =
    MinMaxFlexOptions(power, power, power)
}
