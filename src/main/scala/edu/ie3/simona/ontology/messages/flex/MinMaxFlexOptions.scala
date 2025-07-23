/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.datamodel.models.result.system.FlexOptionsResult
import edu.ie3.simona.exceptions.{CriticalFailureException, FlexException}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  IssueFlexControl,
  IssueNoControl,
  IssuePowerControl,
}
import edu.ie3.util.quantities.QuantityUtils.*
import edu.ie3.util.scala.quantities.DefaultQuantities.*
import squants.Power

import java.time.ZonedDateTime
import java.util.UUID

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

object MinMaxFlexOptions extends FlexOptionsExtra[MinMaxFlexOptions] {

  override val flexType: FlexType = FlexType.MinMax

  /** Checks whether given setPower fits the provided flex options, i.e. whether
    * the set point is feasible given the flex options.
    *
    * @param flexOptions
    *   The flex options that the set point has to fit.
    * @param setPower
    *   The set point.
    */
  override def checkSetPower(
      flexOptions: MinMaxFlexOptions,
      setPower: Power,
  ): Unit = {
    if (setPower < flexOptions.min)
      throw new FlexException(
        s"The set power $setPower must not be lower than the minimum power ${flexOptions.min}!"
      )
    else if (setPower > flexOptions.max)
      throw new FlexException(
        s"The set power $setPower must not be greater than the maximum power ${flexOptions.max}!"
      )
  }

  /** Determines the set point given a flex options message and a flex control
    * message. Also validates the resulting power.
    *
    * @param flexOptions
    *   The flex options.
    * @param flexCtrl
    *   The flex control message.
    * @return
    *   The resulting power set point.
    */
  override def determineFlexPower(
      flexOptions: MinMaxFlexOptions,
      flexCtrl: IssueFlexControl,
  ): Power =
    flexCtrl match {
      case IssuePowerControl(_, setPower) =>
        // sanity check: setPower is in range of latest flex options
        checkSetPower(flexOptions, setPower)

        setPower

      case IssueNoControl(_) =>
        // no override, take reference power
        flexOptions.ref
    }

  override def createResult(
      flexOptions: MinMaxFlexOptions,
      modelUuid: UUID,
      dateTime: ZonedDateTime,
  ): FlexOptionsResult =
    new FlexOptionsResult(
      dateTime,
      modelUuid,
      flexOptions.ref.toMegawatts.asMegaWatt,
      flexOptions.min.toMegawatts.asMegaWatt,
      flexOptions.max.toMegawatts.asMegaWatt,
    )

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
    if (min > ref)
      throw new CriticalFailureException(
        s"Minimum power $min is greater than reference power $ref"
      )

    if (ref > max)
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
