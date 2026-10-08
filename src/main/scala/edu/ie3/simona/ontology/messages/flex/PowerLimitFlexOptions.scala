/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.datamodel.models.result.system.{
  FlexOptionsResult,
  PowerLimitFlexOptionsResult,
}
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
final case class PowerLimitFlexOptions(
    ref: Power,
    min: Power,
    max: Power,
) extends FlexOptions {

  def +(rhs: PowerLimitFlexOptions): PowerLimitFlexOptions =
    PowerLimitFlexOptions(
      ref + rhs.ref,
      min + rhs.min,
      max + rhs.max,
    )
}

object PowerLimitFlexOptions extends FlexOptionsExtra[PowerLimitFlexOptions] {

  override val flexType: FlexType = FlexType.PowerLimit

  override def checkSetPower(
      flexOptions: PowerLimitFlexOptions,
      setPower: Power,
  ): Unit = {
    if setPower < flexOptions.min then
      throw new FlexException(
        s"The set power $setPower must not be lower than the minimum power ${flexOptions.min}!"
      )
    else if setPower > flexOptions.max then
      throw new FlexException(
        s"The set power $setPower must not be greater than the maximum power ${flexOptions.max}!"
      )
  }

  override def determineFlexPower(
      flexOptions: PowerLimitFlexOptions,
      flexCtrl: IssueFlexControl,
  ): Power =
    flexCtrl match {
      case IssuePowerControl(_, setPower) =>
        setPower.max(flexOptions.min).min(flexOptions.max)

      case IssueNoControl(_) =>
        // no override, take reference power
        flexOptions.ref
    }

  override def createResult(
      flexOptions: PowerLimitFlexOptions,
      modelUuid: UUID,
      dateTime: ZonedDateTime,
  ): FlexOptionsResult =
    new PowerLimitFlexOptionsResult(
      dateTime,
      modelUuid,
      flexOptions.ref.toMegawatts.asMegaWatt,
      flexOptions.min.toMegawatts.asMegaWatt,
      flexOptions.max.toMegawatts.asMegaWatt,
    )

  override def zero(tick: Long): PowerLimitFlexOptions = noFlexOption(zeroKW)

  extension (flexOptions: Iterable[PowerLimitFlexOptions]) {
    def flexSum: PowerLimitFlexOptions =
      flexOptions.foldLeft(PowerLimitFlexOptions(zeroKW, zeroKW, zeroKW)) {
        case (sumOptions, addOptions) =>
          sumOptions + addOptions
      }
  }

  /** Creates [[PowerLimitFlexOptions]] with sanity checks regarding the power
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
    *   The [[PowerLimitFlexOptions]].
    */
  def apply(
      ref: Power,
      min: Power,
      max: Power,
  ): PowerLimitFlexOptions = {
    if min > ref then
      throw new CriticalFailureException(
        s"Minimum power $min is greater than reference power $ref"
      )

    if ref > max then
      throw new CriticalFailureException(
        s"Reference power $ref is greater than maximum power $max"
      )

    new PowerLimitFlexOptions(ref, min, max)
  }

  /** Creates [[PowerLimitFlexOptions]] that do not allow any flexibility,
    * meaning that min = ref = max power.
    *
    * @param power
    *   The active power that the flex provider requires.
    * @return
    *   The corresponding [[PowerLimitFlexOptions]].
    */
  def noFlexOption(
      power: Power
  ): PowerLimitFlexOptions =
    PowerLimitFlexOptions(power, power, power)

}
