/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.ProvideFlexOptions
import edu.ie3.util.scala.quantities.DefaultQuantities._
import squants.Power

import java.util.UUID

/** Messages that communicate interval-based flexibility with minimum, reference
  * and maximum power
  */
object MinMaxFixFlexibilityMessage {

  /** Message that provides flexibility options using reference, minimum and
    * maximum power. It is possible that the power values are either all
    * negative or all positive, meaning that feed-in or load is mandatory.
    *
    * @param modelUuid
    *   The UUID of the flex options provider asset model
    * @param ref
    *   The reference active power that the flex options provider would
    *   produce/consume regularly at the current tick, i.e. if it was not
    *   flex-controlled
    * @param min
    *   The minimum active power that the flex options provider allows at the
    *   current tick
    * @param max
    *   The maximum active power that the flex options provider allows at the
    *   current tick
    */
  final case class ProvideMinMaxFixFlexOptions private (
      override val modelUuid: UUID,
      ref: Power,
      min: Power,
      max: Power,
      fix: Power,
  ) extends ProvideFlexOptions

  object ProvideMinMaxFixFlexOptions {

    implicit class RichIterable(
        private val flexOptions: Iterable[ProvideMinMaxFixFlexOptions]
    ) extends AnyVal {
      def flexSum: (Power, Power, Power, Power) =
        flexOptions.foldLeft((zeroKW, zeroKW, zeroKW, zeroKW)) {
          case (
                (sumRef, sumMin, sumMax, sumFix),
                ProvideMinMaxFixFlexOptions(_, addRef, addMin, addMax, addFix),
              ) =>
            (
              sumRef + addRef,
              sumMin + addMin,
              sumMax + addMax,
              sumFix + addFix,
            )
        }
    }

    /** Creates a
      * [[edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions]]
      * message with sanity checks regarding the power values
      *
      * @param modelUuid
      *   The UUID of the flex options provider asset model
      * @param ref
      *   The reference active power that the flex options provider would
      *   produce/consume regularly at the current tick, i.e. if it was not
      *   flex-controlled
      * @param min
      *   The minimum active power that the flex options provider allows at the
      *   current tick
      * @param max
      *   The maximum active power that the flex options provider allows at the
      *   current tick
      * @return
      *   The
      *   [[edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions]]
      *   message
      */
    def apply(
        modelUuid: UUID,
        ref: Power,
        min: Power,
        max: Power,
        fix: Power,
    ): ProvideMinMaxFixFlexOptions = {
      if (min > ref)
        throw new CriticalFailureException(
          s"Minimum power $min is greater than reference power $ref"
        )

      if (ref > max)
        throw new CriticalFailureException(
          s"Reference power $ref is greater than maximum power $max"
        )

      new ProvideMinMaxFixFlexOptions(modelUuid, ref, min, max, fix)
    }

    /** Creates a
      * [[edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions]]
      * message that does not allow any flexibility, meaning that min = ref =
      * max power.
      *
      * @param modelUuid
      *   The UUID of the flex provider asset model
      * @param power
      *   The active power that the flex provider requires
      * @return
      *   The corresponding
      *   [[edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions]]
      *   message
      */
    def noFlexOption(
        modelUuid: UUID,
        power: Power,
    ): ProvideMinMaxFixFlexOptions =
      ProvideMinMaxFixFlexOptions(modelUuid, power, power, power, zeroKW)
  }
}
