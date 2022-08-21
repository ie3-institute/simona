/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages

import tech.units.indriya.ComparableQuantity

import java.util.UUID
import javax.measure.quantity.{Energy, Power}

trait FlexibilityMessage {}

object FlexibilityMessage {

  /** EmAgent requests flexibility options from connected agents
    */
  case object RequestFlexibilityOptions

  /** Connected agents provide flex options
    */
  trait ProvideFlexOptions {
    val modelUuid: UUID
  }

  /** EmAgent issues flexibility control
    */
  trait IssueFlexibilityControl

  case class ProvideMinMaxFlexOptions(
      override val modelUuid: UUID,
      suggestedPower: ComparableQuantity[Power],
      minPower: ComparableQuantity[Power],
      maxPower: ComparableQuantity[Power]
  ) extends ProvideFlexOptions

  /** @param power
    *   positive: charging, negative: discharging
    */
  case class IssuePowerCtrl(
      power: ComparableQuantity[Power]
  ) extends IssueFlexibilityControl

  case object IssueNoCtrl extends IssueFlexibilityControl
}
