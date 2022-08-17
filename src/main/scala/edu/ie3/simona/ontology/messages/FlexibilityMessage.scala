/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages

import tech.units.indriya.ComparableQuantity

import javax.measure.quantity.{Energy, Power}

trait FlexibilityMessage {}

object FlexibilityMessage {

  /** EmAgent requests flexibility options from connected agents
    */
  case object RequestFlexibilityOptions

  /** Connected agents provide flex options
    */
  trait ProvideFlexibilityOptions

  /** EmAgent issues flexibility control
    */
  trait IssueFlexibilityControl

  case class ProvideStorageState(
      storedEnergy: ComparableQuantity[Energy],
      capacity: ComparableQuantity[Energy],
      currentChargingPower: ComparableQuantity[Power]
  ) extends ProvideFlexibilityOptions

  /** @param chargingPower
    *   positive: charging, negative: discharging
    */
  case class IssueChargingPower(
      chargingPower: ComparableQuantity[Power]
  ) extends IssueFlexibilityControl
}
