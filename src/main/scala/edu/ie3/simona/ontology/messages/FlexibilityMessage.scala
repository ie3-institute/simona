/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages

trait FlexibilityMessage {}

object FlexibilityMessage {

  /** EmAgent requests flexibility options from connected agents
    */
  case class RequestFlexibilityOptions()

  /** Connected agents provide flex options
    */
  case class ProvideFlexibilityOptions()

  /** EmAgent issues flexibility control
    */
  case class IssueFlexibilityControl()
}
