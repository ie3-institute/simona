/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages

import tech.units.indriya.ComparableQuantity

import java.util.UUID
import javax.measure.quantity.Power

trait FlexibilityMessage {}

object FlexibilityMessage {

  /** EmAgent requests flexibility options from connected agents
    */
  case object RequestFlexOptions

  /** Connected agents provide flex options
    */
  trait ProvideFlexOptions {
    val modelUuid: UUID
  }

  /** EmAgent issues flexibility control
    */
  trait IssueFlexControl

  /** Provides flexibility options of a system participant using reference,
    * minimum and maximum power. All powers can be negative, signifying a
    * feed-in
    *
    * @param modelUuid
    *   the uuid of the input model that references the system participant
    * @param referencePower
    *   the active power that the system participant would produce/consume
    *   normally
    * @param minPower
    *   the minimum power that the system participant allows
    * @param maxPower
    *   the maximum power that the system participant allows
    */
  case class ProvideMinMaxFlexOptions(
      override val modelUuid: UUID,
      referencePower: ComparableQuantity[Power],
      minPower: ComparableQuantity[Power],
      maxPower: ComparableQuantity[Power]
  ) extends ProvideFlexOptions

  /** Message sent by [[edu.ie3.simona.agent.participant.em.EmAgent]] that
    * specifies a power target that needs to be produced/consumed by the system
    * participant.
    * @param setPower
    *   the power that the system participant has to set. Positive: consuming,
    *   negative: producing
    */
  case class IssuePowerCtrl(
      setPower: ComparableQuantity[Power]
  ) extends IssueFlexControl

  /** Message sent by [[edu.ie3.simona.agent.participant.em.EmAgent]] indicating
    * that no power target is set and the reference power shall be
    * produced/consumed.
    */
  case object IssueNoCtrl extends IssueFlexControl
}
