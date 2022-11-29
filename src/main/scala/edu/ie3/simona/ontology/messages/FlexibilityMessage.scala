/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages

import edu.ie3.simona.ontology.trigger.Trigger
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import tech.units.indriya.ComparableQuantity

import java.util.UUID
import javax.measure.quantity.Power

// TODO adapt scaladoc
sealed trait FlexibilityMessage

object FlexibilityMessage {

  /** EmAgent requests flexibility options from connected agents
    */
  final case class RequestFlexOptions(tick: Long)
      extends FlexibilityMessage
      with Trigger

  /** Connected agents provide flex options
    */
  trait ProvideFlexOptions extends FlexibilityMessage {
    val modelUuid: UUID
  }

  object ProvideFlexOptions {
    def noFlexOption(
        uuid: UUID,
        power: ComparableQuantity[Power]
    ): ProvideMinMaxFlexOptions =
      ProvideMinMaxFlexOptions(uuid, power, power, power)
  }

  /** EmAgent issues flexibility control
    */
  trait IssueFlexControl extends FlexibilityMessage with Trigger

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
  final case class ProvideMinMaxFlexOptions(
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
  final case class IssuePowerCtrl(
      tick: Long,
      setPower: ComparableQuantity[Power]
  ) extends IssueFlexControl

  /** Message sent by [[edu.ie3.simona.agent.participant.em.EmAgent]] indicating
    * that no power target is set and the reference power shall be
    * produced/consumed.
    */
  final case class IssueNoCtrl(tick: Long) extends IssueFlexControl

  final case class RevokeFlexTrigger(
      modelUuid: UUID,
      revokeTick: Long
  )

  /** @param modelUuid
    *   model uuid of participant agent who received flex options request or
    *   issue power control
    * @param revokeRequestAtTick
    *   tick for which the participant agent's flex options request should be
    *   revoked
    * @param requestAtNextActivation
    *   whether to request flex options at the very next activation of EmAgent
    * @param requestAtTick
    *   tick at which flex options are foreseen to have changed
    */
  final case class FlexCtrlCompletion(
      modelUuid: UUID,
      revokeRequestAtTick: Option[Long] = None,
      requestAtNextActivation: Boolean = false,
      requestAtTick: Option[Long] = None
  ) extends FlexibilityMessage

}
