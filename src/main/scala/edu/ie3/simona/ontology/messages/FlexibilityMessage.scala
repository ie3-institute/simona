/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.em.EmAgent.EmMessage
import org.apache.pekko.actor.typed.ActorRef
import squants.Power

import java.util.UUID

// TODO adapt scaladoc
// TODO split off min-max flex options into separate file
object FlexibilityMessage {

  sealed trait FlexRequest {
    val tick: Long
  }

  sealed trait FlexResponse extends EmMessage {
    val modelUuid: UUID
  }

  final case class RegisterParticipant(
      override val modelUuid: UUID,
      participant: ActorRef[FlexRequest],
      inputModel: AssetInput
  ) extends FlexResponse

  final case class ScheduleFlexRequest(
      override val modelUuid: UUID,
      tick: Long,
      scheduleKey: Option[ScheduleKey] = None
  ) extends FlexResponse

  /** EmAgent requests flexibility options from connected agents
    */
  final case class RequestFlexOptions(override val tick: Long)
      extends FlexRequest

  /** Connected agents provide flex options
    */
  trait ProvideFlexOptions extends FlexResponse

  object ProvideFlexOptions {
    def noFlexOption(
        modelUuid: UUID,
        power: Power
    ): ProvideMinMaxFlexOptions =
      ProvideMinMaxFlexOptions(modelUuid, power, power, power)
  }

  /** EmAgent issues flexibility control
    */
  trait IssueFlexControl extends FlexRequest

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
      referencePower: Power,
      minPower: Power,
      maxPower: Power
  ) extends ProvideFlexOptions {

    def fits(power: Power): Boolean =
      minPower <= power && power <= maxPower
  }

  /** Message sent by [[edu.ie3.simona.agent.participant.em.EmAgent]] that
    * specifies a power target that needs to be produced/consumed by the system
    * participant.
    * @param setPower
    *   the power that the system participant has to set. Positive: consuming,
    *   negative: producing
    */
  final case class IssuePowerCtrl(
      override val tick: Long,
      setPower: Power
  ) extends IssueFlexControl

  /** Message sent by [[edu.ie3.simona.agent.participant.em.EmAgent]] indicating
    * that no power target is set and the reference power shall be
    * produced/consumed.
    */
  final case class IssueNoCtrl(tick: Long) extends IssueFlexControl

  /** @param modelUuid
    *   model uuid of participant agent who received flex options request or
    *   issue power control
    * @param requestAtNextActivation
    *   whether to request flex options at the very next activation of EmAgent
    * @param requestAtTick
    *   tick at which flex options are foreseen to have changed
    */
  final case class FlexCtrlCompletion(
      override val modelUuid: UUID,
      result: ApparentPower,
      requestAtNextActivation: Boolean = false,
      requestAtTick: Option[Long] = None
  ) extends FlexResponse

}
