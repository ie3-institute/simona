/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.service.Data.PrimaryData.ComplexPower
import org.apache.pekko.actor.typed.ActorRef
import squants.Power

import java.util.UUID

/** Messages used to facilitate flexibility-based communication between
  * [[edu.ie3.simona.agent.em.EmAgent]] and
  * [[edu.ie3.simona.agent.participant.ParticipantAgent]]s.
  */
object FlexibilityMessage {

  /** Trait that is extended by all messages that are supposed to be received by
    * a controlled asset model, which could be any
    * [[edu.ie3.simona.agent.participant.ParticipantAgent]] or
    * [[edu.ie3.simona.agent.em.EmAgent]], if it is EM-controlled.
    */
  sealed trait FlexRequest {
    val tick: Long
  }

  /** Trait that is extended by all messages that are received by
    * [[edu.ie3.simona.agent.em.EmAgent]]s.
    */
  sealed trait FlexResponse extends EmAgent.Request {
    val modelUuid: UUID
  }

  /** Message that registers a controlled asset model with an
    * [[edu.ie3.simona.agent.em.EmAgent]].
    *
    * @param participant
    *   The actor reference to the controlled asset model
    * @param inputModel
    *   The asset input model of the controlled asset model
    */
  final case class RegisterControlledAsset(
      participant: ActorRef[FlexRequest],
      inputModel: AssetInput,
  ) extends FlexResponse {
    override val modelUuid: UUID = inputModel.getUuid
  }

  /** Message that schedules a flex activation for a controlled asset model at
    * given tick.
    *
    * @param modelUuid
    *   The UUID of the controlled asset model
    * @param tick
    *   The tick to schedule the controlled asset model for
    * @param scheduleKey
    *   Optionally a schedule key that unlocks the scheduler once the scheduling
    *   chain is completed
    */
  final case class ScheduleFlexActivation(
      override val modelUuid: UUID,
      tick: Long,
      scheduleKey: Option[ScheduleKey] = None,
  ) extends FlexResponse

  /** Message that activates a controlled asset agent, usually in order to
    * request [[FlexOptions]] for given tick. During initialization, no flex
    * option provision is expected.
    *
    * @param tick
    *   The tick to request [[FlexOptions]] for.
    */
  final case class FlexActivation(override val tick: Long) extends FlexRequest

  /** Message that provides [[FlexOptions]] to an
    * [[edu.ie3.simona.agent.em.EmAgent]] after they have been requested via
    * [[FlexActivation]].
    */
  final case class ProvideFlexOptions(
      override val modelUuid: UUID,
      flexOptions: FlexOptions,
  ) extends FlexResponse

  /** Message that issues flexibility control to a controlled asset model, i.e.
    * a feasible set point is delivered that the controlled asset model should
    * adhere to
    */
  trait IssueFlexControl extends FlexRequest

  /** Message sent by [[edu.ie3.simona.agent.em.EmAgent]] that specifies a power
    * target that needs to be produced/consumed by the system participant.
    *
    * @param tick
    *   The current tick
    * @param setPower
    *   The power that the system participant should produce (negative) or
    *   consume (positive)
    */
  final case class IssuePowerControl(
      override val tick: Long,
      setPower: Power,
  ) extends IssueFlexControl

  /** Message sent by [[edu.ie3.simona.agent.em.EmAgent]] indicating that no
    * power target is set and the reference power communicated by
    * [[ProvideFlexOptions]] shall be produced/consumed.
    *
    * @param tick
    *   The current tick
    */
  final case class IssueNoControl(override val tick: Long)
      extends IssueFlexControl

  /** Message sent by controlled asset models that transports the result after
    * flex control has been handled. Has to be sent before [[FlexCompletion]],
    * but is not required during initialization.
    *
    * @param modelUuid
    *   The UUID of the controlled asset model
    * @param result
    *   The apparent power that is produced/consumed by the controlled asset
    *   model, which can deviate from the set point communicated by a
    *   [[IssueFlexControl]] message if it is not feasible.
    */
  final case class FlexResult(
      override val modelUuid: UUID,
      result: ComplexPower,
  ) extends FlexResponse

  /** Message sent by controlled asset models indicating that the
    * [[IssueFlexControl]] message has been handled and the flex communication
    * for the current tick is completed.
    *
    * @param modelUuid
    *   The UUID of the controlled asset model
    * @param requestAtNextActivation
    *   Whether to request flex options at the very next activation of the
    *   receiving EM agent. This is the case if flex options change the very
    *   next second after the current tick.
    * @param requestAtTick
    *   Optionally the tick at which flex options are foreseen to have changed,
    *   i.e. the tick at which the controlled asset model would like to be
    *   activated at the latest.
    */
  final case class FlexCompletion(
      override val modelUuid: UUID,
      requestAtNextActivation: Boolean = false,
      requestAtTick: Option[Long] = None,
  ) extends FlexResponse

}
