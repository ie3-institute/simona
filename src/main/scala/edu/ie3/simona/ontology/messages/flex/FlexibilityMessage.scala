/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import org.apache.pekko.actor.typed.ActorRef
import squants.Power

import java.util.UUID

/** Messages used to facilitate flexibility-based communication between
  * [[edu.ie3.simona.agent.em.EmAgent]] and
  * [[edu.ie3.simona.agent.participant.ParticipantAgent]]s.
  */
object FlexibilityMessage {

  /** Trait that is extended by all messages that are supposed to be received by
    * a flex options provider, which could be any
    * [[edu.ie3.simona.agent.participant.ParticipantAgent]] or
    * [[edu.ie3.simona.agent.em.EmAgent]], if it is EM-controlled.
    */
  sealed trait FlexRequest {
    val tick: Long
  }

  /** Trait that is extended by all messages that are supposed to be received by
    * [[edu.ie3.simona.agent.em.EmAgent]]s.
    */
  sealed trait FlexResponse extends EmAgent.Request {
    val modelUuid: UUID
  }

  final case class SetPointFlexRequest(
      tick: Long,
      setPower: Power,
      nextSetPointTick: Option[Long],
  ) extends FlexRequest

  /** Message that registers a flex options provider with an
    * [[edu.ie3.simona.agent.em.EmAgent]].
    *
    * @param modelUuid
    *   The UUID of the flex options provider asset model
    * @param participant
    *   The actor reference to the flex options provider
    * @param inputModel
    *   The asset input model of the flex options provider
    */
  final case class RegisterParticipant(
      override val modelUuid: UUID,
      participant: ActorRef[FlexRequest],
      inputModel: AssetInput,
  ) extends FlexResponse

  /** Message that schedules a flex request for a flex options provider at given
    * tick.
    *
    * @param modelUuid
    *   The UUID of the flex options provider asset model
    * @param tick
    *   The tick to schedule the flex options provider for
    * @param scheduleKey
    *   Optionally a schedule key that unlocks the scheduler once the scheduling
    *   chain is completed
    */
  final case class ScheduleFlexRequest(
      override val modelUuid: UUID,
      tick: Long,
      scheduleKey: Option[ScheduleKey] = None,
  ) extends FlexResponse

  /** Message that activates a connected agent, usually in order to requests
    * flex options for given tick. During initialization, no flex option
    * provision is expected.
    *
    * @param tick
    *   The tick to request flex options for
    */
  final case class FlexActivation(override val tick: Long) extends FlexRequest

  /** Message that provides flex options to an
    * [[edu.ie3.simona.agent.em.EmAgent]] after they have been requested via
    * [[FlexActivation]]
    */
  trait ProvideFlexOptions extends FlexResponse

  /** Message that issues flexibility control to a flex options provider, i.e. a
    * feasible set point is delivered that the flex options provider should
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

  /** Message sent by flex options providers that transports the result after
    * flex control has been handled. Has to be sent before [[FlexCompletion]],
    * but is not required during initialization.
    *
    * @param modelUuid
    *   The UUID of the flex options provider asset model
    * @param result
    *   The apparent power that is produced/consumed by the flex options
    *   provider, which can deviate from the set point communicated by a
    *   [[IssueFlexControl]] message if it is not feasible.
    */
  final case class FlexResult(
      override val modelUuid: UUID,
      result: ApparentPower,
  ) extends FlexResponse

  /** Message sent by flex options providers indicating that the
    * [[IssueFlexControl]] message has been handled and the flex communication
    * for the current tick is completed.
    *
    * @param modelUuid
    *   The UUID of the flex options provider asset model
    * @param requestAtNextActivation
    *   Whether to request flex options at the very next activation of the
    *   receiving EM agent. This is the case if flex options change the very
    *   next second after the current tick.
    * @param requestAtTick
    *   Optionally the tick at which flex options are foreseen to have changed,
    * i.e. the tick at which the flex options provider would like to be
    * activated at the latest.
    */
  final case class FlexCompletion(
      override val modelUuid: UUID,
      requestAtNextActivation: Boolean = false,
      requestAtTick: Option[Long] = None,
  ) extends FlexResponse

}
