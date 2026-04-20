/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.api.data.model.em.SetPoint
import edu.ie3.simona.api.data.model.em.SetPoint.{
  AggregatedSetPoint,
  DisaggregatedSetPoints,
}
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.service.Data.PrimaryData.ComplexPower
import edu.ie3.simona.service.DataTimeType
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.scala.quantities.QuantityConversionUtils.toQuantity
import org.apache.pekko.actor.typed.ActorRef
import squants.Power

import java.util.UUID
import scala.jdk.CollectionConverters.MapHasAsJava

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
  sealed trait FlexResponse {
    val modelUuid: UUID
  }

  /** Message that registers a controlled asset model with an
    * [[edu.ie3.simona.agent.em.EmAgent]].
    *
    * @param participant
    *   The actor reference to the controlled asset model
    * @param assetInput
    *   The asset input model of the controlled asset model
    */
  final case class RegisterControlledAsset(
      participant: ActorRef[FlexRequest],
      assetInput: AssetInput,
  ) extends FlexResponse {
    override val modelUuid: UUID = assetInput.getUuid
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

  /** Message that activates a controlled asset agent for initialization. No
    * flex option provision is expected. Initialization is considered complete
    * when a [[FlexCompletion]] is received as an answer.
    *
    * @param flexType
    *   The flexibility type to calculate [[FlexOptions]] for.
    * @param dataTimeType
    *   The data time type of [[FlexOptions]] to be calculated.
    */
  final case class FlexInit(flexType: FlexType, dataTimeType: DataTimeType)
      extends FlexRequest {
    override val tick: Long = INIT_SIM_TICK
  }

  /** Message that activates a controlled asset agent in order to request
    * [[FlexOptions]] (provided by [[ProvideFlexOptions]]) for given tick.
    *
    * @param tick
    *   The tick to request [[FlexOptions]] for.
    */
  final case class FlexActivation(
      override val tick: Long,
      force: Boolean = false,
  ) extends FlexRequest

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
    * adhere to. Sending agent expects a [[FlexCompletion]] as a reply.
    */
  trait IssueFlexControl extends FlexRequest {

    def toExt(receiver: UUID): SetPoint
  }

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
  ) extends IssueFlexControl {
    override def toExt(receiver: UUID): SetPoint = new AggregatedSetPoint(
      receiver,
      setPower.toQuantity,
    )
  }

  /** Message sent by [[edu.ie3.simona.agent.em.EmAgent]] that specifies
    * disaggregated power values that needs to be produced/consumed by the
    * system participant.
    *
    * @param tick
    *   The current tick
    * @param setPowers
    *   A map: uuid to power that should be produced (negative) or consumed
    *   (positive)
    */
  final case class IssueDisaggregatedControl(
      override val tick: Long,
      setPowers: Map[UUID, Power],
  ) extends IssueFlexControl {
    override def toExt(receiver: UUID): SetPoint = {
      val disaggregated = setPowers.map { case (uuid, power) =>
        uuid -> new PValue(power.toQuantity)
      }.asJava

      new DisaggregatedSetPoints(receiver, disaggregated)
    }
  }

  /** Message sent by [[edu.ie3.simona.agent.em.EmAgent]] indicating that no
    * power target is set and the reference power communicated by
    * [[ProvideFlexOptions]] shall be produced/consumed.
    *
    * @param tick
    *   The current tick
    */
  final case class IssueNoControl(override val tick: Long)
      extends IssueFlexControl {
    override def toExt(receiver: UUID): SetPoint = new AggregatedSetPoint(
      receiver
    )
  }

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
