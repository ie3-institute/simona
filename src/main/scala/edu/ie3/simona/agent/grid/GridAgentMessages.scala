/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.agent.grid.GridAgentMessages.Responses.{
  ExchangePower,
  ExchangeVoltage,
}
import edu.ie3.simona.agent.participant2.ParticipantAgent
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.util.scala.quantities.ReactivePower
import org.apache.pekko.actor.typed.ActorRef
import squants.Power
import squants.electro.ElectricPotential

import java.util.UUID

/** Defines all messages that can be received by a [[GridAgent]] without the
  * need for an adapter.
  */
object GridAgentMessages {

  /** GridAgent initialization data can only be constructed once all GridAgent
    * actors are created. Thus, we need an extra initialization message.
    *
    * @param gridAgentInitData
    *   The initialization data
    */
  final case class CreateGridAgent(
      gridAgentInitData: GridAgentInitData,
      unlockKey: ScheduleKey,
      onlyOneSubGrid: Boolean = false,
  ) extends GridAgent.InternalRequest

  /** Trigger used inside of [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] to
    * execute a power flow calculation
    *
    * @param tick
    *   current tick
    */
  final case class DoPowerFlowTrigger(tick: Long, currentSweepNo: Int)
      extends GridAgent.InternalRequest

  /** Trigger used inside of [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] to
    * activate the superior grid agent to check for deviation after two sweeps
    * and see if the power flow converges
    *
    * @param tick
    *   current tick
    */
  final case class CheckPowerDifferencesTrigger(tick: Long)
      extends GridAgent.InternalRequest

  /** Trigger used inside of [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] to
    * trigger the [[edu.ie3.simona.agent.grid.GridAgent]] s to prepare
    * themselves for a new sweep
    *
    * @param tick
    *   current tick
    */
  final case class PrepareNextSweepTrigger(tick: Long)
      extends GridAgent.InternalRequest

  /** Trigger used inside of [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] to
    * indicate that a result has been found and each
    * [[edu.ie3.simona.agent.grid.GridAgent]] should do it's cleanup work
    *
    * @param tick
    *   current tick
    */
  final case class FinishGridSimulationTrigger(tick: Long)
      extends GridAgent.InternalRequest

  /** Wrapper for activation values
    *
    * @param activation
    *   the tick
    */
  final case class WrappedActivation(activation: Activation)
      extends GridAgent.InternalRequest

  /** Trait for values that can be received as a response to a
    * [[GridAgent.Request]].
    */
  sealed trait ReceivedValues extends GridAgent.InternalReply

  private type PowerRequestResponse[T] = (ActorRef[T], PowerResponse)

  private type SlackVoltageRequestResponse =
    (ActorRef[GridAgent.Request], SlackVoltageResponse)

  sealed trait ReceivedPowerValues extends ReceivedValues {
    def values: Vector[(ActorRef[_], PowerResponse)]
  }

  /** Wrapper for received asset power values (p, q)
    *
    * @param values
    *   the asset power values and their senders
    */
  final case class ReceivedAssetPowerValues(
      values: Vector[PowerRequestResponse[ParticipantAgent.Request]]
  ) extends ReceivedPowerValues

  /** Wrapper for received grid power values (p, q)
    *
    * @param values
    *   the grid power values and their senders
    */
  final case class ReceivedGridPowerValues(
      values: Vector[PowerRequestResponse[GridAgent.Request]]
  ) extends ReceivedPowerValues

  /** Wrapper for received slack voltage values (v)
    *
    * @param values
    *   the slack voltage values and their senders
    */
  final case class ReceivedSlackVoltageValues(
      values: Vector[SlackVoltageRequestResponse]
  ) extends ReceivedValues

  /** Wrapper for received exception.
    *
    * @param exception
    *   that was received
    */
  final case class WrappedFailure(
      exception: Throwable
  ) extends GridAgent.InternalRequest

  /** Request complex power at the nodes that the inferior sub grid shares with
    * the sender's sub grid
    *
    * @param currentSweepNo
    *   The current sweep
    * @param nodeUuids
    *   The UUIDs of the nodes that are bordering the sender's grid
    */
  final case class RequestGridPower(
      currentSweepNo: Int,
      nodeUuids: Seq[UUID],
      sender: ActorRef[GridAgent.Request],
  ) extends GridAgent.InternalRequest

  sealed trait PowerResponse extends GridAgent.InternalReply

  sealed trait ProvidedPowerResponse extends PowerResponse {
    def p: Power

    def q: ReactivePower
  }

  /** Provide complex power at the nodes that the sender's sub grid shares with
    * the superior sub grid, as a reply to a [[RequestGridPower]].
    *
    * @param nodalResidualPower
    *   The complex powers of the shared nodes
    */
  final case class GridPowerResponse(
      nodalResidualPower: Seq[ExchangePower]
  ) extends PowerResponse

  /** Indicate that the power flow calculation failed, as a reply to a
    * [[RequestGridPower]].
    */
  case object FailedPowerFlow extends PowerResponse

  /** Provide power values as a reply to a
    * [[edu.ie3.simona.agent.participant2.ParticipantAgent.RequestAssetPowerMessage]]
    *
    * @param p
    *   Unchanged active power
    * @param q
    *   Unchanged reactive power
    */
  final case class AssetPowerChangedMessage(
      override val p: Power,
      override val q: ReactivePower,
  ) extends ProvidedPowerResponse

  /** Provide values as a reply to a
    * [[edu.ie3.simona.agent.participant2.ParticipantAgent.RequestAssetPowerMessage]].
    * In contrast to [[AssetPowerChangedMessage]], this message indicates that
    * the same values for [[p]] and [[q]] has been sent again as in the previous
    * request
    *
    * @param p
    *   Active power from the previous request
    * @param q
    *   Reactive power from the previous request
    */
  final case class AssetPowerUnchangedMessage(
      override val p: Power,
      override val q: ReactivePower,
  ) extends ProvidedPowerResponse

  /** Request complex voltage at the nodes that the superior sub grid shares
    * with the sender's sub grid
    *
    * @param currentSweepNo
    *   The current sweep
    * @param nodeUuids
    *   The UUIDs of the nodes that are bordering the sender's grid
    */
  final case class SlackVoltageRequest(
      currentSweepNo: Int,
      nodeUuids: Seq[UUID],
      sender: ActorRef[GridAgent.Request],
  ) extends GridAgent.InternalRequest

  /** Provide complex voltage at the nodes that the sender's sub grid shares
    * with the inferior sub grid, as a reply to a [[SlackVoltageRequest]].
    *
    * @param nodalSlackVoltages
    *   The complex voltages of the shared nodes
    */
  final case class SlackVoltageResponse(
      currentSweepNo: Int,
      nodalSlackVoltages: Seq[ExchangeVoltage],
  ) extends GridAgent.InternalReply

  object Responses {

    /** Defining the exchanged power at one interconnection point
      *
      * @param nodeUuid
      *   Unique identifier of the node, at which this residual power did appear
      * @param p
      *   Active power from the previous request
      * @param q
      *   Reactive power from the previous request
      */
    final case class ExchangePower(
        nodeUuid: UUID,
        override val p: Power,
        override val q: ReactivePower,
    ) extends ProvidedPowerResponse

    /** Defining the exchanged voltage at one interconnection point
      *
      * @param nodeUuid
      *   Unique identifier of the node for which complex voltage is shared
      * @param e
      *   Real part of the slack voltage
      * @param f
      *   Imaginary part of the slack voltage
      */
    final case class ExchangeVoltage(
        nodeUuid: UUID,
        e: ElectricPotential,
        f: ElectricPotential,
    )
  }
}
