/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.grid.data.GridAgentData.GridAgentRef
import edu.ie3.simona.agent.grid.GridAgentMessages.Responses.{
  ExchangePower,
  ExchangeVoltage,
}
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.util.scala.quantities.ReactivePower
import org.apache.pekko.actor.typed.ActorRef
import squants.Power
import squants.electro.ElectricPotential

import java.util.UUID

/** Defines all messages that can be received by a [[GridAgent]] without the
  * need for an adapter.
  */
object GridAgentMessages {

  /** Message to register in inferior grid.
    * @param gridRef
    *   The actor reference of the inferior grid.
    * @param nodes
    *   All nodes of the higher side of the transformers that connect this grid
    *   to the inferior grid.
    * @param subgridNo
    *   The number of the inferior grid.
    */
  final case class RegisterInferiorGrid(
      gridRef: GridAgentRef,
      nodes: Set[UUID],
      subgridNo: Int,
  ) extends GridAgent.InternalRequest

  /** Message to register in superior grid.
    * @param gridRef
    *   The actor reference of the superior grid.
    * @param nodes
    *   All nodes of the higher side of the transformers that connect this grid
    *   to the superior grid.
    * @param subgridNo
    *   The number of the superior grid.
    */
  final case class RegisterSuperiorGrid(
      gridRef: GridAgentRef,
      nodes: Set[UUID],
      subgridNo: Int,
  ) extends GridAgent.InternalRequest

  /** Message to register assets.
    * @param nodeToAssets
    *   A map: node uuid to set of participant actor references.
    */
  final case class RegisterParticipants(
      nodeToAssets: Map[UUID, Set[ActorRef[ParticipantAgent.Request]]]
  ) extends GridAgent.InternalRequest

  /** @param onlyOneSubGrid
    *   True, if we only have one subgrid.
    */
  final case class CompleteInitialization(onlyOneSubGrid: Boolean)
      extends GridAgent.InternalRequest

  /** Trigger used inside of [[DBFSAlgorithm]] to execute a power flow
    * calculation.
    *
    * @param tick
    *   Current tick.
    */
  final case class DoPowerFlowTrigger(tick: Long)
      extends GridAgent.InternalRequest

  /** Trigger used inside of [[DBFSAlgorithm]] to activate the superior grid
    * agent to check for deviation after two sweeps and see if the power flow
    * converges.
    *
    * @param tick
    *   Current tick.
    */
  final case class CheckPowerDifferencesTrigger(tick: Long)
      extends GridAgent.InternalRequest

  /** Trigger used inside of [[DBFSAlgorithm]] to trigger the
    * [[edu.ie3.simona.agent.grid.GridAgent]] s to prepare themselves for a new
    * sweep.
    *
    * @param tick
    *   Current tick.
    */
  final case class PrepareNextSweepTrigger(tick: Long)
      extends GridAgent.InternalRequest

  /** Trigger used inside of [[DBFSAlgorithm]] to indicate that a result has
    * been found and each [[edu.ie3.simona.agent.grid.GridAgent]] should do it's
    * cleanup work.
    *
    * @param tick
    *   Current tick.
    */
  final case class FinishGridSimulationTrigger(tick: Long)
      extends GridAgent.InternalRequest

  /** Request complex power at the nodes that the inferior sub grid shares with
    * the sender's sub grid.
    *
    * @param currentSweepNo
    *   The current sweep.
    * @param nodeUuids
    *   The UUIDs of the nodes that are bordering the sender's grid.
    */
  final case class RequestGridPower(
      currentSweepNo: Int,
      nodeUuids: Set[UUID],
      sender: ActorRef[GridAgent.Message],
  ) extends GridAgent.InternalRequest

  sealed trait ReceivedValue extends GridAgent.InternalReply {
    val sender: ActorRef[?]
  }

  sealed trait PowerResponse extends ReceivedValue

  sealed trait ProvidedPowerResponse extends PowerResponse {
    def p: Power

    def q: ReactivePower
  }

  /** Provide complex power at the nodes that the sender's sub grid shares with
    * the superior sub grid, as a reply to a [[RequestGridPower]].
    *
    * @param nodalResidualPower
    *   The complex powers of the shared nodes.
    */
  final case class GridPowerResponse(
      override val sender: ActorRef[GridAgent.Message],
      nodalResidualPower: Seq[ExchangePower],
  ) extends PowerResponse

  /** Indicate that the power flow calculation failed, as a reply to a
    * [[RequestGridPower]].
    */
  case class FailedPowerFlow(override val sender: ActorRef[GridAgent.Message])
      extends PowerResponse

  /** Provide power values as a reply to a
    * [[edu.ie3.simona.agent.participant.ParticipantAgent.RequestAssetPowerMessage]].
    *
    * @param p
    *   Unchanged active power.
    * @param q
    *   Unchanged reactive power.
    */
  final case class AssetPowerChangedMessage(
      override val sender: ActorRef[ParticipantAgent.Request],
      override val p: Power,
      override val q: ReactivePower,
  ) extends ProvidedPowerResponse

  /** Provide values as a reply to a
    * [[edu.ie3.simona.agent.participant.ParticipantAgent.RequestAssetPowerMessage]].
    * In contrast to [[AssetPowerChangedMessage]], this message indicates that
    * the same values for [[p]] and [[q]] has been sent again as in the previous
    * request.
    *
    * @param p
    *   Active power from the previous request.
    * @param q
    *   Reactive power from the previous request.
    */
  final case class AssetPowerUnchangedMessage(
      override val sender: ActorRef[ParticipantAgent.Request],
      override val p: Power,
      override val q: ReactivePower,
  ) extends ProvidedPowerResponse

  /** Request complex voltage at the nodes that the superior sub grid shares
    * with the sender's sub grid. The receiver will reply with a
    * [[SlackVoltageResponse]].
    *
    * @param currentSweepNo
    *   The current sweep.
    * @param nodeUuids
    *   The UUIDs of the nodes that are bordering the sender's grid.
    */
  final case class SlackVoltageRequest(
      currentSweepNo: Int,
      nodeUuids: Set[UUID],
      sender: ActorRef[GridAgent.Message],
  ) extends GridAgent.InternalRequest

  /** Provide complex voltage at the nodes that the sender's sub grid shares
    * with the inferior sub grid, as a reply to a [[SlackVoltageRequest]].
    *
    * @param nodalSlackVoltages
    *   The complex voltages of the shared nodes
    */
  final case class SlackVoltageResponse(
      override val sender: ActorRef[GridAgent.Message],
      currentSweepNo: Int,
      nodalSlackVoltages: Seq[ExchangeVoltage],
  ) extends ReceivedValue

  object Responses {

    /** Defining the exchanged power at one interconnection point.
      *
      * @param nodeUuid
      *   Unique identifier of the node, at which this residual power did
      *   appear.
      * @param p
      *   Active power from the previous request.
      * @param q
      *   Reactive power from the previous request.
      */
    final case class ExchangePower(
        nodeUuid: UUID,
        override val sender: ActorRef[GridAgent.Message],
        override val p: Power,
        override val q: ReactivePower,
    ) extends ProvidedPowerResponse

    /** Defining the exchanged voltage at one interconnection point.
      *
      * @param nodeUuid
      *   Unique identifier of the node for which complex voltage is shared.
      * @param e
      *   Real part of the slack voltage.
      * @param f
      *   Imaginary part of the slack voltage.
      */
    final case class ExchangeVoltage(
        nodeUuid: UUID,
        e: ElectricPotential,
        f: ElectricPotential,
    )
  }
}
