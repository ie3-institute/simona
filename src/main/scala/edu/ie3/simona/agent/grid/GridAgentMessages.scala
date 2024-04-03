/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.agent.grid.GridAgentMessages.SlackVoltageResponse.ExchangeVoltage
import edu.ie3.simona.agent.grid.ReceivedValuesStore.ReceivedValues
import edu.ie3.simona.ontology.messages.{Activation, PowerMessage}
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import org.apache.pekko.actor.typed.ActorRef
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

  final case class WrappedResponse(response: GridAgent.Response)
      extends GridAgent.InternalRequest

  final case class WrappedPowerMessage(msg: PowerMessage)
      extends GridAgent.InternalRequest

  final case class WrappedValues(values: ReceivedValues)
      extends GridAgent.InternalRequest

  /** Wrapper for received exception.
    *
    * @param exception
    *   that was received
    */
  final case class WrappedExceptions(
      exception: Throwable
  ) extends GridAgent.InternalRequest

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
  ) extends GridAgent.InternalResponse

  object SlackVoltageResponse {

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
