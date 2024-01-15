/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.ontology.messages.PowerMessage.PowerResponseMessage
import edu.ie3.simona.ontology.messages.VoltageMessage.ProvideSlackVoltageMessage
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import org.apache.pekko.actor.typed.ActorRef

/** Serves as a wrapper class that allows for matches against received values in
  * [[DBFSAlgorithm]]
  */
sealed trait ReceivedValues

object ReceivedValues {

  type ParticipantPowerRequestResponse =
    (
        ActorRef[_],
        PowerResponseMessage
    ) // necessary, because participants are still classic actors
  type GridPowerRequestResponse =
    (ActorRef[GridAgentMessage], PowerResponseMessage)
  type ActorSlackVoltageRequestResponse =
    (ActorRef[GridAgentMessage], ProvideSlackVoltageMessage)

  sealed trait ReceivedTickValues extends ReceivedValues

  sealed trait ReceivedPowerValues extends ReceivedValues {
    def values: Vector[(ActorRef[_], PowerResponseMessage)]
  }

  /** Wrapper for received asset power values (p, q)
    *
    * @param values
    *   the asset power values and their senders
    */
  final case class ReceivedAssetPowerValues(
      values: Vector[ParticipantPowerRequestResponse]
  ) extends ReceivedPowerValues

  /** Wrapper for received grid power values (p, q)
    *
    * @param values
    *   the grid power values and their senders
    */
  final case class ReceivedGridPowerValues(
      values: Vector[GridPowerRequestResponse]
  ) extends ReceivedPowerValues

  /** Wrapper for received slack voltage values (v)
    *
    * @param values
    *   the slack voltage values and their senders
    */
  final case class ReceivedSlackVoltageValues(
      values: Vector[ActorSlackVoltageRequestResponse]
  ) extends ReceivedValues

  /** GridAgent initialization data can only be constructed once all GridAgent
    * actors are created. Thus, we need an extra initialization message.
    *
    * @param gridAgentInitData
    *   The initialization data
    */
  final case class CreateGridAgent(
      gridAgentInitData: GridAgentInitData,
      unlockKey: ScheduleKey
  ) extends ReceivedTickValues

  /** Trigger used inside of [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] to
    * execute a power flow calculation
    *
    * @param tick
    *   current tick
    */
  final case class DoPowerFlowTrigger(tick: Long, currentSweepNo: Int)
      extends ReceivedTickValues

  /** Trigger used inside of [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] to
    * activate the superior grid agent to check for deviation after two sweeps
    * and see if the power flow converges
    *
    * @param tick
    *   current tick
    */
  final case class CheckPowerDifferencesTrigger(tick: Long)
      extends ReceivedTickValues

  /** Trigger used inside of [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] to
    * trigger the [[edu.ie3.simona.agent.grid.GridAgent]] s to prepare
    * themselves for a new sweep
    *
    * @param tick
    *   current tick
    */
  final case class PrepareNextSweepTrigger(tick: Long)
      extends ReceivedTickValues

  /** Trigger used inside of [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] to
    * indicate that a result has been found and each
    * [[edu.ie3.simona.agent.grid.GridAgent]] should do it's cleanup work
    *
    * @param tick
    *   current tick
    */
  final case class FinishGridSimulationTrigger(tick: Long)
      extends ReceivedTickValues
}
