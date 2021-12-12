/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.trigger

import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.agent.participant.data.Data.PrimaryData
import edu.ie3.simona.agent.participant.statedata.InitializeStateData
import edu.ie3.simona.api.ExtSimAdapter.InitExtSimAdapter
import edu.ie3.simona.service.ServiceStateData
import edu.ie3.simona.util.SimonaConstants

sealed trait Trigger {
  def tick: Long
}

/** General trigger for [[edu.ie3.simona.agent.SimonaAgent]] s
  */
object Trigger {

  /** Initialization Triggers
    */
  sealed trait InitializeTrigger extends Trigger {
    override val tick: Long = SimonaConstants.INIT_SIM_TICK
  }

  /** Trigger with additional information needed to initialize a participant
    * agent. With the receive, the initialization process should be triggered.
    *
    * @param initData
    *   Additional information needed for agent initialization
    * @tparam PD
    *   Type of [[PrimaryData]], that will be covered by the agent
    * @tparam I
    *   Type of [[InitializeStateData]], that is needed by the agent
    */
  final case class InitializeParticipantAgentTrigger[
      PD <: PrimaryData,
      +I <: InitializeStateData[PD]
  ](
      initData: I
  ) extends InitializeTrigger

  /** Trigger to initialize ExtSimScheduler
    */
  final case class InitializeExtSimAdapterTrigger(
      initializeStateData: InitExtSimAdapter
  ) extends InitializeTrigger

  /** Trigger to initialize the service agents (normally during simulation
    * initialization)
    */
  final case class InitializeServiceTrigger[+I <: ServiceStateData](
      initializeStateData: I
  ) extends InitializeTrigger

  /** Trigger to initialize grid agents
    */
  final case class InitializeGridAgentTrigger(
      gridAgentInitData: GridAgentInitData
  ) extends InitializeTrigger

  /** Trigger to start a general activity e.g. reactivate the actor. May only be
    * sent by SimScheduler
    */
  final case class ActivityStartTrigger(tick: Long) extends Trigger

  sealed trait ParticipantTrigger extends Trigger
  object ParticipantTrigger {

    /** Trigger used by AssetAgents to trigger their calculation
      *
      * @param tick
      *   The tick in which the calculation may be carried out
      */
    final case class StartCalculationTrigger(tick: Long)
        extends ParticipantTrigger
  }

  /** Trigger used to start a grid simulation
    * @param tick
    *   current tick
    */
  final case class StartGridSimulationTrigger(tick: Long) extends Trigger

  /** Trigger used inside of [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] to
    * execute a power flow calculation
    * @param tick
    *   current tick
    */
  final case class DoPowerFlowTrigger(tick: Long, currentSweepNo: Int)
      extends Trigger

  /** Trigger used inside of [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] to
    * activate the superior grid agent to check for deviation after two sweeps
    * and see if the power flow converges
    * @param tick
    *   current tick
    */
  final case class CheckPowerDifferencesTrigger(tick: Long) extends Trigger

  /** Trigger used inside of [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] to
    * trigger the [[edu.ie3.simona.agent.grid.GridAgent]] s to prepare
    * themselves for a new sweep
    * @param tick
    *   current tick
    */
  final case class PrepareNextSweepTrigger(tick: Long) extends Trigger

  /** Trigger used inside of [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] to
    * indicate that a result has been found and each
    * [[edu.ie3.simona.agent.grid.GridAgent]] should do it's cleanup work
    * @param tick
    *   current tick
    */
  final case class FinishGridSimulationTrigger(tick: Long) extends Trigger

}
