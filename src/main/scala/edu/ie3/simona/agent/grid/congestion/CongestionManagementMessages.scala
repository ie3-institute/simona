/*
 * © 2025-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.congestion.mitigations.MitigationSteps
import edu.ie3.simona.agent.grid.data.GridAgentData.GridAgentRef
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent

/** Messages for the congestion management.
  */
object CongestionManagementMessages {

  /** Message that informs the grid agent to start with congestion management.
    */
  case class DoCongestionManagement(
      currentTick: Long,
      results: Option[PowerFlowResultEvent],
  ) extends GridAgent.InternalRequest

  /** Message that informs all actors that about the next step that should be
    * performed.
    */
  final case class NextStep(step: MitigationSteps.Value)
      extends GridAgent.InternalRequest

  /** Message that informs all actors that the current step is started.
    */
  case object StartStep extends GridAgent.InternalRequest

  /** Message that informs all actors that the next state is the idle state.
    */
  case object GotoIdle extends GridAgent.InternalRequest
}
