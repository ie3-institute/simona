/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.state

sealed trait GridAgentState extends AgentState

object GridAgentState {

  case object SimulateGrid extends GridAgentState

  case object CheckPowerDifferences extends GridAgentState

  case object HandlePowerFlowCalculations extends GridAgentState

}
