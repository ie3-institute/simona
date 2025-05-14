/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion.mitigations

/** Enumeration with all congestion management steps.
  */
object MitigationSteps extends Enumeration {
  val TransformerTapChange, TopologyChange, FlexibilityDeployment = Value
}
