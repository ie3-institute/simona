/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import java.time.Duration

/** Holds all congestion management configuration parameters used in
  * [[edu.ie3.simona.agent.grid]]. If the parameter [[runCongestionManagement]]
  * is set to false, no congestion management is run and all the other
  * parameters are ignored
  *
  * @param transformerTapping
  *   defines if the transformer tapping should be used for tappable
  *   transformers
  * @param topologyChanges
  *   defines if switches should be used to change the topology of the grid
  * @param flexOptions
  *   defines if available [[edu.ie3.simona.agent.em.EmAgent]] should be used to
  *   resolve congestions
  */
final case class CongestionManagementParams(
    transformerTapping: Boolean,
    topologyChanges: Boolean,
    flexOptions: Boolean,
    timeout: Duration,
    hasRunTransformerTapping: Boolean = false,
    hasRunTopologyChanges: Boolean = false,
) {

  def runCongestionManagement: Boolean =
    transformerTapping || topologyChanges || flexOptions

  def runTransformerTapping: Boolean =
    transformerTapping && !hasRunTransformerTapping

  def runTopologyChanges: Boolean = topologyChanges && !hasRunTopologyChanges

  def useFlexOptions: Boolean = flexOptions

  def clean: CongestionManagementParams = {
    copy(hasRunTransformerTapping = false, hasRunTopologyChanges = false)
  }
}

object CongestionManagementParams {
  object CongestionManagementSteps extends Enumeration {
    val TransformerTapping, TopologyChanges, UsingFlexibilities = Value
  }
}
