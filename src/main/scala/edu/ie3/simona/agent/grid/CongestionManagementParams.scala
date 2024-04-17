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
    maxOptimizationIterations: Int,
    timeout: Duration,
    iteration: Int = 0,
    hasRunTransformerTapping: Boolean = false,
    hasUsedFlexOptions: Boolean = false,
) {

  def runCongestionManagement: Boolean =
    transformerTapping || topologyChanges || flexOptions

  def runTransformerTapping: Boolean =
    transformerTapping && !hasRunTransformerTapping && runOptimization

  def runTopologyChanges: Boolean = topologyChanges && runOptimization

  def useFlexOptions: Boolean = flexOptions && !hasUsedFlexOptions

  def clean: CongestionManagementParams = {
    copy(
      hasRunTransformerTapping = false,
      hasUsedFlexOptions = false,
      iteration = 0,
    )
  }

  private def runOptimization: Boolean = iteration < maxOptimizationIterations
}
