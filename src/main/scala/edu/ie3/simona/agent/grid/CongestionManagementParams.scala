/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import java.time.Duration

/** Holds all congestion management configuration parameters used in
  * [[edu.ie3.simona.agent.grid]]. If the parameter [[detectionEnabled]] is set
  * to false, no congestion management is run and all the other parameters are
  * ignored
  *
  * @param detectionEnabled
  *   defines if the congestion management is active and can be run
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
    detectionEnabled: Boolean,
    transformerTapping: Boolean,
    topologyChanges: Boolean,
    flexOptions: Boolean,
    maxOptimizationIterations: Int,
    timeout: Duration,
    iteration: Int = 0,
    hasRunTransformerTapping: Boolean = false,
    hasUsedFlexOptions: Boolean = false,
) {

  /** Returns true if the transformer tapping can be run.
    */
  def runTransformerTapping: Boolean =
    transformerTapping && !hasRunTransformerTapping && runOptimization

  /** Returns true if the topology change can be run.
    */
  def runTopologyChanges: Boolean = topologyChanges && runOptimization

  /** Returns true if flex option can be used.
    */
  def useFlexOptions: Boolean = flexOptions && !hasUsedFlexOptions

  /** Returns cleaned [[CongestionManagementParams]].
    */
  def clean: CongestionManagementParams = {
    copy(
      hasRunTransformerTapping = false,
      hasUsedFlexOptions = false,
      iteration = 0,
    )
  }

  /** Returns tru if the optimization can be run.
    */
  private def runOptimization: Boolean = iteration < maxOptimizationIterations
}
