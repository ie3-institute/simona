/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

/** Holds all congestion management configuration parameters used in
  * [[edu.ie3.simona.agent.grid]]. If the parameter [[runCongestionManagement]]
  * is set to false, no congestion management is run and all the other
  * parameters are ignored
  *
  * @param runTransformerTapping
  *   defines if the transformer tapping should be used for tappable
  *   transformers
  * @param runTopologyChanges
  *   defines if switches should be used to change the topology of the grid
  * @param useFlexOptions
  *   defines if available [[edu.ie3.simona.agent.em.EmAgent]] should be used to
  *   resolve congestions
  */
final case class CongestionManagementParams(
    runTransformerTapping: Boolean,
    runTopologyChanges: Boolean,
    useFlexOptions: Boolean,
    hasRunTransformerTapping: Boolean = false,
    hasRunTopologyChanges: Boolean = false,
) {

  def runCongestionManagement: Boolean =
    runTransformerTapping || runTopologyChanges || useFlexOptions

  def clean: CongestionManagementParams = {
    copy(hasRunTransformerTapping = false, hasRunTopologyChanges = false)
  }
}
