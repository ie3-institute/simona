/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import edu.ie3.simona.agent.grid.GridAgent

/** Messages for the congestion management.
  */
object CMMessages {

  /** Message that indicates all actors that the current step is started.
    */
  final case object StartStep extends GridAgent.InternalRequest

  /** Message that indicates all actors that the current step is finished.
    */
  final case object FinishStep extends GridAgent.InternalRequest

  /** Message that indicates all actors that the next state is the idle state.
    */
  final case object GotoIdle extends GridAgent.InternalRequest
}
