/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import java.time.Duration

/** Holds all congestion management configuration parameters used in
  * [[edu.ie3.simona.agent.grid]]. If the parameter [[detectionEnabled]] is set
  * to false, no congestion management is run and all the other parameters are
  * ignored
  *
  * @param detectionEnabled
  *   defines if the congestion management is active and can be run
  * @param timeout
  *   used for asking other actors
  */
final case class CongestionManagementParams(
    detectionEnabled: Boolean,
    timeout: Duration,
)
