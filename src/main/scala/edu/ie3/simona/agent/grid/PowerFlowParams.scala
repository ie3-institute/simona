/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import java.time.{Duration => JavaDuration}


/** Holds all power flow configuration parameters used in
  * [[edu.ie3.simona.agent.grid]]
  *
  * @param maxSweepPowerDeviation
  *   the maximum allowed power deviation until convergence in the
  *   backward-forward-sweep algorithm is reached
  * @param epsilon
  *   Permissible deviation between actual (given) use of the grid (P, Q, |V|)
  *   and those values determined during iterative solving of the system of
  *   equations
  * @param maxIterations
  *   Maximum amount of iterations in a [[edu.ie3.powerflow.NewtonRaphsonPF]]
  * @param sweepTimeout
  *   [[org.apache.pekko.pattern.ask]] timeout for a sweep
  * @param stopOnFailure
  *   Whether to stop the GridAgent after a power flow calculation failed
  */
final case class PowerFlowParams(
    maxSweepPowerDeviation: Double,
    epsilon: Vector[Double],
    maxIterations: Int,
    sweepTimeout: JavaDuration,
    stopOnFailure: Boolean,
)
