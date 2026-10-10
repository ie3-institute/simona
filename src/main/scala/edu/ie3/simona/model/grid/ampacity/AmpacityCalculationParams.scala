/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.config.SimonaConfig.AmpacityCalculation

/** Holds all ampacity calculation configuration parameters used in
  * [[edu.ie3.simona.agent.grid]]. If the parameter
  * [[activateAmpacityCalculations]] is set to false, no ampacity calculation
  * will run and all the other parameters are ignored.
  *
  * @param activateAmpacityCalculation
  *   Defines whether the ampacity calculation is active.
  * @param activateParallelProximitySplitting
  *   Defines whether parallel-proximity splitting of thermal segments is
  *   active.
  * @param parallelProximityThreshold
  *   The lateral distance threshold in meters below which a neighbouring line
  *   is considered a parallel-proximity neighbour.
  */
final case class AmpacityCalculationParams(
    activateAmpacityCalculation: Boolean,
    activateParallelProximitySplitting: Boolean = false,
    parallelProximityThreshold: Option[Double] = None,
)

object AmpacityCalculationParams {

  def apply(cfg: AmpacityCalculation): AmpacityCalculationParams =
    AmpacityCalculationParams(
      cfg.activateAmpacityCalculation,
      cfg.activateParallelProximitySplitting,
      cfg.parallelProximityThreshold,
    )

}
