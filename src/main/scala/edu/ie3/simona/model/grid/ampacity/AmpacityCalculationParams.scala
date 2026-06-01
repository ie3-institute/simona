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
  */
final case class AmpacityCalculationParams(
    activateAmpacityCalculation: Boolean
)

object AmpacityCalculationParams {

  def apply(cfg: AmpacityCalculation): AmpacityCalculationParams =
    AmpacityCalculationParams(
      cfg.activateAmpacityCalculation
    )

}
