/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import edu.ie3.simona.agent.grid.congestion.CongestionManagementParams.MitigationProgress
import edu.ie3.simona.agent.grid.congestion.mitigations.MitigationSteps
import edu.ie3.simona.agent.grid.congestion.mitigations.MitigationSteps.*
import edu.ie3.simona.config.SimonaConfig.CongestionManagement

/** Holds all congestion management configuration parameters used in
  * [[edu.ie3.simona.agent.grid]]. If the parameter [[detectionEnabled]] is set
  * to false, no congestion management is run and all the other parameters are
  * ignored.
  *
  * @param detectionEnabled
  *   Defines if the congestion management is active and can be run.
  * @param enableTransformerTapChange
  *   Defines if changing the transformer tapping can be used for congestion
  *   management.
  * @param progress
  *   The progress of the congestion management.
  */
final case class CongestionManagementParams(
    detectionEnabled: Boolean,
    enableTransformerTapChange: Boolean,
    progress: MitigationProgress = MitigationProgress(),
) {

  private def anyMitigationEnabled: Boolean = enableTransformerTapChange

  def getNextStepsAndUpdate
      : (MitigationSteps.Value, CongestionManagementParams) =
    if anyMitigationEnabled then {
      val (step, updatedProgress) = progress.getNextStepsAndUpdate
      (step, copy(progress = updatedProgress))
    } else {
      (NoMeasure, this)
    }

}

object CongestionManagementParams {

  def apply(cfg: CongestionManagement): CongestionManagementParams =
    CongestionManagementParams(
      cfg.enableDetection,
      cfg.enableTransformerTapChange,
    )

  private[congestion] case class MitigationProgress(
      hasUsedTransformerTapChange: Boolean = false,
      currentIteration: Int = 0,
      maxNrOfOptimizationIterations: Int = 1,
  ) {

    def hasNextStep: Boolean = !hasUsedTransformerTapChange

    def getNextStepsAndUpdate: (MitigationSteps.Value, MitigationProgress) = {

      if hasUsedTransformerTapChange then {
        // we reset the progress and do not return a next step
        (NoMeasure, reset)
      } else {
        (
          TransformerTapChange,
          copy(hasUsedTransformerTapChange = true),
        )
      }
    }

    def reset: MitigationProgress = copy(
      hasUsedTransformerTapChange = false,
      currentIteration = 0,
    )

  }
}
