/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion.mitigations

import edu.ie3.simona.agent.grid.congestion.mitigations.MitigationSteps.*

case class MitigationProgress(
    hasUsedTransformerTapChange: Boolean = false,
    currentIteration: Int = 0,
    maxNrOfOptimizationIterations: Int = 1,
) {

  def getNextStepsAndUpdate
      : (Option[MitigationSteps.Value], MitigationProgress) = {
    val maxOptimizationIterationReached =
      currentIteration == maxNrOfOptimizationIterations

    hasUsedTransformerTapChange match {
      case true if maxOptimizationIterationReached =>
        // we have reached the maximal amount of iterations
        // we reset the progress and return no next step
        (None, reset)

      case true =>
        (
          Some(TransformerTapChange),
          copy(currentIteration = currentIteration + 1),
        )

      case false =>
        (
          Some(TransformerTapChange),
          copy(hasUsedTransformerTapChange = true),
        )

    }
  }

  def reset: MitigationProgress = copy(
    hasUsedTransformerTapChange = false,
    currentIteration = 0,
  )

}
