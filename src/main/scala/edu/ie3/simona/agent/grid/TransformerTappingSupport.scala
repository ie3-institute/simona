/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.model.grid.TransformerTapping
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroPU
import squants.Dimensionless

object TransformerTappingSupport {

  /** Method to get the tapping options.
    *
    * @param tappings
    *   All [[TransformerTapping]] models.
    * @return
    *   The possible voltage increase and decrease.
    */
  def getTappingOptions(tappings: Set[TransformerTapping]): (
      Dimensionless,
      Dimensionless,
  ) = {
    // allow tapping only if all transformers support tapping
    if tappings.forall(_.hasAutoTap) then {

      val tappingRanges = tappings.map { tapping =>
        val currentPos = tapping.currentTapPos
        val deltaV = tapping.deltaV / -1
        val increase = deltaV * (tapping.tapMin - currentPos)
        val decrease = deltaV * (tapping.tapMax - currentPos)

        (increase, decrease)
      }.toList

      tappingRanges.size match {
        case 1 =>
          tappingRanges(0)
        case _ =>
          // check for possible increase and decrease that can be applied to all transformers

          // TODO #1553: Enhance this, to support transformer combinations with different tap deltas
          val (increases, decreases) = tappingRanges.unzip
          (
            increases.minByOption(_.toEach).getOrElse(zeroPU),
            decreases.maxByOption(_.toEach).getOrElse(zeroPU),
          )
      }

    } else {
      // no tapping possible
      (zeroPU, zeroPU)
    }
  }

}
