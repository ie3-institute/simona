/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.model.grid.TransformerTapping
import edu.ie3.util.quantities.QuantityUtils.asPu
import tech.units.indriya.ComparableQuantity

import javax.measure.quantity.Dimensionless

object TransformerTappingSupport {

  /** Method to get the tapping options.
    *
    * @param tappings
    *   All [[TransformerTapping]] models.
    * @return
    *   The possible voltage increase and decrease.
    */
  def getTappingOptions(tappings: Set[TransformerTapping]): (
      ComparableQuantity[Dimensionless],
      ComparableQuantity[Dimensionless],
  ) = {
    // allow tapping only if all transformers support tapping
    if (tappings.forall(_.hasAutoTap)) {

      val tappingRanges = tappings.map { tapping =>
        val currentPos = tapping.currentTapPos
        val deltaV = tapping.deltaV.divide(-100)
        val increase = deltaV.multiply(tapping.tapMin - currentPos)
        val decrease = deltaV.multiply(tapping.tapMax - currentPos)

        (increase, decrease)
      }.toSeq

      if (tappings.size == 1) {
        tappingRanges(0)
      } else {
        // check for possible increase and decrease that can be applied to all transformers

        // TODO: Enhance this, to support transformer combinations with different tap deltas
        (
          tappingRanges.map(_._1).minOption.getOrElse(0.asPu),
          tappingRanges.map(_._2).maxOption.getOrElse(0.asPu),
        )
      }
    } else {
      // no tapping possible
      (0.asPu, 0.asPu)
    }
  }

}
