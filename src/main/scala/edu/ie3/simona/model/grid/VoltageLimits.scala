/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import tech.units.indriya.ComparableQuantity

import javax.measure.quantity.Dimensionless

/** Case class that holds information about the lower and upper voltage limit of
  * the grid.
  * @param vMin
  *   minimal allowed voltage in the grid
  * @param vMax
  *   maximal allowed voltage in the grid
  */
final case class VoltageLimits(
    vMin: ComparableQuantity[Dimensionless],
    vMax: ComparableQuantity[Dimensionless],
) {

  /** Checks if a given voltage magnitude is within the given limits.
    * @param voltage
    *   to check
    * @return
    *   true if the given voltage magnitude is within the limits
    */
  def isInLimits(voltage: ComparableQuantity[Dimensionless]): Boolean =
    vMin.isLessThanOrEqualTo(voltage) && voltage.isLessThanOrEqualTo(vMax)
}

object VoltageLimits {
  def apply(
      vMin: Double,
      vMax: Double,
  ): VoltageLimits = VoltageLimits(vMin.asPu, vMax.asPu)
}
