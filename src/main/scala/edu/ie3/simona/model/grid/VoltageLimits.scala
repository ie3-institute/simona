/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

case class VoltageLimits(
    vMin: Double,
    vMax: Double,
) {
  def isInLimits(voltage: Double): Boolean =
    vMin <= voltage && voltage <= vMax
}
