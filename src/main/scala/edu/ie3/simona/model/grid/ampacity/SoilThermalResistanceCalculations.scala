/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.SoilType
import squants.Meters
import squants.space.Length

import scala.math.*

/** A collection of methods for calculating external thermal resistance (T4) to
  * soil, thermal capacitances, and cable bed related quantities.
  */
object SoilThermalResistanceCalculations extends LazyLogging {

  /** Calculates the equivalent radius of a rectangular cable bed cross-section
    * according to Anders (1997), Eq. (9.61).
    */
  def calcCableBedEquivalentRadius(
      width: Length,
      height: Length,
  ): Length = {
    val x = math.min(width.toMeters, height.toMeters)
    val y = math.max(width.toMeters, height.toMeters)
    val ratio = y / x
    if ratio >= 3.0 then
      throw new IllegalArgumentException(
        s"Analytical equivalent radius is only valid for y/x < 3, but got y/x = $ratio. " +
          s"This case is not supported."
      )
    val lnRb =
      (x / (2 * y)) * (4 / Pi - x / y) * log(1 + (y * y) / (x * x)) + log(x / 2)
    Meters(exp(lnRb))
  }

}
