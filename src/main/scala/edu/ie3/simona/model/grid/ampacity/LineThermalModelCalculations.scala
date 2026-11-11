/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import com.typesafe.scalalogging.LazyLogging
import squants.Temperature
import squants.thermal.Celsius

import scala.math.*

/** A collection of methods used for calculation of the
  * [[LineSegmentThermalModel]].
  */
object LineThermalModelCalculations extends LazyLogging {

  // Constants for thermal calculations
  private[ampacity] val TWO_PI: Double = 2 * Pi
  private[ampacity] val PI_OVER_FOUR: Double = Pi / 4
  private[ampacity] val TREFOIL_COEFFICIENT: Double = 1.5
  private[ampacity] val TREFOIL_ADJUSTMENT: Double = 0.63
  private[ampacity] val REFERENCE_TEMPERATURE: Temperature = Celsius(20d)

  /** The temperatures of the line segment's thermal model nodes at the current
    * point in time.
    *
    * @param currentLineTemp1
    *   the current temperature of the first node
    * @param currentLineTemp2
    *   the current temperature of the second node
    * @param currentLineTemp3
    *   the current temperature of the third node
    * @param currentLineTemp4
    *   the current temperature of the fourth node
    * @param currentLineTemp5
    *   the current temperature of the fifth node
    * @param currentLineTemp6
    *   the current temperature of the sixth node
    * @param currentLineTemp7
    *   the current temperature of the seventh node
    * @param currentLineTemp8
    *   the current temperature of the eighth node
    * @param currentLineTemp9
    *   the current temperature of the ninth node
    * @param currentLineTemp10
    *   the current temperature of the tenth node
    */
  final case class LineTemperatures(
      currentLineTemp1: Temperature,
      currentLineTemp2: Temperature,
      currentLineTemp3: Temperature,
      currentLineTemp4: Temperature,
      currentLineTemp5: Temperature,
      currentLineTemp6: Temperature,
      currentLineTemp7: Temperature,
      currentLineTemp8: Temperature,
      currentLineTemp9: Temperature,
      currentLineTemp10: Temperature,
  )
}
