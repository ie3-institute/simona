/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import com.typesafe.scalalogging.LazyLogging
import squants.thermal.Celsius
import squants.{Meters, Temperature}

import scala.math.*

/** A collection of methods used for calculation of the
  * [[LineSegmentThermalModel]].
  */
object LineThermalModelCalculations extends LazyLogging {

  // Constants for thermal calculations
  private[ampacity] val TWO_PI: Double = 2 * Pi
  private[ampacity] val PI_OVER_FOUR: Double = Pi / 4
  private[ampacity] val REFERENCE_TEMPERATURE: Temperature = Celsius(20d)

  /** Calculates the lay factor of the screen layer which is the ratio of the
    * lay length to the external diameter of the corresponding layer of wires or
    * members in the stranded conductor.
    * @param screenLayer
    *   Cable layer of the screen with type [[ScreenLayer]]
    * @return
    *   The lay factor.
    */
  private[ampacity] def calcLayFactor(
      screenLayer: Option[ScreenLayer]
  ): Double = {
    screenLayer match {
      case Some(layer) =>
        sqrt(
          1 + (pow(
            (layer.innerDiameter.toMeters + layer.wireDiameter.toMeters) * Pi,
            2,
          ) / pow(
            layer.lengthOfLay.getOrElse(Meters(999d)).toMeters,
            2,
          ))
        )
      case None => 1.0
    }
  }

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
