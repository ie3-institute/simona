/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.util.scala.quantities.{
  JoulesPerMeterKelvin,
  ThermalCapacitance,
  ThermalResistivity,
}
import squants.space.Length
import squants.thermal.ThermalCapacity

import scala.math.*

/** A collection of methods used for calculation of the
  * [[LineSegmentThermalModel]].
  */
object LineThermalModelCalculations {

  /** Calculates the thermal resistivity of the individual layers of the cable
    * (e.g. for isolation shell as the first shell between conductor and further
    * outer layers of the cable).
    *
    * @param specificThermalResistivity
    *   The material dependent specific thermal resistance of the layer in
    *   question.
    * @param innerRadius
    *   The radius of the conductor.
    * @param outerRadius
    *   The radius of the isolation layer.
    * @return
    *   The thermal resistance of this cable shell in Kelvin * Meter / Watt per
    *   unit cable length.
    */
  def calcThermalResistanceCableShells(
      specificThermalResistivity: ThermalResistivity,
      innerRadius: Length,
      outerRadius: Length,
  ): ThermalResistivity = {
    (specificThermalResistivity / (2 * Pi)) * log(
      outerRadius.toMeters / innerRadius.toMeters
    )
  }

  /** Calculates the thermal resistivity between the cable layers and the
    * surrounding soil.
    *
    * @param specificThermalResistivity
    *   The material dependent specific thermal resistance of the layer in
    *   question.
    * @param depthCable
    *   The laying depth of the cable.
    * @param cableDiameter
    *   The outer diameter of the cable.
    * @return
    *   The thermal resistance between cable and its surrounding soil in Kelvin
    *   * Meter / Watt per unit cable length.
    */
  def calcThermalResistanceToSoil(
      specificThermalResistivity: ThermalResistivity,
      depthCable: Length,
      cableDiameter: Length,
  ): ThermalResistivity = {
    (specificThermalResistivity / (2 * Pi)) * log(
      (2d * depthCable.toMeters / cableDiameter.toMeters) + sqrt(
        pow(2 * depthCable.toMeters / cableDiameter.toMeters, 2) - 1
      )
    )
  }

  /** Calculates the thermal resistivity between the cable layers and the
    * surrounding soil.
    *
    * @param specificThermalCapacity
    *   The material dependent specific thermal capacity of the layer in
    *   question (Joule per cubic meter).
    * @param innerDiameter
    *   The inner diameter of this layer.
    * @param outerDiameter
    *   The outer diameter of this layer.
    * @return
    *   The thermal capacity of this cable layer Joule / Kelvin per unit cable
    *   length.
    */
  def calcThermalCapacityCylindrical(
      specificThermalCapacity: ThermalCapacity,
      innerDiameter: Length,
      outerDiameter: Length,
  ): ThermalCapacitance = {
    JoulesPerMeterKelvin(
      (Pi / 4) * (pow(outerDiameter.toMeters, 2) - pow(
        innerDiameter.toMeters,
        2,
      )) * specificThermalCapacity.toJoulesPerKelvin
    )
  }

  /** Determines the Van-Woermer-Coefficient for long-duration transients.
    *
    * @param diameterDielectric
    *   The inner diameter of this layer.
    * @param diameterConductor
    *   The outer diameter of this layer.
    * @return
    *   Van-Woermer coefficient as Double.
    */
  def vanWoermerCoefficientLongDurationTransients(
      diameterDielectric: Length,
      diameterConductor: Length,
  ): Double = {
    (1 / (2 * log(diameterDielectric / diameterConductor))) - (1 / (pow(
      diameterDielectric / diameterConductor,
      2,
    ) - 1))
  }

  /** Determines the Van-Woermer-Coefficient for short-duration transients.
    *
    * @param diameterDielectric
    *   The inner diameter of this layer.
    * @param diameterConductor
    *   The outer diameter of this layer.
    * @return
    *   Van-Woermer coefficient as Double.
    */
  def vanWoermerCoefficientShortTermDurationTransients(
      diameterDielectric: Length,
      diameterConductor: Length,
  ): Double = {
    (1 / (2 * log(
      diameterDielectric / diameterConductor
    ))) - (1 / ((diameterDielectric / diameterConductor) - 1))
  }
}
