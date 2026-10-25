/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.util.quantities.thermal.ThermalResistivity

/** Shared helper for non-uniform soil thermal resistance calculations.
  */
object SoilBoundaryReflection {

  /** Calculates the dimensionless thermal reflection factor at a plane boundary
    * between two homogeneous media. The formula is the same for vertical and
    * horizontal boundaries. Reference: Slaninka and Morgan, "External thermal
    * resistance of power cable in non-uniform soil", IEE Proceedings A, 139(3)
    * (1992), eqn. (3).
    * @param soilResistivity
    *   The thermal resistivity of the medium in which the cable is buried
    *   (medium I).
    * @param boundaryResistivity
    *   The thermal resistivity of the adjacent medium (medium II).
    * @return
    *   The dimensionless reflection factor m.
    */
  def calcBoundaryReflectionFactor(
      soilResistivity: ThermalResistivity,
      boundaryResistivity: ThermalResistivity,
  ): Double = {
    val rho1 = soilResistivity.toKelvinMetersPerWatt
    val rho2 = boundaryResistivity.toKelvinMetersPerWatt
    (rho2 - rho1) / (rho1 + rho2)
  }
}
