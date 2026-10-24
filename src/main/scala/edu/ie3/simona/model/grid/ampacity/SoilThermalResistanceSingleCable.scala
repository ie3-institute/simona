/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity
import edu.ie3.simona.model.grid.ampacity.LineThermalModelCalculations.calcGeometricFactor
import edu.ie3.simona.model.grid.ampacity.SoilBoundaryReflection.calcBoundaryReflectionFactor
import edu.ie3.util.scala.quantities.ThermalResistivity
import squants.space.Length
import scala.math.*

/** Soil thermal resistance calculations for a single cable in non-uniform soil,
  * based on the image-source method. Reference: Slaninka and Morgan, "External
  * thermal resistance of power cable in non-uniform soil", IEE Proceedings A,
  * 139(3) (1992).
  */
object SoilThermalResistanceSingleCable {
  private val TWO_PI: Double = 2 * Pi
  private val IMAGE_SERIES_MAX_TERMS: Int = 1000
  private val IMAGE_SERIES_CONVERGENCE_TOLERANCE: Double = 1e-12

  /** Calculates the self external thermal resistance T4 of a single cable
    * buried at a distance from a plane vertical boundary between two
    * homogeneous media, using the image-source method. Reference: Slaninka and
    * Morgan, "External thermal resistance of power cable in non-uniform soil",
    * IEE Proceedings A, 139(3) (1992), eqn. (13). For equal resistivities on
    * both sides of the boundary the method reduces exactly to
    * [[LineThermalModelCalculations.calcThermalResistanceToSoilSingleCable]].
    * @param soilResistivity
    *   The thermal resistivity of the medium in which the cable is buried
    *   (medium I).
    * @param boundaryResistivity
    *   The thermal resistivity of the adjacent medium (medium II).
    * @param depthCable
    *   The laying depth of the cable, negative below the ground surface
    *   (consistent with the other T4 methods in this object).
    * @param cableDiameter
    *   The outer diameter of the cable.
    * @param distanceToBoundary
    *   The horizontal distance from the cable axis to the vertical boundary.
    * @return
    *   The external thermal resistance between cable and soil in Kelvin * Meter
    *   / Watt per unit cable length.
    */
  def calcThermalResistanceToSoilVerticalBoundarySingleCable(
      soilResistivity: ThermalResistivity,
      boundaryResistivity: ThermalResistivity,
      depthCable: Length,
      cableDiameter: Length,
      distanceToBoundary: Length,
  ): ThermalResistivity = {
    if distanceToBoundary.toMeters <= 0 then {
      val message =
        s"The distance to the vertical boundary must be positive, but was $distanceToBoundary."
      throw new IllegalArgumentException(message)
    }
    val reflectionFactor =
      calcBoundaryReflectionFactor(soilResistivity, boundaryResistivity)
    val depth = -depthCable.toMeters
    val distance = distanceToBoundary.toMeters
    val boundaryFactor = log(sqrt(pow(distance, 2) + pow(depth, 2)) / distance)
    val geometricFactor = // TODO DF check if this calcGeometricFactor is same as ln(2h/r_s)...
      calcGeometricFactor(
        depthCable,
        cableDiameter,
      ) + reflectionFactor * boundaryFactor
    (soilResistivity / TWO_PI) * geometricFactor
  }

  /** Calculates the self external thermal resistance T4 of a single cable
    * buried beneath a horizontal layer of medium II on top of the uniform
    * medium I, using the image-source method. Reference: Slaninka and Morgan,
    * "External thermal resistance of power cable in non-uniform soil", IEE
    * Proceedings A, 139(3) (1992), Fig. 3a, eqn. (10). For equal resistivities
    * the method reduces exactly to
    * [[LineThermalModelCalculations.calcThermalResistanceToSoilSingleCable]].
    *
    * @param soilResistivity
    *   The thermal resistivity of the uniform medium I in which the cable is
    *   buried.
    * @param layerResistivity
    *   The thermal resistivity of the top layer of medium II.
    * @param depthCable
    *   The laying depth of the cable, negative below the ground surface.
    * @param cableDiameter
    *   The outer diameter of the cable.
    * @param layerThickness
    *   The thickness of the top layer.
    * @return
    *   The external thermal resistance between cable and soil in Kelvin * Meter
    *   / Watt per unit cable length.
    */
  def calcThermalResistanceToSoilHorizontalLayerBeneath(
      soilResistivity: ThermalResistivity,
      layerResistivity: ThermalResistivity,
      depthCable: Length,
      cableDiameter: Length,
      layerThickness: Length,
  ): ThermalResistivity = {
    val depth = -depthCable.toMeters
    val t = layerThickness.toMeters
    val a = depth - t
    if a <= 0 then {
      throw new IllegalArgumentException(
        s"The cable must be below the layer interface (depth > layerThickness), but depth was $depth m and layerThickness was $t m."
      )
    }
    val rs = cableDiameter.toMeters / 2
    val rho1 = soilResistivity.toKelvinMetersPerWatt
    val rho2 = layerResistivity.toKelvinMetersPerWatt
    val m1 =
      calcBoundaryReflectionFactor(soilResistivity, layerResistivity)
    val m2 = -m1
    val n1 = 2 * rho1 / (rho1 + rho2)
    val n2 = 2 * rho2 / (rho1 + rho2)
    val p = m2 * n1 * n2
    val q = m2 * m2
    var s1 = 0.0
    var s2 = 0.0
    var qi = 1.0
    var i = 1
    while i <= IMAGE_SERIES_MAX_TERMS do {
      val term1 = qi * log(1.0 / (2.0 * (a + 2.0 * i * t)))
      val term2 = qi * log(1.0 / (2.0 * (a + (2.0 * i - 1.0) * t)))
      s1 += term1
      s2 += term2
      if abs(term1) < IMAGE_SERIES_CONVERGENCE_TOLERANCE * abs(s1) &&
        abs(term2) < IMAGE_SERIES_CONVERGENCE_TOLERANCE * abs(s2)
      then i = IMAGE_SERIES_MAX_TERMS + 1
      else
        qi *= q
        i += 1
    }
    val geometricFactor =
      log(1.0 / rs) + m1 * log(1.0 / (2.0 * a)) + p * s1 - n1 * n2 * s2
    (soilResistivity / TWO_PI) * geometricFactor
  }

  /** Calculates the self external thermal resistance T4 of a single cable
    * buried within a horizontal layer of medium I (thickness a + h) on top of
    * medium II, using the image-source method. Reference: Slaninka and Morgan,
    * "External thermal resistance of power cable in non-uniform soil", IEE
    * Proceedings A, 139(3) (1992), Fig. 4a, eqn. (12). For equal resistivities
    * the method reduces exactly to
    * [[LineThermalModelCalculations.calcThermalResistanceToSoilSingleCable]].
    *
    * @param soilResistivity
    *   The thermal resistivity of the layer (medium I) in which the cable is
    *   buried.
    * @param layerResistivity
    *   The thermal resistivity of the medium II below the layer.
    * @param distanceToLowerBoundary
    *   The vertical distance from the cable axis to the lower boundary of the
    *   layer (with medium II).
    * @param distanceToSurface
    *   The vertical distance from the cable axis to the ground surface.
    * @param cableDiameter
    *   The outer diameter of the cable.
    * @return
    *   The external thermal resistance between cable and soil in Kelvin * Meter
    *   / Watt per unit cable length.
    */
  def calcThermalResistanceToSoilHorizontalLayerWithin(
      soilResistivity: ThermalResistivity,
      layerResistivity: ThermalResistivity,
      distanceToLowerBoundary: Length,
      distanceToSurface: Length,
      cableDiameter: Length,
  ): ThermalResistivity = {
    val a = distanceToLowerBoundary.toMeters
    val h = distanceToSurface.toMeters
    if a <= 0 || h <= 0 then {
      throw new IllegalArgumentException(
        s"The distances to the lower boundary and the surface must be positive, but were $distanceToLowerBoundary and $distanceToSurface."
      )
    }
    val rs = cableDiameter.toMeters / 2
    val rho1 = soilResistivity.toKelvinMetersPerWatt
    val rho2 = layerResistivity.toKelvinMetersPerWatt
    val m =
      calcBoundaryReflectionFactor(soilResistivity, layerResistivity)
    val q = m * m
    var s = 0.0
    var i = 1
    var converged = false
    while i <= IMAGE_SERIES_MAX_TERMS && !converged do {
      val t1 = pow(m, 2 * i - 1) * log(
        (
          (i - 0.5) * (a + h)
        ) / ((i - 0.5) * a + (i - 1) * h)
      )
      val t2 = pow(m, 2 * i - 1) * log(
        (
          (i - 0.5) * (a + h)
        ) / ((i - 0.5) * a + i * h)
      )
      val t3 = pow(q, i) * log((i * a + (i - 0.5) * h) / (i * (a + h)))
      val t4 = pow(q, i) * log((i * a + (i + 0.5) * h) / (i * (a + h)))
      val term = t1 + t2 + t3 + t4
      s += term
      converged = abs(term) < IMAGE_SERIES_CONVERGENCE_TOLERANCE * abs(s)
      i += 1
    }
    val geometricFactor = log(2 * h / rs) + s
    (soilResistivity / TWO_PI) * geometricFactor
  }

  /** Calculates the self external thermal resistance T4 of a single cable
    * buried near a vertical layer of medium II (thickness t) sandwiched between
    * medium I and medium III, using the image-source method. Reference:
    * Slaninka and Morgan, "External thermal resistance of power cable in
    * non-uniform soil", IEE Proceedings A, 139(3) (1992), Fig. 6a, eqn. (14).
    * For equal resistivities the method reduces exactly to
    * [[LineThermalModelCalculations.calcThermalResistanceToSoilSingleCable]].
    *
    * @param soilResistivity
    *   The thermal resistivity of the medium I in which the cable is buried.
    * @param layerResistivity
    *   The thermal resistivity of the vertical layer (medium II).
    * @param farResistivity
    *   The thermal resistivity of the medium III beyond the layer.
    * @param depthCable
    *   The laying depth of the cable, negative below the ground surface.
    * @param cableDiameter
    *   The outer diameter of the cable.
    * @param distanceToNearBoundary
    *   The horizontal distance from the cable axis to the near boundary of the
    *   vertical layer.
    * @param layerThickness
    *   The thickness of the vertical layer.
    * @return
    *   The external thermal resistance between cable and soil in Kelvin * Meter
    *   / Watt per unit cable length.
    */
  def calcThermalResistanceToSoilVerticalLayerSingleCable(
      soilResistivity: ThermalResistivity,
      layerResistivity: ThermalResistivity,
      farResistivity: ThermalResistivity,
      depthCable: Length,
      cableDiameter: Length,
      distanceToNearBoundary: Length,
      layerThickness: Length,
  ): ThermalResistivity = {
    val a = distanceToNearBoundary.toMeters
    val t = layerThickness.toMeters
    if a <= 0 || t <= 0 then {
      throw new IllegalArgumentException(
        s"The distance to the vertical layer and the layer thickness must be positive, but were $distanceToNearBoundary and $layerThickness."
      )
    }
    val depth = -depthCable.toMeters
    val rs = cableDiameter.toMeters / 2
    val rho1 = soilResistivity.toKelvinMetersPerWatt
    val m1 =
      calcBoundaryReflectionFactor(soilResistivity, layerResistivity)
    val m2 =
      calcBoundaryReflectionFactor(layerResistivity, farResistivity)
    val rho2 = layerResistivity.toKelvinMetersPerWatt
    val n1 = 2 * rho1 / (rho1 + rho2)
    val n2 = 2 * rho2 / (rho1 + rho2)
    val p = m2 * n1 * n2
    val q = -m1 * m2
    def verticalFactor(d: Double): Double =
      log(sqrt(d * d + depth * depth) / d)
    var s = 0.0
    var qi = 1.0
    var i = 1
    while i <= IMAGE_SERIES_MAX_TERMS do {
      val term = qi * verticalFactor(a + i * t)
      s += term
      if abs(term) < IMAGE_SERIES_CONVERGENCE_TOLERANCE * abs(s) then
        i = IMAGE_SERIES_MAX_TERMS + 1
      else
        qi *= q
        i += 1
    }
    val geometricFactor =
      log(2 * depth / rs) + m1 * verticalFactor(a) + p * s
    (soilResistivity / TWO_PI) * geometricFactor
  }
}
