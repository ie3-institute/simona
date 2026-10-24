/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.model.grid.ampacity.SoilBoundaryReflection.calcBoundaryReflectionFactor
import edu.ie3.util.scala.quantities.{KelvinMetersPerWatt, ThermalResistivity}

import squants.space.Length
import scala.math.*

/** Soil thermal resistance calculations for a group of cables in non-uniform
  * soil, based on the image-source method. Reference: Morgan and Slaninka, "The
  * external thermal resistance of power cables in a group buried in non-uniform
  * soil", Electric Power Systems Research, 29(1) (1994).
  */
object SoilThermalResistanceCableGroup extends LazyLogging {

  private val FOUR_PI: Double = 4 * Pi
  private val IMAGE_SERIES_MAX_TERMS: Int = 1000
  private val IMAGE_SERIES_CONVERGENCE_TOLERANCE: Double = 1e-12

  /** Calculates the mutual external thermal resistance T4 of two cables, j and
    * k, buried in a flat formation in medium I at a plane vertical boundary to
    * medium II, using the image-source method. Reference: Morgan and Slaninka,
    * "The external thermal resistance of power cables in a group buried in
    * non-uniform soil", Electric Power Systems Research, 29 (1994), eqns.
    * (17)-(19). For equal resistivities on both sides of the boundary the
    * image-source correction vanishes.
    * @param soilResistivity
    *   The thermal resistivity of the medium in which the cables are buried
    *   (medium I).
    * @param boundaryResistivity
    *   The thermal resistivity of the adjacent medium (medium II).
    * @param depthCable
    *   The laying depth of the cables, negative below the ground surface.
    * @param distanceCableJToBoundary
    *   The horizontal distance from cable j to the vertical boundary.
    * @param distanceCableKToBoundary
    *   The horizontal distance from cable k to the vertical boundary.
    * @return
    *   The mutual external thermal resistance in Kelvin * Meter / Watt per unit
    *   cable length.
    */
  def calcThermalResistanceMutualVerticalBoundary(
      soilResistivity: ThermalResistivity,
      boundaryResistivity: ThermalResistivity,
      depthCable: Length,
      distanceCableJToBoundary: Length,
      distanceCableKToBoundary: Length,
  ): ThermalResistivity = {
    val distanceJ = distanceCableJToBoundary.toMeters
    val distanceK = distanceCableKToBoundary.toMeters
    val depth = -depthCable.toMeters
    val axialDistance = abs(distanceJ - distanceK)
    val imageDistance = distanceJ + distanceK

    if distanceJ <= 0 || distanceK <= 0
    then {
      throw new IllegalArgumentException(
        s"The distances to the vertical boundary must be positive, but were $distanceCableJToBoundary and $distanceCableKToBoundary."
      )
    }
    if axialDistance <= 0 then {
      throw new IllegalArgumentException(
        s"The two cables must be at different distances from the boundary, but both were at $distanceJ m."
      )
    }

    val reflectionFactor =
      calcBoundaryReflectionFactor(soilResistivity, boundaryResistivity)

    val uniformFactor = // TODO FIXME DF this is right for R12 and R23 but not for R13...
      log((4 * pow(depth, 2) + pow(axialDistance, 2)) / pow(axialDistance, 2))
    val imageFactor = // TODO FIXME DF this is right for R12 and R23 but not for R13...
      log((pow(imageDistance, 2) + 4 * pow(depth, 2)) / pow(imageDistance, 2))
    (soilResistivity / FOUR_PI) * (uniformFactor + reflectionFactor * imageFactor)
  }

  /** Calculates the mutual external thermal resistance T4 of two cables, j and
    * k, buried in a flat formation in medium I beneath a horizontal layer of
    * medium II, using the image-source method. Reference: Morgan and Slaninka,
    * "The external thermal resistance of power cables in a group buried in
    * non-uniform soil", Electric Power Systems Research, 29(1) (1994), eqn.
    * (8). For equal resistivities the method reduces to the Kennelly mutual
    * formula for a uniform medium with a symmetry plane at the surface.
    * @param soilResistivity
    *   The thermal resistivity of medium I in which the cables are buried.
    * @param layerResistivity
    *   The thermal resistivity of the top layer of medium II.
    * @param distanceToSurface
    *   The vertical distance from the cable axis to the ground surface (h).
    * @param axialSpacing
    *   The axial distance between the two cables (s).
    * @param distanceToBoundary
    *   The vertical distance from the cable axis to the layer interface (a).
    * @param layerThickness
    *   The thickness of the top layer (t).
    * @return
    *   The mutual external thermal resistance in Kelvin * Meter / Watt per unit
    *   cable length.
    */
  def calcThermalResistanceMutualHorizontalLayerBeneath(
      soilResistivity: ThermalResistivity,
      layerResistivity: ThermalResistivity,
      distanceToSurface: Length,
      axialSpacing: Length,
      distanceToBoundary: Length,
      layerThickness: Length,
  ): ThermalResistivity = {
    val h = distanceToSurface.toMeters
    val s = axialSpacing.toMeters
    val a = distanceToBoundary.toMeters
    val t = layerThickness.toMeters
    if h <= 0 || s <= 0 || a <= 0 || t <= 0 then {
      val message =
        s"All distances must be positive, but were h=$h m, s=$s m, a=$a m, t=$t m."
      logger.error(message)
      throw new IllegalArgumentException(message)
    }
    val rho1 = soilResistivity.toKelvinMetersPerWatt
    val rho2 = layerResistivity.toKelvinMetersPerWatt
    val m1 =
      calcBoundaryReflectionFactor(soilResistivity, layerResistivity)
    val m2 = -m1
    val n1 = 2 * rho1 / (rho1 + rho2)
    val n2 = 2 * rho2 / (rho1 + rho2)
    val p = m2 * n1 * n2
    val q = m2 * m2

    val s2 = s * s
    val uniformFactor = log(1.0 / s2)
    val imageFactor = m1 * log(1.0 / (4.0 * a * a + s2))

    if abs(m1) < 1e-15 then {
      val d2 = (2.0 * a + 2.0 * t) * (2.0 * a + 2.0 * t) + s2
      return (soilResistivity / FOUR_PI) * (uniformFactor + imageFactor - log(
        1.0 / d2
      ))
    }

    var sum1 = 0.0
    var sum2 = 0.0
    var qi = 1.0
    var i = 1
    while i <= IMAGE_SERIES_MAX_TERMS do {
      val d1 = 2.0 * a + 4.0 * i * t
      val term1 = qi * log(1.0 / (d1 * d1 + s2))
      val d2 = 2.0 * a + (4.0 * i - 2.0) * t
      val term2 = qi * log(1.0 / (d2 * d2 + s2))
      sum1 += term1
      sum2 += term2
      // TODO DF CHECK THIS / Understand this
      if abs(term1) < IMAGE_SERIES_CONVERGENCE_TOLERANCE * abs(sum1) &&
        abs(term2) < IMAGE_SERIES_CONVERGENCE_TOLERANCE * abs(sum2)
      then i = IMAGE_SERIES_MAX_TERMS + 1
      else
        qi *= q
        i += 1
    }
    val geometricFactor =
      uniformFactor + imageFactor + p * sum1 - n1 * n2 * sum2
    (soilResistivity / FOUR_PI) * geometricFactor
  }

  /** Calculates the mutual external thermal resistance T4 of two cables, j and
    * k, buried within a horizontal layer of medium I (thickness a + h) on top
    * of the semi-infinite medium II, using the image-source method.
    *
    * Reference: Morgan and Slaninka, "The external thermal resistance of power
    * cables in a group buried in non-uniform soil", Electric Power Systems
    * Research, 29(1) (1994), Fig. 4, eqn. (12), and its general derivation in
    * the Appendix (eqn. A1).
    *
    * NOTE on a typographical error in the source: the published eqn. (12)
    * prints the 4th and 5th summation terms as *identical*, both with offset
    * `(4i - 2)h`. This is a genuine typo. A line source between the two
    * parallel boundaries produces two image sets at *distinct* offsets. The 5th
    * term (upper-boundary / surface reflection series) must use `(4i + 2)h`.
    *
    * Using the identical `(4i - 2)h` in both terms is non-physical: it doubles
    * the lower-boundary image set, omits the surface-reflection series, and
    * makes the computed resistance non-monotonic (it even drops below the
    * uniform-soil value for large rho2/rho1), violating the maximum principle
    * for steady heat conduction. The corrected `(4i + 2)h` restores
    * monotonicity and matches the structure of the 1992 self-resistance formula
    * (Slaninka and Morgan, IEE Proc. A, 139(3), eqn. 12), where the
    * corresponding terms carry the distinct offsets `(i - 0.5)h` and `(i +
    * 0.5)h` (which map to `(4i - 2)h` and `(4i + 2)h` under the mutual 4x
    * scaling). For equal resistivities (m = q = 0) all image terms vanish and
    * the result reduces to the Kennelly mutual formula for a uniform medium.
    * See research notes 16 and 17. TODO DF, remove last sentence after included
    * in rtd.
    *
    * @param soilResistivity
    *   The thermal resistivity of the layer medium I in which the cables are
    *   buried.
    * @param layerResistivity
    *   The thermal resistivity of the medium II beneath the layer.
    * @param distanceToLowerBoundary
    *   The distance a from the cable axis to the lower interface (medium I /
    *   II).
    * @param distanceToSurface
    *   The distance h from the cable axis to the ground surface.
    * @param axialSpacing
    *   The axial distance s between the two cables.
    * @return
    *   The mutual external thermal resistance in Kelvin * Meter / Watt per unit
    *   cable length.
    */
  def calcThermalResistanceMutualHorizontalLayerWithin(
      soilResistivity: ThermalResistivity,
      layerResistivity: ThermalResistivity,
      distanceToLowerBoundary: Length,
      distanceToSurface: Length,
      axialSpacing: Length,
  ): ThermalResistivity = {
    val a = distanceToLowerBoundary.toMeters
    val h = distanceToSurface.toMeters
    val s = axialSpacing.toMeters
    if a <= 0 || h <= 0 || s <= 0 then {
      val message =
        s"All distances must be positive, but were a=$a m, h=$h m, s=$s m."
      logger.error(message)
      throw new IllegalArgumentException(message)
    }
    val rho1 = soilResistivity.toKelvinMetersPerWatt
    val rho2 = layerResistivity.toKelvinMetersPerWatt
    val m =
      calcBoundaryReflectionFactor(soilResistivity, layerResistivity)
    val q = m * m
    val sSquared = s * s

    val term1 = log((4.0 * h * h + sSquared) / sSquared)
    if abs(m) < 1e-15 then return (soilResistivity / FOUR_PI) * term1

    var s2sum = 0.0
    var s3sum = 0.0
    var s4sum = 0.0
    var s5sum = 0.0
    var i = 1
    while i <= IMAGE_SERIES_MAX_TERMS do {
      val num2 = (4.0 * i - 2.0) * (a + h)
      val den2 = (4.0 * i - 2.0) * a + 4.0 * (i - 1) * h
      val t2 = pow(m, 2 * i - 1) * log(
        (num2 * num2 + sSquared) / (den2 * den2 + sSquared)
      )
      val den3 = (4.0 * i - 2.0) * a + 4.0 * i * h
      val t3 = pow(m, 2 * i - 1) * log(
        (num2 * num2 + sSquared) / (den3 * den3 + sSquared)
      )
      val num4 = 4.0 * i * a + (4.0 * i - 2.0) * h
      val den4 = 4.0 * i * (a + h)
      val t4 = pow(q, i) * log(
        (num4 * num4 + sSquared) / (den4 * den4 + sSquared)
      )
      val num5 = 4.0 * i * a + (4.0 * i + 2.0) * h
      val t5 = pow(q, i) * log(
        (num5 * num5 + sSquared) / (den4 * den4 + sSquared)
      )
      s2sum += t2
      s3sum += t3
      s4sum += t4
      s5sum += t5
      if max(
          abs(pow(m, 2 * i - 1)),
          abs(pow(q, i)),
        ) < IMAGE_SERIES_CONVERGENCE_TOLERANCE
      then i = IMAGE_SERIES_MAX_TERMS + 1
      else i += 1
    }
    (soilResistivity / FOUR_PI) * (term1 + s2sum + s3sum + s4sum + s5sum)
  }

  /** Calculates the mutual external thermal resistance of two adjacent cables
    * (cables 1 and 2) near a vertical layer of medium II (thickness t)
    * sandwiched between medium I and medium III, using the image-source method.
    * Reference: Morgan and Slaninka, "The external thermal resistance of power
    * cables in a group buried in non-uniform soil", Electric Power Systems
    * Research, 29(1) (1994), Fig 7, eqn. (21).
    *
    * @param soilResistivity
    *   The thermal resistivity of medium I in which the cables are buried.
    * @param layerResistivity
    *   The thermal resistivity of the vertical layer (medium II).
    * @param farResistivity
    *   The thermal resistivity of medium III beyond the layer.
    * @param distanceToSurface
    *   The vertical distance from the cable axis to the ground surface (h).
    * @param axialSpacing
    *   The axial distance between the two cables (s).
    * @param distanceToNearBoundary
    *   The horizontal distance from the nearer cable to the near boundary (a).
    * @param layerThickness
    *   The thickness of the vertical layer (t).
    * @return
    *   The mutual external thermal resistance in Kelvin * Meter / Watt per unit
    *   cable length.
    */
  def calcThermalResistanceMutualVerticalLayer(
      soilResistivity: ThermalResistivity,
      layerResistivity: ThermalResistivity,
      farResistivity: ThermalResistivity,
      distanceToSurface: Length,
      axialSpacing: Length,
      distanceToNearBoundary: Length,
      layerThickness: Length,
  ): ThermalResistivity = {
    val h = distanceToSurface.toMeters
    val s = axialSpacing.toMeters
    val a = distanceToNearBoundary.toMeters
    val t = layerThickness.toMeters
    if h <= 0 || s <= 0 || a <= 0 || t <= 0 then {
      throw new IllegalArgumentException(
        s"All distances must be positive, but were h=$h m, s=$s m, a=$a m, t=$t m."
      )
    }
    val rho1 = soilResistivity.toKelvinMetersPerWatt
    val rho2 = layerResistivity.toKelvinMetersPerWatt
    val rho3 = farResistivity.toKelvinMetersPerWatt
    val m1 =
      calcBoundaryReflectionFactor(soilResistivity, layerResistivity)
    val m2 =
      calcBoundaryReflectionFactor(layerResistivity, farResistivity)
    val n1 = 2 * rho1 / (rho1 + rho2)
    val n2 = 2 * rho2 / (rho1 + rho2)
    val p = m2 * n1 * n2
    val q = m1 * m2

    val sSquared = s * s
    val fourHSquared = 4.0 * h * h
    val term1 = log((fourHSquared + sSquared) / sSquared)
    val dImg = 2.0 * a + s
    val term2 = m1 * log((dImg * dImg + fourHSquared) / (dImg * dImg))

    if abs(m1) < 1e-15 && abs(m2) < 1e-15 then
      return (soilResistivity / FOUR_PI) * (term1 + term2)

    var sSum = 0.0
    var qi = 1.0
    var i = 1
    while i <= IMAGE_SERIES_MAX_TERMS do {
      val d = 2.0 * a + s + 2.0 * i * t
      val term = qi * log((d * d + fourHSquared) / (d * d))
      sSum += term
      if abs(term) < IMAGE_SERIES_CONVERGENCE_TOLERANCE * abs(sSum) then
        i = IMAGE_SERIES_MAX_TERMS + 1
      else
        qi *= q
        i += 1
    }
    (soilResistivity / FOUR_PI) * (term1 + term2 + p * sSum)
  }

  /** Calculates the delta correction to the mutual thermal resistance of the
    * outer pair of cables (cables 2 and 3) near a vertical layer, arising from
    * the additional multiple reflections at the second boundary B. Reference:
    * Morgan and Slaninka, "The external thermal resistance of power cables in a
    * group buried in non-uniform soil", Electric Power Systems Research, 29(1)
    * (1994), Fig 7, eqn. (22). This delta is added to the two-media base mutual
    * resistance to obtain the total three-media mutual R.
    *
    * @param soilResistivity
    *   The thermal resistivity of medium I in which the cables are buried.
    * @param layerResistivity
    *   The thermal resistivity of the vertical layer (medium II).
    * @param farResistivity
    *   The thermal resistivity of medium III beyond the layer.
    * @param distanceToSurface
    *   The vertical distance from the cable axis to the ground surface (h).
    * @param axialSpacing
    *   The axial distance between adjacent cables (s).
    * @param distanceToNearBoundary
    *   The horizontal distance from cable 1 to the near boundary (a).
    * @param layerThickness
    *   The thickness of the vertical layer (t).
    * @return
    *   The delta correction to the mutual thermal resistance in Kelvin * Meter
    *   / Watt per unit cable length.
    */
  def calcThermalResistanceDeltaVerticalLayerOuter(
      soilResistivity: ThermalResistivity,
      layerResistivity: ThermalResistivity,
      farResistivity: ThermalResistivity,
      distanceToSurface: Length,
      axialSpacing: Length,
      distanceToNearBoundary: Length,
      layerThickness: Length,
  ): ThermalResistivity = {
    val h = distanceToSurface.toMeters
    val s = axialSpacing.toMeters
    val a = distanceToNearBoundary.toMeters
    val t = layerThickness.toMeters
    if h <= 0 || s <= 0 || a <= 0 || t <= 0 then {
      val message =
        s"All distances must be positive, but were h=$h m, s=$s m, a=$a m, t=$t m."
      logger.error(message)
      throw new IllegalArgumentException(message)
    }
    val rho1 = soilResistivity.toKelvinMetersPerWatt
    val m1 =
      calcBoundaryReflectionFactor(soilResistivity, layerResistivity)
    val m2 =
      calcBoundaryReflectionFactor(layerResistivity, farResistivity)
    val rho2 = layerResistivity.toKelvinMetersPerWatt
    val n1 = 2 * rho1 / (rho1 + rho2)
    val n2 = 2 * rho2 / (rho1 + rho2)
    val p = m2 * n1 * n2
    val q = m1 * m2

    if abs(p) < 1e-15 then return KelvinMetersPerWatt(0.0)

    val fourHSquared = 4.0 * h * h
    var sSum = 0.0
    var qi = 1.0
    var i = 1
    while i <= IMAGE_SERIES_MAX_TERMS do {
      val d = 2.0 * a + 3.0 * s + 2.0 * i * t
      val term = qi * log((d * d + fourHSquared) / (d * d))
      sSum += term
      if abs(term) < IMAGE_SERIES_CONVERGENCE_TOLERANCE * abs(sSum) then
        i = IMAGE_SERIES_MAX_TERMS + 1
      else
        qi *= q
        i += 1
    }
    KelvinMetersPerWatt(rho1 * p / FOUR_PI * sSum)
  }

  /** Calculates the delta correction to the mutual thermal resistance of the
    * far pair of cables (cables 1 and 3) near a vertical layer, arising from
    * the additional multiple reflections at the second boundary B. Reference:
    * Morgan and Slaninka, "The external thermal resistance of power cables in a
    * group buried in non-uniform soil", Electric Power Systems Research, 29(1)
    * (1994), Fig 7, eqn. (23). This delta is added to the two-media base mutual
    * resistance to obtain the total three-media mutual R.
    *
    * @param soilResistivity
    *   The thermal resistivity of medium I in which the cables are buried.
    * @param layerResistivity
    *   The thermal resistivity of the vertical layer (medium II).
    * @param farResistivity
    *   The thermal resistivity of medium III beyond the layer.
    * @param distanceToSurface
    *   The vertical distance from the cable axis to the ground surface (h).
    * @param axialSpacing
    *   The axial distance between adjacent cables (s).
    * @param distanceToNearBoundary
    *   The horizontal distance from cable 1 to the near boundary (a).
    * @param layerThickness
    *   The thickness of the vertical layer (t).
    * @return
    *   The delta correction to the mutual thermal resistance in Kelvin * Meter
    *   / Watt per unit cable length.
    */
  def calcThermalResistanceDeltaVerticalLayerFar(
      soilResistivity: ThermalResistivity,
      layerResistivity: ThermalResistivity,
      farResistivity: ThermalResistivity,
      distanceToSurface: Length,
      axialSpacing: Length,
      distanceToNearBoundary: Length,
      layerThickness: Length,
  ): ThermalResistivity = {
    val h = distanceToSurface.toMeters
    val s = axialSpacing.toMeters
    val a = distanceToNearBoundary.toMeters
    val t = layerThickness.toMeters
    if h <= 0 || s <= 0 || a <= 0 || t <= 0 then {
      val message =
        s"All distances must be positive, but were h=$h m, s=$s m, a=$a m, t=$t m."
      logger.error(message)
      throw new IllegalArgumentException(message)
    }
    val rho1 = soilResistivity.toKelvinMetersPerWatt
    val m1 =
      calcBoundaryReflectionFactor(soilResistivity, layerResistivity)
    val m2 =
      calcBoundaryReflectionFactor(layerResistivity, farResistivity)
    val rho2 = layerResistivity.toKelvinMetersPerWatt
    val n1 = 2 * rho1 / (rho1 + rho2)
    val n2 = 2 * rho2 / (rho1 + rho2)
    val p = m2 * n1 * n2
    val q = m1 * m2

    if abs(p) < 1e-15 then return KelvinMetersPerWatt(0.0)

    val hSq = h * h
    var sSum = 0.0
    var qi = 1.0
    var i = 1
    while i <= IMAGE_SERIES_MAX_TERMS do {
      val d = a + s + i * t
      val term = qi * log((d * d + hSq) / (d * d))
      sSum += term
      if abs(term) < IMAGE_SERIES_CONVERGENCE_TOLERANCE * abs(sSum) then
        i = IMAGE_SERIES_MAX_TERMS + 1
      else
        qi *= q
        i += 1
    }
    KelvinMetersPerWatt(rho1 * p / FOUR_PI * sSum)
  }
}
