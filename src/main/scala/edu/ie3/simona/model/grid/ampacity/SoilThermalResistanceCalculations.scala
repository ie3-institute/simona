/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.SoilType
import edu.ie3.simona.model.grid.ampacity.LineThermalModelCalculations.{
  PI_OVER_FOUR,
  TREFOIL_ADJUSTMENT,
  TREFOIL_COEFFICIENT,
  TWO_PI,
}
import edu.ie3.simona.model.grid.ampacity.SoilTypeExtensions.*
import edu.ie3.util.quantities.energy.PowerPerLength
import edu.ie3.util.quantities.thermal.*
import edu.ie3.util.scala.quantities.QuantityConversionUtils.*
import squants.Meters
import squants.space.*
import tech.units.indriya.ComparableQuantity

import javax.measure.quantity.Temperature as MeasureTemperature
import scala.math.*

/** A collection of methods for calculating external thermal resistance (T4) to
  * soil, thermal capacitances, and cable bed related quantities.
  */
object SoilThermalResistanceCalculations extends LazyLogging {

  /** Calculates the thermal resistivity between the cable layers and the
    * surrounding soil for a single cable (e.g. Three-Core-Cable)
    * @param specificThermalResistivityGround
    *   The material dependent specific thermal resistance of the surrounding
    *   soil.
    * @param depthCable
    *   The laying depth of the cable.
    * @param cableDiameter
    *   The outer diameter of the cable.
    * @return
    *   The thermal resistance between cable and its surrounding soil in Kelvin
    *   * Meter / Watt per unit cable length.
    */
  def calcThermalResistanceToSoilSingleCable(
      specificThermalResistivityGround: ThermalResistivity,
      depthCable: Length,
      cableDiameter: Length,
  ): ThermalResistivity =
    (specificThermalResistivityGround / TWO_PI) * calcGeometricFactor(
      depthCable,
      cableDiameter,
    )

  /** Calculates the geometrical factor of the single core cable to its mirrored
    * (Kennelly method).
    * @param depthCable
    *   The laying depth of the cable.
    * @param cableDiameter
    *   The outer diameter of the cable.
    * @return
    *   The thermal resistance between cable and its surrounding soil in Kelvin
    *   * Meter / Watt per unit cable length.
    */
  def calcGeometricFactor(
      depthCable: Length,
      cableDiameter: Length,
  ): Double = {
    val normalizationFactor = (depthCable * -2d) / cableDiameter
    log(normalizationFactor + sqrt(pow(normalizationFactor, 2) - 1))
  }

  /** Calculates the thermal resistivity of the middle single core cable
    * (hottest cable) for a flat formation of three single core cables. Cable
    * formation: (A) --- (B) --- (C). Reference Anders Rating of electric power
    * cables: ampacity computations for transmission, distribution, and
    * industrial applications p. 215.
    */
  def calcThermalResistanceToSoilThreeSingleCoreFlatFormationDistance(
      specificThermalResistivityGround: ThermalResistivity,
      depthCables: Length,
      diameterCableB: Length,
      distanceOfCables: Length,
      lossesCableA: PowerPerLength,
      lossesCableB: PowerPerLength,
      lossesCableC: PowerPerLength,
  ): ThermalResistivity = {
    val distancePtoKDashDividedByDistancePtoK = sqrt(
      (pow(distanceOfCables.toMeters, 2) + pow(
        2 * depthCables.toMeters * -1,
        2,
      )) / (pow(distanceOfCables.toMeters, 2) + pow(0, 2))
    )

    val thermalInfluenceCableAonB =
      (lossesCableA / lossesCableB) * log(distancePtoKDashDividedByDistancePtoK)
    val thermalInfluenceCableConB =
      (lossesCableC / lossesCableB) * log(distancePtoKDashDividedByDistancePtoK)
    val thermalResistanceShareOfCableB =
      calcGeometricFactor(depthCables, diameterCableB)

    specificThermalResistivityGround / TWO_PI * (thermalResistanceShareOfCableB + thermalInfluenceCableAonB + thermalInfluenceCableConB)
  }

  /** Calculates the thermal resistivity of the middle single core cable
    * (hottest cable) for a flat formation of three single core cables that
    * touches each other. Cable formation: (A)(B)(C).
    */
  private[ampacity] def calcThermalResistanceToSoilThreeSingleCoreFlatFormationTouching(
      specificThermalResistivityGround: ThermalResistivity,
      depthCables: Length,
      diameterCableB: Length,
  ): ThermalResistivity = {
    val u = (depthCables * -2) / diameterCableB

    if u < 5 then
      logger.warn(
        s"The formula for calculating the thermal resistance to soil for three single core cables in flat formation that touch each other is only valid for u > 5. The calculated value may be inaccurate, because value of u is $u in this case."
      )

    specificThermalResistivityGround * (0.475 * log(2 * u) - 0.346)
    /*FIXME DF Check
      An Improved Formula for External Thermal  Resistance of Three Buried Single-Core Metal-Sheathed Touching Cables in Flat Formation by Niksa Kovac, George Anders, and Dragan Poljak
     */
  }

  /** Calculates the thermal resistivity of the top single core cable (hottest
    * cable) for a trefoil touching formation.
    */
  def calcThermalResistanceToSoilThreeSingleCoreTrefoilTouching(
      specificThermalResistivityGround: ThermalResistivity,
      depthToCenter: Length,
      diameterCable: Length,
  ): ThermalResistivity = {
    val u = (depthToCenter * -2) / diameterCable

    KelvinMetersPerWatt(
      TREFOIL_COEFFICIENT * specificThermalResistivityGround.toKelvinMetersPerWatt / Pi
    ) * (log(2 * u) - TREFOIL_ADJUSTMENT)
  }

  /** Calculates the external thermal resistance T4 for a cable (or cable
    * formation) in a horizontally layered, non-uniform soil using the
    * image-source method.
    */
  private[ampacity] def calcThermalResistanceToSoilNonUniform(
      formation: LayoutFormation,
      depthCables: Length,
      cableDiameter: Length,
      distanceCables: Length,
      lossesLeading: PowerPerLength,
      lossesMiddle: PowerPerLength,
      lossesLagging: PowerPerLength,
      soilLayers: Seq[LineSegmentThermalModel.SoilLayerInfo],
      temperatureDifference: ComparableQuantity[MeasureTemperature],
  ): Option[ThermalResistivity] = {
    if soilLayers.size < 2 then return None

    val depth = -depthCables.toMeters // positive depth below the surface
    val sorted = soilLayers
      .map { layer =>
        val cUpper = layer.upperDepth.toMeters
        val cLower = layer.lowerDepth.toMeters
        // Convert the (negative) boundary coordinates to positive depths below
        // the surface, robustly, regardless of which field holds which bound.
        val top = -math.max(cUpper, cLower) // shallow boundary
        val bottom = -math.min(cUpper, cLower) // deep boundary
        (top, bottom, layer.soilType)
      }
      .sortBy(_._1) // surface layer (top closest to 0) first
    val resistivity = (soilType: SoilType) =>
      soilType.currentThermalResistivity(temperatureDifference).toSquants

    val cableInTopLayer = sorted.head._2 >= depth
    val (topResistivity, belowResistivity, aMeters, hMeters, tMeters) =
      if cableInTopLayer then
        // Cable within the topmost layer: a = distance to the lower boundary,
        // h = distance to the surface, t unused.
        (
          resistivity(sorted.head._3),
          resistivity(sorted(1)._3),
          sorted.head._2 - depth,
          depth,
          0.0,
        )
      else
        // Cable in the layer below the top layer: t = thickness of the top
        // layer, a = distance to its lower boundary, h = distance to surface.
        (
          resistivity(sorted.head._3),
          resistivity(sorted(1)._3),
          depth - sorted.head._2,
          depth,
          sorted.head._2,
        )

    val selfResistance =
      if cableInTopLayer then
        SoilThermalResistanceSingleCable
          .calcThermalResistanceToSoilHorizontalLayerWithin(
            topResistivity,
            belowResistivity,
            Meters(aMeters),
            Meters(hMeters),
            cableDiameter,
          )
      else
        SoilThermalResistanceSingleCable
          .calcThermalResistanceToSoilHorizontalLayerBeneath(
            belowResistivity,
            topResistivity,
            Meters(-hMeters),
            cableDiameter,
            Meters(tMeters),
          )

    val mutualTerm: ThermalResistivity =
      if formation == LayoutFormation.FlatDistance &&
        abs(lossesMiddle.toWattsPerMeter) >= 1e-15
      then
        val mutualSingle =
          if cableInTopLayer then
            SoilThermalResistanceCableGroup
              .calcThermalResistanceMutualHorizontalLayerWithin(
                topResistivity,
                belowResistivity,
                Meters(aMeters),
                Meters(hMeters),
                distanceCables,
              )
          else
            SoilThermalResistanceCableGroup
              .calcThermalResistanceMutualHorizontalLayerBeneath(
                belowResistivity,
                topResistivity,
                Meters(hMeters),
                distanceCables,
                Meters(aMeters),
                Meters(tMeters),
              )
        val weightLeading =
          lossesLeading.toWattsPerMeter / lossesMiddle.toWattsPerMeter
        val weightLagging =
          lossesLagging.toWattsPerMeter / lossesMiddle.toWattsPerMeter
        mutualSingle * (weightLeading + weightLagging)
      else KelvinMetersPerWatt(0.0)

    Some(selfResistance + mutualTerm)
  }

  /** Calculates the thermal resistivity per length unit for a cylindrical area.
    */
  def calcThermalCapacityCylindrical(
      volumetricHeatCapacityMaterial: VolumetricHeatCapacity,
      innerDiameter: Length,
      outerDiameter: Length,
  ): ThermalCapacityPerLength = {
    val areaDifference = SquareMeters(
      (pow(outerDiameter.toMeters, 2) -
        pow(innerDiameter.toMeters, 2)) * PI_OVER_FOUR
    )

    volumetricHeatCapacityMaterial * areaDifference
  }

  /** Calculates the thermal resistivity per length unit for a rectangular area.
    */
  def calcThermalCapacityRectangular(
      volumetricHeatCapacityMaterial: VolumetricHeatCapacity,
      width: Length,
      height: Length,
  ): ThermalCapacityPerLength =
    val area = width * height
    volumetricHeatCapacityMaterial * area

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

  /** Calculates the Kennelly outer geometric factor.
    */
  private def calcCableBedOuterGeometricFactor(
      bedCenterDepth: Length,
      bedRadius: Length,
  ): Double = {
    val ratio = bedCenterDepth.toMeters / bedRadius.toMeters
    log(ratio + sqrt(pow(ratio, 2) - 1))
  }

  /** Calculates the outer-region T4 correction for a cable bed.
    */
  def calcCableBedT4OuterCorrection(
      soilResistivity: ThermalResistivity,
      bedResistivity: ThermalResistivity,
      bedCenterDepth: Length,
      bedRadius: Length,
      lossWeightFactor: Double,
  ): ThermalResistivity = {
    val geometricFactor =
      calcCableBedOuterGeometricFactor(bedCenterDepth, bedRadius)
    KelvinMetersPerWatt(
      ((soilResistivity.toKelvinMetersPerWatt - bedResistivity.toKelvinMetersPerWatt) /
        TWO_PI) * lossWeightFactor * geometricFactor
    )
  }

  /** Splits the total external thermal resistance into the bedding-region part
    * and the outer-soil-region part.
    */
  def calcCableBedT4Split(
      baseT4: ThermalResistivity,
      bedResistivity: ThermalResistivity,
      soilResistivity: ThermalResistivity,
      bedCenterDepth: Length,
      bedRadius: Length,
      lossWeightFactor: Double,
  ): (ThermalResistivity, ThermalResistivity) = {
    val geometricFactor =
      calcCableBedOuterGeometricFactor(bedCenterDepth, bedRadius)
    val outerShare = (lossWeightFactor / TWO_PI) * geometricFactor
    val t4Soil =
      KelvinMetersPerWatt(soilResistivity.toKelvinMetersPerWatt * outerShare)
    val t4BedOuterPortion =
      KelvinMetersPerWatt(bedResistivity.toKelvinMetersPerWatt * outerShare)
    (baseT4 - t4BedOuterPortion, t4Soil)
  }

  /** Calculates the resistance-matched outer radius of the soil model region
    * according to Diaz-Aguilo et al. (2014), Eq. (13).
    */
  def calcSoilModelOuterRadius(
      depth: Length,
      innerRadius: Length,
  ): Length = {
    val l = depth.toMeters
    val r = innerRadius.toMeters
    Meters(l + sqrt(pow(l, 2) - pow(r, 2)))
  }

  /** Calculates the thermal capacity of the outer soil region per unit cable
    * length.
    */
  def calcSoilThermalCapacity(
      specificHeatCapacity: VolumetricHeatCapacity,
      depth: Length,
      innerRadius: Length,
  ): VolumetricHeatCapacity = {
    val outerRadius = calcSoilModelOuterRadius(depth, innerRadius)
    val area =
      Pi * (pow(outerRadius.toMeters, 2) - pow(innerRadius.toMeters, 2))
    JoulesPerCubicMeterKelvin(
      area * specificHeatCapacity.toJoulesPerCubicMeterKelvin
    )
  }

  private[ampacity] final case class EnvironmentalCell(
      resistance: ThermalResistivity,
      thermalCapacity: ThermalCapacityPerLength,
  )

  private[ampacity] def calcCableBedEnvironmentalCells(
      bedResistance: ThermalResistivity,
      soilResistance: ThermalResistivity,
      cableOuterRadius: Length,
      bedRadius: Length,
      soilOuterRadius: Length,
      bedHeatCapacity: VolumetricHeatCapacity,
      soilHeatCapacity: VolumetricHeatCapacity,
  ): Seq[EnvironmentalCell] = {
    if cableOuterRadius >= bedRadius || bedRadius >= soilOuterRadius then {
      val message =
        s"Environmental radii must satisfy cable < bed < soil, but got $cableOuterRadius, $bedRadius, $soilOuterRadius."
      logger.error(message)
      throw new IllegalArgumentException(message)
    }
    if bedResistance <= KelvinMetersPerWatt(0d) ||
      soilResistance <= KelvinMetersPerWatt(0d)
    then {
      val message =
        s"Environmental thermal resistances must be positive, but got $bedResistance and $soilResistance."
      logger.error(message)
      throw new IllegalArgumentException(message)
    }

    def cellsInZone(
        count: Int,
        innerRadius: Length,
        outerRadius: Length,
        resistance: ThermalResistivity,
        heatCapacity: VolumetricHeatCapacity,
    ): Seq[EnvironmentalCell] = {
      val inner = innerRadius.toMeters
      val outer = outerRadius.toMeters
      (0 until count).map { index =>
        val cellInner = inner * pow(outer / inner, index.toDouble / count)
        val cellOuter = inner * pow(outer / inner, (index + 1).toDouble / count)
        val area = Pi * (pow(cellOuter, 2) - pow(cellInner, 2))
        EnvironmentalCell(
          resistance / count,
          JoulesPerMeterKelvin(
            area * heatCapacity.toJoulesPerCubicMeterKelvin
          ),
        )
      }
    }

    cellsInZone(
      2,
      cableOuterRadius,
      bedRadius,
      bedResistance,
      bedHeatCapacity,
    ) ++ cellsInZone(
      3,
      bedRadius,
      soilOuterRadius,
      soilResistance,
      soilHeatCapacity,
    )
  }

  /** Determines the loss weight factor sum(W_k / W_p) used for the cable bed T4
    * correction.
    */
  private[ampacity] def calcCableBedLossWeightFactor(
      layoutFormation: LayoutFormation,
      thermalTotalLossesLeadingCable: PowerPerLength,
      thermalTotalLossesMiddleCable: PowerPerLength,
      thermalTotalLossesLaggingCable: PowerPerLength,
  ): Double =
    layoutFormation match {
      case LayoutFormation.FlatDistance =>
        val wSum =
          thermalTotalLossesLeadingCable.toWattsPerMeter +
            thermalTotalLossesMiddleCable.toWattsPerMeter +
            thermalTotalLossesLaggingCable.toWattsPerMeter
        wSum / thermalTotalLossesMiddleCable.toWattsPerMeter
      case _ => 3.0
    }
}
