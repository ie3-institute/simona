/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.util.scala.quantities.SquantsUtils.RichCapacitance
import edu.ie3.util.scala.quantities.{
  JoulesPerMeterKelvin,
  KelvinMetersPerWatt,
  ThermalCapacitance,
  ThermalResistivity,
  SquantsUtils as RichElectricPotential,
}
import squants.electro.*
import squants.energy.Watts
import squants.space.{Area, Length}
import squants.thermal.ThermalCapacity
import squants.time.Frequency
import squants.{ElectricCurrent, Power, Temperature}

import scala.math.*

/** A collection of methods used for calculation of the
  * [[LineSegmentThermalModel]].
  */
object LineThermalModelCalculations {

  // Constants for thermal calculations
  private val TWO_PI: Double = 2 * Pi
  private val PI_OVER_FOUR: Double = Pi / 4
  private val TREFOIL_COEFFICIENT: Double = 1.5
  private val TREFOIL_ADJUSTMENT: Double = 0.63
  private val REFERENCE_TEMPERATURE: Double = 20

  /** Calculates the AC resistance of a conductor accounting for temperature and
    * high-frequency effects (skin effect, proximity effect).
    *
    * @param resistivity
    *   The resistivity of the conductor material
    * @param conductorArea
    *   The cross-sectional area of the conductor
    * @param temperatureCorrectionFactor
    *   Temperature coefficient for resistance variation
    * @param operatingTemperature
    *   The operating temperature of the conductor
    * @param factorSkinEffect
    *   Factor accounting for skin effect at operating frequency
    * @param factorProximityEffect
    *   Factor accounting for proximity effect
    * @return
    *   The AC resistance in Ohms per meter
    */
  def calcAcResistance(
      resistivity: Resistivity,
      conductorArea: Area,
      temperatureCorrectionFactor: Double,
      operatingTemperature: Temperature,
      factorSkinEffect: Double,
      factorProximityEffect: Double,
  ): ElectricalResistance = {
    // normally in Ohms/Meter...
    Ohms(
      (1 + factorSkinEffect + factorProximityEffect) * (resistivity.toOhmMeters / conductorArea.toSquareMeters) *
        (1 + temperatureCorrectionFactor * (operatingTemperature.toCelsiusScale - REFERENCE_TEMPERATURE))
    )
  }

  /** Calculates the thermal losses of the cable segment per unit cable length.
    * @param specificThermalResistivity
    *
    * @return
    */
  def calcThermalLosses(
  ): Power = {
    ???
    // Stromwärmeverluste
    // + Mantel- und Schirmverluste
    // + Wirbelstrom-, Längsstrom- und Magnetisierungsverluste
    // + dielektrische Verluste
    // + ohmsche Verluste (stromunabhängig)
  }

  /** Calculates the thermal losses of the cable segment per unit cable length.
    *
    * @param specificThermalResistivity
    * @return
    */
  def calcLossesConductor(
      acResistance: ElectricalResistance,
      current: ElectricCurrent,
  ): Power = {
    Watts(current.toAmperes * current.toAmperes * acResistance.toOhms)
  }

  /** Calculates the losses within the cable sheath. Zero / Not applicable if
    * cable has no sheath.
    *
    * @param circulatingSheathLossFactor
    *   Determines the losses in the sheath caused by circulating currents.
    *   Often given as lambda_1_dash.
    * @param eddyCurrentsSheathLossFactor
    *   Determines the losses in the sheath caused by eddy currents. Often given
    *   as lambda_1_dash_dash.
    * @return
    */
  def calcLossesSheath(
      circulatingSheathLossFactor: Double,
      eddyCurrentsSheathLossFactor: Double,
      conductorLosses: Power,
  ): Power = {
    // lambda_1 = lambda_1_dash + lambda_1_dash_dash
    val lambdaOne = circulatingSheathLossFactor + eddyCurrentsSheathLossFactor
    conductorLosses * lambdaOne
  }

  /** Calculates the losses within the cable armor. Zero / Not applicable if
    * cable has no armor.
    *
    * @param circulatingArmorLossFactor
    *   Determines the losses in the armor caused by circulating currents. Often
    *   given as lambda_2_dash.
    * @param eddyCurrentsArmorLossFactor
    *   Determines the losses in the armor caused by eddy currents. Often given
    *   as lambda_2_dash_dash.
    * @return
    */
  def calcLossesArmor(
      circulatingArmorLossFactor: Double,
      eddyCurrentsArmorLossFactor: Double,
      conductorLosses: Power,
  ): Power = {
    // lambda_2 = lambda_2_dash + lambda_2_dash_dash
    val lambdaTwo = circulatingArmorLossFactor + eddyCurrentsArmorLossFactor
    conductorLosses * lambdaTwo
  }

  /** Calculates the losses within the cable that are not current-dependent.
    *
    * @param voltage
    * @param frequency
    *   the frequency of the system (50 Hz) in general.
    * @param tanDelta
    * @param dielectricCapacity
    * @return
    */
  def calcDielectricLosses(
      voltage: ElectricPotential,
      frequency: Frequency,
      tanDelta: Double,
      dielectricCapacity: Capacitance,
  ): Power = {
    dielectricCapacity.calculateDielectricLosses(voltage, frequency, tanDelta)
  }

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
    (specificThermalResistivity / TWO_PI) * log(
      outerRadius.toMeters / innerRadius.toMeters
    )
  }

  /** Calculates the thermal resistivity between the cable layers and the
    * surrounding soil for a single cable (e.g. Three-Core-Cable).
    *
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
  ): ThermalResistivity = {
    (specificThermalResistivityGround / TWO_PI) * calcGeometricFactor(
      depthCable,
      cableDiameter,
    )
  }

  /** Calculates the geometrical factor of the single core cable to its mirrored
    * (Kennelly method).
    *
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
    val normalizationFactor = 2d * depthCable.toMeters / cableDiameter.toMeters
    log(normalizationFactor + sqrt(pow(normalizationFactor, 2) - 1))
  }

  /** Calculates the thermal resistivity of the middle single core cable
    * (hottest cable) for a flat formation of three single core cables. Cable
    * formation: (A) --- (B) --- (C). Reference Anders Rating of electric power
    * cables: ampacity computations for transmission, distribution, and
    * industrial applications p. 215
    *
    * @param specificThermalResistivityGround
    *   The material dependent specific thermal resistance of the surrounding
    *   soil.
    * @param depthCables
    *   The laying depth of the cables.
    * @param diameterCableB
    *   The outer diameter of the cable B.
    * @param distanceOfCables
    *   The distance between the cables. Assuming same distance between A, B and
    *   C. Distance is from the cables centers.
    * @param diameterCableA
    *   The losses of the cable A.
    * @param diameterCableB
    *   The losses of the cable B.
    * @param diameterCableC
    *   The losses of the cable C.
    * @return
    *   The thermal resistance between cable and its surrounding soil in Kelvin
    *   * Meter / Watt per unit cable length.
    */
  def calcThermalResistanceToSoilThreeSingleCoreFlatFormation(
      specificThermalResistivityGround: ThermalResistivity,
      depthCables: Length,
      diameterCableB: Length,
      distanceOfCables: Length,
      lossesCableA: Power,
      lossesCableB: Power,
      lossesCableC: Power,
  ): ThermalResistivity = {
    val distancePtoKDash = sqrt(
      pow(distanceOfCables.toMeters, 2) + pow(2 * depthCables.toMeters, 2)
    )
    val thermalInfluenceCableAonB =
      (lossesCableA / lossesCableB) * log(
        distancePtoKDash / distanceOfCables.toMeters
      )
    val thermalInfluenceCableConB =
      (lossesCableC / lossesCableB) * log(
        distancePtoKDash / distanceOfCables.toMeters
      )

    val thermalResistanceShareOfCableB =
      calcGeometricFactor(depthCables, diameterCableB)

    specificThermalResistivityGround / TWO_PI * (thermalResistanceShareOfCableB + thermalInfluenceCableAonB + thermalInfluenceCableConB)
  }

  /** Calculates the thermal resistivity of the top single core cable (hottest
    * cable) for a trefoil touching formation. Reference: Anders Rating of
    * electric power cables: ampacity computations for transmission,
    * distribution, and industrial applications p. 220
    *
    * @param specificThermalResistivityGround
    *   The material dependent specific thermal resistance of the surrounding
    *   soil.
    * @param depthToCenter
    *   The laying depth of the cables measured to the center of the trefoil
    *   group
    * @param diameterCable
    *   The outer diameter of one of the single cables.
    * @return
    *   The thermal resistance between cable and its surrounding soil in Kelvin
    *   * Meter / Watt per unit cable length.
    */
  def calcThermalResistanceToSoilThreeSingleCoreTrefoilTouching(
      specificThermalResistivityGround: ThermalResistivity,
      depthToCenter: Length,
      diameterCable: Length,
  ): ThermalResistivity = {
    val u = 2 * depthToCenter.toMeters / diameterCable.toMeters

    KelvinMetersPerWatt(
      TREFOIL_COEFFICIENT * specificThermalResistivityGround.toKelvinMetersPerWatt / Pi
    ) * (log(2 * u) - TREFOIL_ADJUSTMENT)
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
    val areaDifference = pow(outerDiameter.toMeters, 2) - pow(
      innerDiameter.toMeters,
      2,
    )
    JoulesPerMeterKelvin(
      PI_OVER_FOUR * areaDifference * specificThermalCapacity.toJoulesPerKelvin
    )
  }

  /** Determines the Van-Wormer-Coefficient for long-duration transients.
    *
    * @param diameterDielectric
    *   The inner diameter of this layer.
    * @param diameterConductor
    *   The outer diameter of this layer.
    * @return
    *   Van-Wormer coefficient as Double.
    */
  def vanWormerCoefficientLongDurationTransients(
      diameterDielectric: Length,
      diameterConductor: Length,
  ): Double = {
    val diameterRatio = diameterDielectric / diameterConductor
    (1 / (2 * log(diameterRatio))) - (1 / (pow(diameterRatio, 2) - 1))
  }

  /** Determines the Van-Wormer-Coefficient for short-duration transients.
    *
    * @param innerDiameter
    *   The inner diameter of this layer.
    * @param outerDiameter
    *   The outer diameter of this layer.
    * @return
    *   Van-Wormer coefficient as Double.
    */
  def vanWormerCoefficientShortTermDurationTransients(
      innerDiameter: Length,
      outerDiameter: Length,
  ): Double = {
    val diameterRatio = outerDiameter / innerDiameter
    (1 / (log(diameterRatio))) - (1 / (diameterRatio - 1))
  }
}
