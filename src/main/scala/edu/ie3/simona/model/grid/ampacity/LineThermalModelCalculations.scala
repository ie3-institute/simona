/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import breeze.linalg.{DenseMatrix, DenseVector}
import edu.ie3.simona.model.grid.ampacity.LineSegmentThermalModel.LineState
import edu.ie3.simona.model.grid.ampacity.LineThermalModelNetworkSolver
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
import squants.thermal.Celsius
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
  private val AC_RESISTIVITY_COOPER: Resistivity = OhmMeters(1.7241e-8)
  private val TEMPERATURE_COEFFICIENT_COOPER: Double = 3.93e-3
  private val AC_RESISTIVITY_ALUMINIUM: Resistivity = OhmMeters(2.8264e-8)
  private val TEMPERATURE_COEFFICIENT_ALUMINIUM: Double = 4.03e-3

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
      specificThermalCapacity: ThermalCapacitance,
      innerDiameter: Length,
      outerDiameter: Length,
  ): ThermalCapacitance = {
    val areaDifference = pow(outerDiameter.toMeters, 2) - pow(
      innerDiameter.toMeters,
      2,
    )
    JoulesPerMeterKelvin(
      PI_OVER_FOUR * areaDifference * specificThermalCapacity.toJoulesPerMeterKelvin
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
    (1 / log(diameterRatio)) - (1 / (diameterRatio - 1))
  }

  /** Splits a given capacitance in case of short-term durations by the method
    * of van-Wormer through its coefficient.
    *
    * @param capacitance
    *   The capacitance that should be split.
    * @param innerDiameter
    *   The inner diameter of the shell that creates the capacitance.
    * @param outerDiameter
    *   The outer diameter of the shell that creates the capacitance.
    * @return
    *   Returns the four parts C11,C12,C21 and C22 of the input capacitance.
    */

  private def splitCapacitanceByVanWormerShortDuration(
      capacitance: ThermalCapacitance,
      innerDiameter: Length,
      outerDiameter: Length,
  ) = {

    val diameterMid = innerDiameter + (outerDiameter - innerDiameter) / 2
    val vanWormerJackFirstHalf =
      vanWormerCoefficientShortTermDurationTransients(
        innerDiameter,
        diameterMid,
      )
    val capacitanceC11 = capacitance * vanWormerJackFirstHalf
    val capacitanceC12 = capacitance * (1 - vanWormerJackFirstHalf)
    val vanWormerJackSecondHalf =
      vanWormerCoefficientShortTermDurationTransients(
        diameterMid,
        outerDiameter,
      )
    val capacitanceC21 = capacitance * vanWormerJackSecondHalf
    val capacitanceC22 = capacitance * (1 - vanWormerJackSecondHalf)
    (capacitanceC11, capacitanceC12, capacitanceC21, capacitanceC22)
  }

  def createAndCalcRCNetworkMvCableShortDuration(
      state: LineState,
      cableSetup: CableSetup,
      lineCurrent: ElectricCurrent,
  ): Temperature = {
    val currentLineModel = state.currentLineSegmentThermalModel
    val thermalResistivitySoil = KelvinMetersPerWatt(2.9)

    val conductorArea =
      cableSetup.conductorDiameter * cableSetup.conductorDiameter * PI_OVER_FOUR
    val proximityEffect = 0.01 // Check CIGRE for detailed method //FIXME
    val skinEffect = 0.01 // FIXME

    val (specificAcResistance, specificTempCoefficient) =
      cableSetup.conductorMaterial match {
        case "Cooper" => (AC_RESISTIVITY_COOPER, TEMPERATURE_COEFFICIENT_COOPER)
        case "Aluminium" =>
          (AC_RESISTIVITY_ALUMINIUM, TEMPERATURE_COEFFICIENT_ALUMINIUM)
        case _ =>
          throw new IllegalArgumentException(
            s"Unknown conductor material: ${cableSetup.conductorMaterial}"
          )
      }

    val acResistance = calcAcResistance(
      specificAcResistance,
      conductorArea,
      specificTempCoefficient,
      state.currentLineTemp1,
      skinEffect,
      proximityEffect,
    )

    val conductorLosses = calcLossesConductor(acResistance, lineCurrent)

    val circulatingSheathLossFactor = 0.01 // FIXME
    val eddyCurrentsSheathLossFactor = 0.01 // FIXME

    val sheathLosses = calcLossesSheath(
      circulatingSheathLossFactor,
      eddyCurrentsSheathLossFactor,
      conductorLosses,
    )

    val thermalTotalLossesCableA =
      conductorLosses + sheathLosses // FIXME: Same as CableB?
    val thermalTotalLossesCableB = conductorLosses + sheathLosses
    val thermalTotalLossesCableC =
      conductorLosses + sheathLosses // FIXME: Same as CableB?

    // No changes for T1-T3
    val t1 = currentLineModel.thermalResistanceT1
    val t2 = currentLineModel.thermalResistanceT2
    val t3 = currentLineModel.thermalResistanceT3

    // FIXME Check for Trefoil
    // T4 changes since it depends on losses from neighbouring cables
    val t4 = calcThermalResistanceToSoilThreeSingleCoreFlatFormation(
      thermalResistivitySoil,
      cableSetup.depthCables,
      cableSetup.jackDiameter,
      cableSetup.distanceCables,
      thermalTotalLossesCableA,
      thermalTotalLossesCableB,
      thermalTotalLossesCableC,
    )

    val conductorThermCapacitanceCc = currentLineModel.thermalCapacityCc
    val dielectricThermCapacitanceCd = currentLineModel.thermalCapacityCd

    val (
      dielectricThermCapacitanceC11,
      dielectricThermCapacitanceC12,
      dielectricThermCapacitanceC21,
      dielectricThermCapacitanceC22,
    ) =
      splitCapacitanceByVanWormerShortDuration(
        dielectricThermCapacitanceCd,
        cableSetup.conductorDiameter,
        cableSetup.dielectricDiameter,
      )

    val (
      jackThermCapacitanceC11,
      jackThermCapacitanceC12,
      jackThermCapacitanceC21,
      jackThermCapacitanceC22,
    ) =
      splitCapacitanceByVanWormerShortDuration(
        currentLineModel.thermalCapacityCj,
        cableSetup.sheathDiameter,
        cableSetup.jackDiameter,
      )

    val soilThermCapacitance = JoulesPerMeterKelvin(10) // FIXME
    val ambientSoilTemp = state.groundTemperature

    // in case of short durations we have an RC-Network with seven loops. There are 5 resistors (dielectric and jack needs to be split) and 5 capacitors.
    // However, it simplifies if we transform them to conductance
    val g1 = 2d / t1.toKelvinMetersPerWatt // FIXME, Squants should convert this
    val g2 = 2d / t1.toKelvinMetersPerWatt
    val g3 = 2d / t3.toKelvinMetersPerWatt
    val g4 = 2d / t3.toKelvinMetersPerWatt
    val g5 = 1d / t4.toKelvinMetersPerWatt

    // the RC-Network can be simplified since all parallel capacitance can be merged
    // Conductor capacitance and first part of the first half of the dielectric
    val c1 =
      (conductorThermCapacitanceCc + dielectricThermCapacitanceC11).toJoulesPerMeterKelvin // FIXME check conversion factor !
        // Capacitance of the second part of first half of the dielectric + first part of the second half of the dielectric
    val c2 =
      (dielectricThermCapacitanceC12 + dielectricThermCapacitanceC21).toJoulesPerMeterKelvin
    // Capacitance of the second part of second half of the dielectric + the sheath + the first part of the first half of the jack
    val c3 =
      (dielectricThermCapacitanceC22 + currentLineModel.thermalCapacityCs + jackThermCapacitanceC11).toJoulesPerMeterKelvin
    // Capacitance of the second part of first half of the jack + first part of the second half of the jack
    val c4 =
      (jackThermCapacitanceC12 + jackThermCapacitanceC21).toJoulesPerMeterKelvin
    // Capacitance of the second part of second half of the jack + the capacitance of the soil
    val c5 =
      (jackThermCapacitanceC22 + soilThermCapacitance).toJoulesPerMeterKelvin

    // Using the nodal potential method the 5 differential equation can be formulated and result in the system matrix
    val matrixA = DenseMatrix(
      (-g1 / c1, g1 / c1, 0.0, 0.0, 0.0),
      (g1 / c2, (-g1 - g2) / c2, g2 / c2, 0.0, 0.0),
      (0.0, g2 / c3, (-g2 - g3) / c3, g3 / c3, 0.0),
      (0.0, 0.0, g3 / c4, (-g3 - g4) / c4, g4 / c4),
      (0.0, 0.0, 0.0, g4 / c5, (-g4 - g5) / c5),
    )

    val vectorB = DenseVector(
      conductorLosses.toWatts / c1,
      0d,
      sheathLosses.toWatts / c3,
      0d,
      (g5 * ambientSoilTemp.toCelsiusScale) / c5,
    )

    val (eigenvalues, eigenvectors) =
      LineThermalModelNetworkSolver.determineEigenvaluesAndVectors(matrixA)

    // sanity check
    if eigenvalues.length != 5 ||
      eigenvectors.rows != eigenvalues.length ||
      eigenvectors.cols != eigenvalues.length
    then
      throw new IllegalStateException(
        s"Unexpected number of Eigenvalues or Eigenvectors. Expected are 5 each, Got: Eigenvalues: $eigenvalues, Eigenvectors: $eigenvectors."
      )

    val vStart = DenseVector(
      state.currentLineTemp1.toCelsiusScale,
      state.currentLineTemp2.toCelsiusScale,
      state.currentLineTemp3.toCelsiusScale,
      state.currentLineTemp4.toCelsiusScale,
      state.currentLineTemp5.toCelsiusScale,
    )

    val vp = matrixA \ (-vectorB)

    val c = eigenvectors \ (vStart - vp)

    def getV1(t: Long, node: Int): Double = {
      var voltage = vp(node) // Start with the steady-state value of Node

      for i <- 0 until 5 do {
        // Add the transient responses of the 5 e-functions
        // c(i) is the coefficient, v_eigen(0, i) is the Node 1 component of the i-th eigenvector
        voltage += c(i) * eigenvectors(node, i) * math.exp(eigenvalues(i) * t)
      }

      voltage
    }

    val duration = state.tick - state.lastTick

    Celsius(getV1(duration, 0))
  }
}
