/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import breeze.linalg.{DenseMatrix, DenseVector}
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.model.grid.ampacity.LineSegmentThermalModel.LineState
import edu.ie3.simona.model.grid.ampacity.LineThermalModelNetworkSolver
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
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
import squants.space.Length
import squants.thermal.Celsius
import squants.time.{Frequency, Hertz}
import squants.{ElectricCurrent, Power, Temperature}

import scala.math.*

/** A collection of methods used for calculation of the
  * [[LineSegmentThermalModel]].
  */
object LineThermalModelCalculations extends LazyLogging {

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
    * @param electricalResistanceCable
    *   The dc resistance of the cable at 20°C as mentioned in the data sheet of
    *   the cable.
    * @param temperatureCorrectionFactor
    *   Temperature coefficient for resistance variation.
    * @param limitTemperature
    *   The maximum permissible temperature of the conductor.
    * @param factorSkinEffect
    *   Factor accounting for skin effect at operating frequency.
    * @param factorProximityEffect
    *   Factor accounting for proximity effect.
    * @return
    *   The AC resistance in Ohms per meter.
    */
  def calcAcResistance(
      electricalResistanceCable: ElectricalResistance,
      temperatureCorrectionFactor: Double,
      limitTemperature: Temperature,
      factorSkinEffect: Double,
      factorProximityEffect: Double,
  ): ElectricalResistance = {
    // normally in Ohms/Meter...
    Ohms(
      (1 + factorSkinEffect + factorProximityEffect) * electricalResistanceCable.toOhms *
        (1 + temperatureCorrectionFactor * (limitTemperature.toCelsiusScale - REFERENCE_TEMPERATURE))
    )
    // ALTERNATIVE: Calculate R0 by specific resistivity / conductorArea
    // (1 + factorSkinEffect + factorProximityEffect) * (resistivity.toOhmMeters / conductorArea.toSquareMeters) *
    // ALTERNATIVE: We are not using the limitTemperature because this overestimates the losses at lower operating temperatures. Therefore, we use the mean value between the limiting temperature and the current line temperature
    //        (1 + temperatureCorrectionFactor * ((limitTemperature.toCelsiusScale - operatingTemperature.toCelsiusScale)/2 - REFERENCE_TEMPERATURE))
  }

  /** Calculates the thermal losses of the cable segment per unit cable length.
    *
    * @param acResistance
    *   The ac resistance of the line segment per unit of cable length.
    * @param current
    *   The electric current of this line.
    * @return
    *   The thermal losses of this line segment in power per unit cable length.
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
    * @param phaseToGroundVoltage
    *   The phase-to-ground voltage U0 of the cable system
    * @param frequency
    *   The frequency of the system (50 Hz) in general.
    * @param tanDelta
    *   The dissipation factor of the cable.
    * @param dielectricCapacity
    *   The electric capacity that is formed by the dielectric of the cable.
    * @return
    *   The voltage dependent dielectric losses.
    */
  def calcDielectricLosses(
      dielectricMaterial: String,
      phaseToGroundVoltage: ElectricPotential,
      frequency: Frequency,
      tanDelta: Double,
      dielectricCapacity: Capacitance,
  ): Power = {
    (dielectricMaterial, phaseToGroundVoltage) match {
      case ("XLPE", voltage) if voltage < Kilovolts(127) => zeroKW
      case ("PVC", voltage) if voltage < Kilovolts(6)    => zeroKW
      case ("XLPE", voltage) if voltage >= Kilovolts(127) =>
        dielectricCapacity.calculateDielectricLosses(
          voltage,
          frequency,
          tanDelta,
        )
      case ("PVC", voltage) if voltage >= Kilovolts(6) =>
        dielectricCapacity.calculateDielectricLosses(
          voltage,
          frequency,
          tanDelta,
        )
      case _ =>
        throw new IllegalArgumentException(
          s"Unknown material used for dielectric: $dielectricMaterial."
        )
    }

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
    val soilThermCapacitance = JoulesPerMeterKelvin(10) // FIXME

    /* Calculate the losses */
    val conductorArea =
      cableSetup.conductorDiameter * cableSetup.conductorDiameter * PI_OVER_FOUR

    val specificTempCoefficient =
      cableSetup.conductor.material match {
        case CableMaterial.Copper    => TEMPERATURE_COEFFICIENT_COOPER
        case CableMaterial.Aluminium => TEMPERATURE_COEFFICIENT_ALUMINIUM
        case _ =>
          throw new IllegalArgumentException(
            s"Unknown conductor material: ${cableSetup.conductor.material}"
          )
      }

    val acResistance = calcAcResistance(
      cableSetup.electricResistance,
      specificTempCoefficient,
      cableSetup.limitTemperature,
      skinEffect,
      proximityEffect,
    )

    val conductorLosses = calcLossesConductor(acResistance, lineCurrent)

    val circulatingSheathLossFactor = 0.0435122656 // FIXME
    val eddyCurrentsSheathLossFactor = 0.0 // FIXME

    val sheathLosses = calcLossesSheath(
      circulatingSheathLossFactor,
      eddyCurrentsSheathLossFactor,
      conductorLosses,
    )

    val phaseToGroundVoltage = cableSetup.voltage / sqrt(3)

    val dielectricLosses = calcDielectricLosses(
      cableSetup.dielectric.material,
      phaseToGroundVoltage,
      Hertz(50),
      cableSetup.tanDelta,
      cableSetup.electricCapacitance,
    )

    val thermalTotalLossesCableA =
      conductorLosses + sheathLosses + dielectricLosses // FIXME: Same as CableB?
    val thermalTotalLossesCableB =
      conductorLosses + sheathLosses + dielectricLosses
    val thermalTotalLossesCableC =
      conductorLosses + sheathLosses + dielectricLosses // FIXME: Same as CableB?

    /* Update thermoelectric equivalent circuit */
    // No changes for T1-T3
    val t1 = currentLineModel.thermalResistanceT1
    val t2 = currentLineModel.thermalResistanceT2
    val t3 = currentLineModel.thermalResistanceT3

    // Sanity Check t2 should be zero, else some simplifications further down are not possible.
    if t2 != KelvinMetersPerWatt(0) then
      throw new IllegalStateException(
        s"Unexpected value for thermal-electric resistance T2, which should be zero."
      )

    // T4 changes since it depends on losses from neighbouring cables
    val t4 = cableSetup.layoutFormation match {
      case "flat-distance" =>
        calcThermalResistanceToSoilThreeSingleCoreFlatFormation(
          state.cableSetup.soilResistivity,
          cableSetup.depthCables,
          cableSetup.outerCover
            .map(_.outerDiameter)
            .getOrElse(
              throw new IllegalArgumentException(
                "Jack layer expected but not found for thermal resistance to soil calculation"
              )
            ),
          cableSetup.distanceCables,
          thermalTotalLossesCableA,
          thermalTotalLossesCableB,
          thermalTotalLossesCableC,
        )
      case "flat-touching" =>
        throw new IllegalArgumentException(
          s"Flat-touching layout formation is currently not supported"
        )
      case "trefoil-not-touching" => // FIXME Check for Trefoil
        throw new IllegalArgumentException(
          s"Trefoil not touching layout formation is currently not supported"
        )
      case "trefoil-touching" =>
        calcThermalResistanceToSoilThreeSingleCoreTrefoilTouching(
          state.cableSetup.soilResistivity,
          cableSetup.depthCables,
          cableSetup.outerCover
            .map(_.outerDiameter)
            .getOrElse(
              throw new IllegalArgumentException(
                "Jack layer expected but not found for thermal resistance to soil calculation"
              )
            ),
        )
      case _ =>
        throw new IllegalArgumentException(
          s"Unknown layout formation: ${cableSetup.layoutFormation}"
        )
    }

    /* Split Capacitance with van-Wormer Coefficient */
    val (
      dielectricThermCapacitanceC11,
      dielectricThermCapacitanceC12,
      dielectricThermCapacitanceC21,
      dielectricThermCapacitanceC22,
    ) =
      splitCapacitanceByVanWormerShortDuration(
        currentLineModel.thermalCapacityCd,
        cableSetup.conductor.outerDiameter,
        cableSetup.dielectric.outerDiameter,
      )

    val (
      jackThermCapacitanceC11,
      jackThermCapacitanceC12,
      jackThermCapacitanceC21,
      jackThermCapacitanceC22,
    ) =
      splitCapacitanceByVanWormerShortDuration(
        currentLineModel.thermalCapacityCj,
        cableSetup.screen
          .map(_.outerDiameter)
          .getOrElse(
            throw new IllegalArgumentException(
              "Screen layer expected but not found for van-Wormer capacitance splitting"
            )
          ),
        cableSetup.jack
          .map(_.outerDiameter)
          .getOrElse(
            throw new IllegalArgumentException(
              "Jack layer expected but not found for van-Wormer capacitance splitting"
            )
          ),
      )

    /* Build RC-Network */
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
      (currentLineModel.thermalCapacityCc + dielectricThermCapacitanceC11).toJoulesPerMeterKelvin // FIXME check conversion factor !
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
      (jackThermCapacitanceC22 + state.cableSetup.soilCapacitance).toJoulesPerMeterKelvin

    // Using the nodal potential method the 5 differential equation can be formulated and result in the system matrix
    val matrixA = DenseMatrix(
      (-g1 / c1, g1 / c1, 0.0, 0.0, 0.0),
      (g1 / c2, (-g1 - g2) / c2, g2 / c2, 0.0, 0.0),
      (0.0, g2 / c3, (-g2 - g3) / c3, g3 / c3, 0.0),
      (0.0, 0.0, g3 / c4, (-g3 - g4) / c4, g4 / c4),
      (0.0, 0.0, 0.0, g4 / c5, (-g4 - g5) / c5),
    )

    val vectorB = DenseVector(
      (conductorLosses.toWatts + dielectricLosses.toWatts / 2) / c1,
      0d,
      (sheathLosses.toWatts + dielectricLosses.toWatts / 2) / c3,
      0d,
      (g5 * state.groundTemperature.toCelsiusScale) / c5,
    )

    val (eigenvalues, eigenvectors) =
      LineThermalModelNetworkSolver.determineEigenvaluesAndVectors(matrixA)

    val vStart = DenseVector(
      state.lineTemperatures.currentLineTemp1.toCelsiusScale,
      state.lineTemperatures.currentLineTemp2.toCelsiusScale,
      state.lineTemperatures.currentLineTemp3.toCelsiusScale,
      state.lineTemperatures.currentLineTemp4.toCelsiusScale,
      state.lineTemperatures.currentLineTemp5.toCelsiusScale, // FIXME: Check if this is always ambientTemp, then it can be removed.
    )

    val vp = matrixA \ (-vectorB)

    val c = eigenvectors \ (vStart - vp)

    def getNodeTemperature(t: Long, node: Int): Temperature = {
      var temperature = vp(node) // Start with the steady-state value of Node

      for i <- 0 until 5 do {
        // Add the transient responses of the 5 e-functions
        // c(i) is the coefficient, v_eigen(0, i) is the Node 1 component of the i-th eigenvector
        temperature += c(i) * eigenvectors(node, i) * math.exp(
          eigenvalues(i) * t
        )
      }

      voltage
    }

    val duration = state.tick - state.lastTick
    if duration > 3600 then
      logger.warn(
        s"RC-Network for short durations has been used. However, the duration of $duration ticks might be of type long duration. Currently used method might be inaccurate but should estimate on the safe side."
      )

    Celsius(getV1(duration, 0))
  }
}
