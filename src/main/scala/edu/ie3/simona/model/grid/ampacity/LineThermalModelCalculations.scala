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
  ElectricalResistancePerLength,
  JoulesPerMeterKelvin,
  KelvinMetersPerWatt,
  OhmsPerMeter,
  ThermalCapacitance,
  ThermalResistivity,
  SquantsUtils as RichElectricPotential,
}
import squants.electro.*
import squants.energy.Watts
import squants.space.{Length, Millimeters, SquareMeters}
import squants.thermal.Celsius
import squants.time.{Frequency, Hertz}
import squants.{ElectricCurrent, Kelvin, Meters, Power, Temperature}

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
  private val REFERENCE_TEMPERATURE: Temperature = Celsius(20d)
  private val AC_RESISTIVITY_COOPER: Resistivity = OhmMeters(1.7241e-8)
  private val TEMPERATURE_COEFFICIENT_COOPER: Double = 3.93e-3
  private val AC_RESISTIVITY_ALUMINIUM: Resistivity = OhmMeters(2.8264e-8)
  private val TEMPERATURE_COEFFICIENT_ALUMINIUM: Double = 4.03e-3

  /** Calculates the thermal resistance T1 of the inner cable elements between
    * conductor and screen.
    *
    * @param cableSetup
    *   The setup of the cable in this line segment.
    * @return
    *   The thermal resistance per unit of length.
    */
  def calcThermalResistanceT1(
      cableSetup: CableSetup,
      voltage: ElectricPotential,
  ): ThermalResistivity = {
    val baseResistance =
      cableSetup.layersIsolationElements.foldLeft(KelvinMetersPerWatt(0)) {
        (acc, layer) =>
          acc + calcThermalResistanceCableShells(
            layer.thermalResistivity,
            layer.innerDiameter,
            layer.outerDiameter,
          )
      }

    val degreeOfCover = calcDegreeOfScreenCover(cableSetup.screenLayer)

    val screenCorrection = {
      // IEC 60287 requires to add 7 % to T1 in case the degree of cover of screen is below 50 % and the Voltage below 35 kV
      if degreeOfCover < 0.5 && voltage < Kilovolts(35d) then {
        baseResistance * 0.07
      } else {
        KelvinMetersPerWatt(0)
      }
    }

    baseResistance + screenCorrection
  }

  /** Calculates the thermal resistance T3 of the outer cable covering.
    *
    * @param cableSetup
    *   The setup of the cable in this line segment.
    * @return
    *   The thermal resistance per unit of length.
    */
  def calcThermalResistanceT3(
      cableSetup: CableSetup
  ): ThermalResistivity = {
    val baseResistance =
      cableSetup.layersJackElements.foldLeft(KelvinMetersPerWatt(0)) {
        (acc, layer) =>
          acc + calcThermalResistanceCableShells(
            layer.thermalResistivity,
            layer.innerDiameter,
            layer.outerDiameter,
          )
      }

    val degreeOfCover = calcDegreeOfScreenCover(cableSetup.screenLayer)

    val screenCorrection = {
      // IEC 60287 requires to add 60 % to T3 in case of trefoil-touching formation and the degree of cover of screen is below 50 %
      if cableSetup.layoutFormation == "trefoil-touching" && degreeOfCover < 0.5
      then {
        baseResistance * 0.6
      } else {
        KelvinMetersPerWatt(0)
      }
    }

    baseResistance + screenCorrection
  }

  /** Calculates the thermal resistance T1 of the inner cable elements between
    * conductor and screen.
    *
    * @param screenLayer
    *   FIXME
    * @return
    *   FIXME
    */
  def calcDegreeOfScreenCover(
      screenLayer: Option[ScreenLayer]
  ): Double = {
    val layFactor = calcLayFactor(screenLayer)
    screenLayer match {
      case Some(layer) =>
        (layer.wireDiameter * layer.wiresNumber * layFactor) / ((layer.innerDiameter + layer.wireDiameter) * Pi)
      case None => 1.0
    }
  }

  /** Calculates the thermal resistance T1 of the inner cable elements between
    * conductor and screen.
    *
    * @param screenLayer
    *   FIXME
    * @return
    *   FIXME
    */
  def calcLayFactor(
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

  /** Calculates the AC resistance per unit of length of a conductor accounting
    * for temperature and high-frequency effects (skin effect, proximity
    * effect).
    *
    * @param electricalResistanceCable
    *   The dc resistance of the cable at 20°C per unit of length as mentioned
    *   in the data sheet of the cable.
    * @param temperatureCorrectionFactor
    *   Temperature coefficient for resistance variation.
    * @param cableSetup
    *   The setup of the cable in this line segment.
    * @return
    *   The AC resistance per unit of length.
    */
  def calcAcResistance(
      electricalResistanceCable: ElectricalResistancePerLength,
      temperatureCorrectionFactor: Double,
      cableSetup: CableSetup,
  ): ElectricalResistancePerLength = {
    // ALTERNATIVE 1: Calculate R0 by specific resistivity / conductorArea
    // ALTERNATIVE 2: We are not using the limitTemperature because this overestimates the losses at lower operating temperatures. Therefore, we use the mean value between the limiting temperature and the current line temperature
    // (1 + temperatureCorrectionFactor * ((limitTemperature.toCelsiusScale - operatingTemperature.toCelsiusScale)/2 - REFERENCE_TEMPERATURE.toCelsiusScale))
    val rDc =
      electricalResistanceCable * (1 + temperatureCorrectionFactor * (cableSetup.limitTemperature.toCelsiusScale - REFERENCE_TEMPERATURE.toCelsiusScale))

    val factorSkinEffect = calculateSkinEffectFactor(
      rDc,
      cableSetup.frequency,
      cableSetup.skinEffectCoefficient,
    )
    val factorProximityEffect = calculateProximityEffectFactor(
      rDc,
      cableSetup.frequency,
      cableSetup.proximityEffectCoefficient,
      cableSetup.conductor.outerDiameter,
      cableSetup.distanceCables,
    )

    rDc * (1 + factorSkinEffect + factorProximityEffect)
  }

  /** Calculates the AC resistance per unit of length of the sheath accounting
    * for temperature and high-frequency effects (skin effect, proximity
    * effect).
    * @param resistivity
    *   The resistivity of the conductor material.
    * @param conductorArea
    *   The cross-sectional area of the conductor.
    * @param temperatureCorrectionFactor
    *   Temperature coefficient for resistance variation.
    * @param operatingTemperature
    *   The operating temperature of the conductor.
    * @param factorSkinEffect
    *   Factor accounting for skin effect at operating frequency.
    * @param factorProximityEffect
    *   Factor accounting for proximity effect.
    * @return
    *   The AC resistance per unit of length.
    */
  def calcAcResistanceSheath(
      resistivity: Resistivity,
      wiresNumber: Int,
      wireDiameter: Length,
      lengthOfLaySheath: Option[Length],
      diameterUnderTheScreen: Length,
      temperatureCorrectionFactor: Double,
      limitTemperature: Temperature,
      thermalResistanceT1: ThermalResistivity,
      conductorLosses: Power,
      dielectricLosses: Power,
  ): ElectricalResistancePerLength = {
    // Calculation of cross-sectional area of the sheath
    val area = SquareMeters(
      wiresNumber * PI_OVER_FOUR * wireDiameter.toMeters * wireDiameter.toMeters
    )

    // Consider lay factor
    val layFactor = sqrt(
      1 + (pow(
        Pi * (diameterUnderTheScreen.toMeters + wireDiameter.toMeters),
        2,
      ) / pow(lengthOfLaySheath.getOrElse(Meters(999d)).toMeters, 2))
    )

    // ac resistance per unit length of screen at 20°C
    val r0Screen = OhmsPerMeter(
      layFactor * resistivity.toOhmMeters / area.toSquareMeters
    )

    // calculate operating temperatur of the screen
    val screenTemp =
      limitTemperature // FIXME this is only limitTemperate if the conductor is at limitTemp
        - Kelvin(
          (conductorLosses.toWatts + 0.5 * dielectricLosses.toWatts) * thermalResistanceT1.toKelvinMetersPerWatt
        )

    // ac resistance of screen wires at operating temp
    r0Screen * (1 + temperatureCorrectionFactor * (screenTemp.toCelsiusScale - REFERENCE_TEMPERATURE.toCelsiusScale))
  }

  /** Calculates the skin effect factor (y_s) according to IEC 60287.
    *
    * @param rDc
    *   DC resistance of the conductor at operating temperature
    * @param frequency
    *   System frequency
    * @param ks
    *   Skin effect coefficient (e.g., 1.0 for solid/stranded copper or
    *   aluminum)
    * @return
    *   Skin effect factor (y_s)
    */
  def calculateSkinEffectFactor(
      rDc: ElectricalResistancePerLength,
      frequency: Frequency,
      ks: Double,
  ): Double = {
    val xsSquared =
      (8 * Pi * frequency.toHertz / rDc.toOhmsPerMeter) * 1e-7 * ks
    val xsFourth = pow(xsSquared, 2)
    val xs = sqrt(xsSquared)

    // Apply the piecewise formulas based on the value of x_s
    xs match {
      case x if x <= 2.8 => xsFourth / (192.0 + 0.8 * xsFourth)
      case x if x <= 3.8 => -0.136 - (0.0177 * xs) + (0.0563 * xsSquared)
      case _             => (0.354 * xs) - 0.733
    }
  }

  /** Calculates the proximity effect factor (y_p) for a three-core cable or
    * three single-core cables in a trefoil arrangement (IEC 60287).
    *
    * @param rDc
    *   DC resistance of the conductor at operating temperature
    * @param frequency
    *   System frequency
    * @param kp
    *   Proximity effect coefficient
    * @param conductorDiameter
    *   External diameter of the conductor
    * @param s
    *   Distance between conductor axes (spacing)
    * @return
    *   Proximity effect factor (y_p)
    */
  def calculateProximityEffectFactor(
      rDc: ElectricalResistancePerLength,
      frequency: Frequency,
      kp: Double,
      conductorDiameter: Length,
      s: Length,
  ): Double = {
    val xpSquared =
      (8 * Pi * frequency.toHertz / rDc.toOhmsPerMeter) * 1e-7 * kp
    val xpFourth = pow(xpSquared, 2)

    val ratioDcToS = conductorDiameter / s
    val ratioSquared = pow(ratioDcToS, 2)

    val baseFactor = xpFourth / (192.0 + 0.8 * xpFourth)

    val bracketTerm = (0.312 * ratioSquared) + (1.18 / (baseFactor + 0.27))
    baseFactor * ratioSquared * bracketTerm
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
      acResistance: ElectricalResistancePerLength,
      current: ElectricCurrent,
  ): Power = {
    Watts(current.toAmperes * current.toAmperes * acResistance.toOhmsPerMeter)
  }

  /** Calculates the losses within the cable sheath. Zero / Not applicable if
    * cable has no sheath.
    * @layoutFormation
    *   Formation in which the cables are laid.
    * @param r
    *   AC-Resistance per Length of conductor at operating temperature.
    * @param rs
    *   AC-Resistance per Length of sheath at operating temperature.
    * @param axialCableDistance
    *   Axial distance of the cables (Center to Center).
    * @param averageDiameterSheath
    *   Average diameter of the sheath.
    * @phase
    *   String that indicates whether the phase is the leading, the middle, or
    *   the lagging phase within the three-phase-system.
    * @param eddyCurrentsSheathLossFactor
    *   Determines the losses in the sheath caused by eddy currents. Often given
    *   as lambda_1''.
    * @param conductorLosses
    *   The thermal losses of the conductor.
    * @return
    */
  def calcLossesSheath(
      layoutFormation: String,
      r: ElectricalResistancePerLength,
      rs: ElectricalResistancePerLength,
      axialCableDistance: Length,
      averageDiameterSheath: Length,
      phase: String,
      eddyCurrentsSheathLossFactor: Double,
      conductorLosses: Power,
  ): Power = {
    val (x, xm) =
      calculateReactance(Hertz(50), axialCableDistance, averageDiameterSheath)
    val lambda1Dash = layoutFormation match {
      case "flat-distance" => calcLambda1DashFlatDistance(r, rs, x, xm, phase)
      case "flat-touching" =>
        throw new IllegalArgumentException(
          s"Flat-touching layout formation is currently not supported"
        )
      case "trefoil-not-touching" =>
        throw new IllegalArgumentException(
          s"Trefoil not touching layout formation is currently not supported"
        )
      case "trefoil-touching" => calcLambda1DashTrefoilTouching(r, rs, x)
      case _ =>
        throw new IllegalArgumentException(
          s"Unknown layout formation: $layoutFormation"
        )
    }
    // lambda_1 = lambda_1_dash + lambda_1_dash_dash
    val lambdaOne = lambda1Dash + eddyCurrentsSheathLossFactor

    conductorLosses * lambdaOne
  }

  /** Calculates the sheath loss factor for circulating currents in case of
    * systems in flat formation and bonded on both ends.
    * @param r
    *   AC-Resistance per Length of conductor at operating temperature.
    * @param rs
    *   AC-Resistance per Length of sheath at operating temperature.
    * @param x
    *   Reactance per Length of the sheath.
    * @param xm
    *   Mutual Reactance per Length.
    * @return
    *   Loss factor lambda1'.
    */
  def calcLambda1DashFlatDistance(
      r: ElectricalResistancePerLength,
      rs: ElectricalResistancePerLength,
      x: ElectricalResistancePerLength,
      xm: ElectricalResistancePerLength,
      phase: String,
  ): Double = {
    // 	Auxiliary quantity by IEC 60287
    val p = x.toOhmsPerMeter + xm.toOhmsPerMeter
    val q = x.toOhmsPerMeter - (xm.toOhmsPerMeter / 3.0)

    val rsSq = rs.toOhmsPerMeter * rs.toOhmsPerMeter
    val pSq = p * p
    val qSq = q * q

    // Split into three parts
    val term1 = (0.25 * qSq) / (rsSq + qSq)
    val term2 = (0.75 * pSq) / (rsSq + pSq)

    // Asymmetric part (adds up for leading phase, subtracts for lagging phase)
    // val term3 = (sqrt(3.0) * rs * p * q * xm) / ((rsSq + pSq) * (rsSq + qSq))
    val term3 =
      (2 * rs.toOhmsPerMeter * p * q * xm.toOhmsPerMeter) / (sqrt(
        3.0
      ) * (rsSq + pSq) * (rsSq + qSq))

    phase match {
      case "leading" =>
        (rs / r) * (term1 + term2 - term3) // subtracts for leading phase
      case "middle" => (rs / r) * (qSq / (rsSq + qSq))
      case "lagging" =>
        (rs / r) * (term1 + term2 + term3) // adds up for lagging phase
    }
  }

  /** Calculates the sheath loss factor for circulating currents in case of
    * systems in trefoil-touching formation and bonded on both ends.
    * @param r
    *   AC-Resistance per Length of conductor at operating temperature
    * @param rs
    *   AC-Resistance per Length of sheath at operating temperature
    * @param x
    *   Reactance per Length of the sheath
    * @return
    *   loss factor lambda1'
    */
  def calcLambda1DashTrefoilTouching(
      r: ElectricalResistancePerLength,
      rs: ElectricalResistancePerLength,
      x: ElectricalResistancePerLength,
  ): Double = {
    (rs / r) * (1 / (1 + pow(rs / x, 2)))
  }

  /** Helper method to calculate the geometric reactance.
    *
    * @param f
    *   System frequency.
    * @param axialCableDistance
    *   Axial distance of the cables (Center to Center).
    * @param averageDiameterSheath
    *   Average diameter of the sheath.
    * @return
    *   Tuple of sheath reactance X, and mutual reactance Xm.
    */
  def calculateReactance(
      f: Frequency,
      axialCableDistance: Length,
      averageDiameterSheath: Length,
  ): (ElectricalResistancePerLength, ElectricalResistancePerLength) = {
    val omega = f.toHertz * TWO_PI

    // 2*10^-7 results from μ0/(2*PI) in Henry pro Meter (H/m)
    val x = OhmsPerMeter(
      omega * 2e-7 * log((axialCableDistance * 2.0) / averageDiameterSheath)
    )
    val xm = OhmsPerMeter(omega * 2e-7 * log(2.0))

    (x, xm)
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
      phaseToGroundVoltage: ElectricPotential,
      frequency: Frequency,
      tanDelta: Double,
      dielectricCapacity: Capacitance,
  ): Power = {
    dielectricCapacity.calculateDielectricLosses(
      phaseToGroundVoltage,
      frequency,
      tanDelta,
    )
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
    * industrial applications p. 215. This assumes, that all three cables are
    * within the same depth and the outer cables have the same distance to the
    * middle one. See CIGRE TB880 p. 94 for further information if this is not
    * the case.
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
    val distancePtoKDashDividedByDistancePtoK = sqrt(
      (pow(distanceOfCables.toMeters, 2) + pow(
        2 * depthCables.toMeters,
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
      tick: Long,
      state: LineState,
      lineCurrent: ElectricCurrent,
  ): LineTemperatures = {
    val currentLineModel = state.currentLineSegmentThermalModel
    val cableSetup = state.cableSetup

    /* Calculate the losses */
    val proximityEffect = 6.6227e-3 // Check CIGRE for detailed method //FIXME
    val skinEffect = 8.835e-3 // FIXME

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
      cableSetup,
    )

    val conductorLosses = calcLossesConductor(acResistance, lineCurrent)

    val phaseToGroundVoltage =
      cableSetup.voltage / sqrt(
        3
      ) // Fixme: This should be the currentVoltage at the cable / average of both connected nodes!
    val dielectricLosses = calcDielectricLosses(
      phaseToGroundVoltage,
      Hertz(50),
      cableSetup.tanDelta,
      cableSetup.electricCapacitance,
    )

    val (acResistanceSheath, sheatAverageDiameter) =
      cableSetup.screenLayer match {
        case Some(layer) => {

          val resistance = calcAcResistanceSheath(
            OhmMeters(
              1.7241e-8
            ), // FIXME this should be of the screen not of the conductor
            layer.wiresNumber,
            layer.wireDiameter,
            layer.lengthOfLay,
            layer.innerDiameter,
            specificTempCoefficient,
            cableSetup.limitTemperature,
            state.currentLineSegmentThermalModel.thermalResistanceT1,
            conductorLosses,
            dielectricLosses,
          )
          val diameter = (layer.innerDiameter + layer.outerDiameter) / 2
          (resistance, diameter)
        }

        case None => (OhmsPerMeter(0d), Millimeters(0d))
      }

    val sheathLossesLeadingPhase = calcLossesSheath(
      cableSetup.layoutFormation,
      acResistance,
      acResistanceSheath,
      cableSetup.distanceCables,
      sheatAverageDiameter,
      "leading",
      cableSetup.eddyCurrentsLossFactorScreen,
      conductorLosses,
    )
    val sheathLossesMiddlePhase = calcLossesSheath(
      cableSetup.layoutFormation,
      acResistance,
      acResistanceSheath,
      cableSetup.distanceCables,
      sheatAverageDiameter,
      "middle",
      cableSetup.eddyCurrentsLossFactorScreen,
      conductorLosses,
    )
    val sheathLossesLaggingPhase = calcLossesSheath(
      cableSetup.layoutFormation,
      acResistance,
      acResistanceSheath,
      cableSetup.distanceCables,
      sheatAverageDiameter,
      "lagging",
      cableSetup.eddyCurrentsLossFactorScreen,
      conductorLosses,
    )

    val thermalTotalLossesLeadingCable =
      conductorLosses + dielectricLosses + sheathLossesLeadingPhase
    val thermalTotalLossesMiddleCable =
      conductorLosses + dielectricLosses + sheathLossesMiddlePhase
    val thermalTotalLossesLaggingCable =
      conductorLosses + dielectricLosses + sheathLossesLaggingPhase

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
          cableSetup.layersJackElements
            .map(_.outerDiameter)
            .maxOption
            .getOrElse(
              throw new IllegalArgumentException(
                "Jack layer expected but not found for thermal resistance to soil calculation"
              )
            ),
          cableSetup.distanceCables,
          thermalTotalLossesLeadingCable,
          thermalTotalLossesMiddleCable,
          thermalTotalLossesLaggingCable,
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
          cableSetup.layersJackElements
            .map(_.outerDiameter)
            .maxOption
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
        cableSetup.layersIsolationElements
          .map(_.innerDiameter)
          .minOption
          .getOrElse(
            throw new IllegalArgumentException("No isolation layers found")
          ),
        cableSetup.layersIsolationElements
          .map(_.outerDiameter)
          .maxOption
          .getOrElse(
            throw new IllegalArgumentException("No isolation layers found")
          ),
      )

    val (
      jackThermCapacitanceC11,
      jackThermCapacitanceC12,
      jackThermCapacitanceC21,
      jackThermCapacitanceC22,
    ) =
      splitCapacitanceByVanWormerShortDuration(
        currentLineModel.thermalCapacityCj,
        cableSetup.layersJackElements
          .map(_.innerDiameter)
          .minOption
          .getOrElse(
            throw new IllegalArgumentException(
              "No jack layers found for van-Wormer capacitance splitting"
            )
          ),
        cableSetup.layersJackElements
          .map(_.outerDiameter)
          .maxOption
          .getOrElse(
            throw new IllegalArgumentException(
              "No jack layers found for van-Wormer capacitance splitting"
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
      (sheathLossesMiddlePhase.toWatts + dielectricLosses.toWatts / 2) / c3,
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

      Celsius(temperature)
    }

    val duration = tick - state.tick
    if duration > 3600 then
      logger.warn(
        s"RC-Network for short durations has been used. However, the duration of $duration ticks might be of type long duration. Currently used method might be inaccurate but should estimate on the safe side."
      )

    LineTemperatures(
      getNodeTemperature(duration, 0),
      getNodeTemperature(duration, 1),
      getNodeTemperature(duration, 2),
      getNodeTemperature(duration, 3),
      getNodeTemperature(duration, 4),
    )
  }

  final case class LineTemperatures(
      currentLineTemp1: Temperature,
      currentLineTemp2: Temperature,
      currentLineTemp3: Temperature,
      currentLineTemp4: Temperature,
      currentLineTemp5: Temperature,
  )
}
