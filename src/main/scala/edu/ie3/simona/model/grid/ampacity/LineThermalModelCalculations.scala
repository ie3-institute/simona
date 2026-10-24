/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import org.apache.commons.math3.linear.{
  Array2DRowRealMatrix,
  ArrayRealVector,
  LUDecomposition,
  RealMatrix,
  RealVector,
}

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.model.grid.ampacity.LineSegmentThermalModel.LineState
import edu.ie3.simona.model.grid.ampacity.LineThermalModelNetworkSolver
import edu.ie3.util.scala.quantities.SquantsUtils.{
  RichResistivity,
  RichSpecificCapacitance,
}
import edu.ie3.util.scala.quantities.QuantityConversionUtils.*
import edu.ie3.util.scala.quantities.{
  ElectricalResistancePerLength,
  JoulesPerCubicMeterKelvin,
  JoulesPerMeterKelvin,
  KelvinMetersPerWatt,
  OhmsPerMeter,
  PowerPerLength,
  SpecificCapacitance,
  ThermalCapacitance,
  ThermalCapacityPerLength,
  ThermalResistivity,
  WattsPerMeter,
  SquantsUtils as RichElectricPotential,
}
import squants.electro.*
import squants.space.*
import squants.thermal.Celsius
import squants.time.{Frequency, Hertz}
import squants.{ElectricCurrent, Kelvin, Meters, Temperature}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.CELSIUS

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

  /** Calculates the thermal resistance T1 of the inner cable elements between
    * conductor and screen.
    *
    * @param cableSetup
    *   The setup of the cable in this line segment.
    * @param voltage
    *   The rated voltage of the cable.
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

  /** Calculates the thermal resistance T2 of the cable elements between screen
    * and the outer cable covering. This includes the filler and armour
    * elements.
    *
    * @param cableSetup
    *   The setup of the cable in this line segment.
    * @return
    *   The thermal resistance per unit of length.
    */
  def calcThermalResistanceT2(
      cableSetup: CableSetup
  ): ThermalResistivity = {
    val t2Filler =
      cableSetup.layersFillerElements.foldLeft(KelvinMetersPerWatt(0)) {
        (acc, layer) =>
          acc + calcThermalResistanceCableShells(
            layer.thermalResistivity,
            layer.innerDiameter,
            layer.outerDiameter,
          )
      }

    val t2Armor =
      cableSetup.layersArmorElements.foldLeft(KelvinMetersPerWatt(0)) {
        (acc, layer) =>
          acc + calcThermalResistanceCableShells(
            layer.thermalResistivity,
            layer.innerDiameter,
            layer.outerDiameter,
          )
      }

    t2Filler + t2Armor
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
    *   Cable layer of the screen with type [[ScreenLayer]].
    * @return
    *   Returns the degree of cover how much of the screen surface is covered by
    *   screen elements (e.g. screen wires).
    */
  private def calcDegreeOfScreenCover(
      screenLayer: Option[ScreenLayer]
  ): Double = {
    val layFactor = calcLayFactor(screenLayer)
    screenLayer match {
      case Some(layer) =>
        (layer.wireDiameter * layer.wiresNumber * layFactor) / ((layer.innerDiameter + layer.wireDiameter) * Pi)
      case None => 1.0
    }
  }

  /** Calculates the lay factor of the screen layer which is the ratio of the
    * lay length to the external diameter of the corresponding layer of wires or
    * members in the stranded conductor (measures how much longer a spiraled (or
    * stranded) wire is compared to a perfectly straight, parallel wire).
    * @param screenLayer
    *   Cable layer of the screen with type [[ScreenLayer]]
    * @return
    *   The lay factor.
    */
  private def calcLayFactor(
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
    * @param distanceCables
    *   The distance between the cables.
    * @return
    *   The AC resistance per unit of length.
    */
  def calcAcResistance(
      electricalResistanceCable: ElectricalResistancePerLength,
      temperatureCorrectionFactor: Double,
      cableSetup: CableSetup,
      distanceCables: Length,
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
      distanceCables,
    )

    rDc * (1 + factorSkinEffect + factorProximityEffect)
  }

  /** Calculates the AC resistance per unit of length of the sheath accounting
    * for temperature and high-frequency effects (skin effect, proximity
    * effect).
    * @param screenLayer
    *   Cable layer of the screen with type [[ScreenLayer]].
    * @param limitTemperature
    *   Limit temperature of the cable.
    * @param thermalResistanceT1
    *   The resistance T1 (between conductor and screen) of this line segment.
    * @param conductorLosses
    *   The thermal losses of the conductor of this line segment in power per
    *   unit cable length.
    * @param dielectricLosses
    *   The thermal losses caused by dielectric losses of this line segment in
    *   power per unit cable length.
    * @return
    *   The AC resistance per unit of length.
    */
  def calcAcResistanceSheath(
      screenLayer: Option[ScreenLayer],
      limitTemperature: Temperature,
      thermalResistanceT1: ThermalResistivity,
      conductorLosses: PowerPerLength,
      dielectricLosses: PowerPerLength,
  ): ElectricalResistancePerLength = {
    // Calculation of cross-sectional area of the sheath
    screenLayer match {
      case Some(layer) =>
        val area = layer.area.getOrElse(
          SquareMeters(
            layer.wiresNumber * PI_OVER_FOUR * pow(
              layer.wireDiameter.toMeters,
              2,
            )
          )
        )

        val layFactor = calcLayFactor(screenLayer)

        // ac resistance per unit length of screen at 20°C
        val r0Screen = (layer.materialResistivity / area) * layFactor

        // calculate operating temperatur of the screen
        val screenTemp: Temperature =
          limitTemperature - Kelvin(
            (conductorLosses.toWattsPerMeter + 0.5 * dielectricLosses.toWattsPerMeter) * thermalResistanceT1.toKelvinMetersPerWatt
          )

        // ac resistance of screen wires at operating temp
        r0Screen * (1 + layer.material.getElectricalResistivityTemperatureCoefficient * (screenTemp.toCelsiusScale - REFERENCE_TEMPERATURE.toCelsiusScale))
      case None =>
        throw new IllegalArgumentException(
          "Screen layer expected but not found."
        )
    }
  }

  /** Calculates the skin effect factor (y_s) according to IEC 60287.
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
  private def calculateSkinEffectFactor(
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
  private def calculateProximityEffectFactor(
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
  ): PowerPerLength = {
    val currentSquared = current.toAmperes * current.toAmperes
    WattsPerMeter(currentSquared * acResistance.toOhmsPerMeter)
  }

  /** Calculates the losses within the cable sheath. Zero / Not applicable if
    * cable has no sheath.
    * @param layoutFormation
    *   Formation in which the cables are laid.
    * @param r
    *   AC-Resistance per Length of conductor at operating temperature.
    * @param rs
    *   AC-Resistance per Length of sheath at operating temperature.
    * @param axialCableDistance
    *   Axial distance of the cables (Center to Center).
    * @param averageDiameterSheath
    *   Average diameter of the sheath.
    * @param phase
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
      conductorLosses: PowerPerLength,
  ): PowerPerLength = {
    val (x, xm) =
      calculateReactance(Hertz(50), axialCableDistance, averageDiameterSheath)
    val lambda1Dash = layoutFormation match {
      case "flat-distance" => calcLambda1DashFlatDistance(r, rs, x, xm, phase)
      case "flat-touching" => calcLambda1DashFlatDistance(r, rs, x, xm, phase)
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
    *   Loss factor lambda1.
    */
  private def calcLambda1DashFlatDistance(
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
  private def calcLambda1DashTrefoilTouching(
      r: ElectricalResistancePerLength,
      rs: ElectricalResistancePerLength,
      x: ElectricalResistancePerLength,
  ): Double = {
    (rs / r) * (1 / (1 + pow(rs / x, 2)))
  }

  /** Helper method to calculate the geometric reactance.
    * @param f
    *   System frequency.
    * @param axialCableDistance
    *   Axial distance of the cables (Center to Center).
    * @param averageDiameterSheath
    *   Average diameter of the sheath.
    * @return
    *   Tuple of sheath reactance X, and mutual reactance Xm.
    */
  private def calculateReactance(
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
      conductorLosses: PowerPerLength,
  ): PowerPerLength = {
    // lambda_2 = lambda_2_dash + lambda_2_dash_dash
    val lambdaTwo = circulatingArmorLossFactor + eddyCurrentsArmorLossFactor
    conductorLosses * lambdaTwo
  }

  /** Calculates the losses within the cable that are not current-dependent, per
    * unit cable length.
    * @param phaseToGroundVoltage
    *   The phase-to-ground voltage U0 of the cable system
    * @param frequency
    *   The frequency of the system (50 Hz) in general.
    * @param tanDelta
    *   The dissipation factor of the cable.
    * @param specificDielectricCapacity
    *   The specific capacitance per unit length of the cable dielectric.
    * @return
    *   The voltage dependent dielectric losses per unit cable length.
    */
  def calcDielectricLosses(
      phaseToGroundVoltage: ElectricPotential,
      frequency: Frequency,
      tanDelta: Double,
      specificDielectricCapacity: SpecificCapacitance,
  ): PowerPerLength = {
    specificDielectricCapacity.calculateSpecificDielectricLosses(
      phaseToGroundVoltage,
      frequency,
      tanDelta,
    )
  }

  /** Calculates the thermal resistivity of the individual layers of the cable
    * (e.g. for isolation shell as the first shell between conductor and further
    * outer layers of the cable).
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
  ): ThermalResistivity =
    (specificThermalResistivity / TWO_PI) * log(outerRadius / innerRadius)

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
    * industrial applications p. 215. This assumes, that all three cables are
    * within the same depth and the outer cables have the same distance to the
    * middle one. See CIGRE TB880 p. 94 for further information if this is not
    * the case.
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
    * touches each other. Cable formation: (A)(B)(C). Reference Anders Rating of
    * electric power cables: ampacity computations for transmission,
    * distribution, and industrial applications p. 218.
    * @param specificThermalResistivityGround
    *   The material dependent specific thermal resistance of the surrounding
    *   soil.
    * @param depthCables
    *   The laying depth of the cables.
    * @param diameterCableB
    *   The losses of the cable B.
    * @return
    *   The thermal resistance between cable and its surrounding soil in Kelvin
    *   * Meter / Watt per unit cable length.
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
    * cable) for a trefoil touching formation. Reference: Anders Rating of
    * electric power cables: ampacity computations for transmission,
    * distribution, and industrial applications p. 220
    * @param specificThermalResistivityGround
    *   The material dependent specific thermal resistance of the surrounding
    *   soil.
    * @param depthToCenter
    *   The laying depth of the cables measured to the center of the trefoil
    *   group.
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
    val u = (depthToCenter * -2) / diameterCable

    KelvinMetersPerWatt(
      TREFOIL_COEFFICIENT * specificThermalResistivityGround.toKelvinMetersPerWatt / Pi
    ) * (log(2 * u) - TREFOIL_ADJUSTMENT)
  }

  /** Calculates the thermal resistivity per length unit for a cylindrical area.
    * @param thermalCapacitanceMaterial
    *   The material dependent specific thermal capacitance.
    * @param innerDiameter
    *   The inner diameter of this area.
    * @param outerDiameter
    *   The outer diameter of this area.
    * @return
    *   The thermal capacity per length unit of this area.
    */
  def calcThermalCapacityCylindrical(
      thermalCapacitanceMaterial: ThermalCapacitance,
      innerDiameter: Length,
      outerDiameter: Length,
  ): ThermalCapacityPerLength = {
    val areaDifference = SquareMeters(
      (pow(outerDiameter.toMeters, 2) -
        pow(innerDiameter.toMeters, 2)) * PI_OVER_FOUR
    )

    thermalCapacitanceMaterial * areaDifference
  }

  /** Calculates the thermal resistivity per length unit for a rectangular area.
    * @param thermalCapacitanceMaterial
    *   The material dependent specific thermal capacitance.
    * @param width
    *   The width of this area.
    * @param height
    *   The height of this area.
    * @return
    *   The thermal capacity per length unit of this area.
    */
  def calcThermalCapacityRectangular(
      thermalCapacitanceMaterial: ThermalCapacitance,
      width: Length,
      height: Length,
  ): ThermalCapacityPerLength =
    val area = width * height
    thermalCapacitanceMaterial * area

  /** Determines the Van-Wormer-Coefficient for long-duration transients.
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
    * @param thermalCapacityPerLength
    *   The capacity that should be split.
    * @param innerDiameter
    *   The inner diameter of the shell that creates the capacity.
    * @param outerDiameter
    *   The outer diameter of the shell that creates the capacity.
    * @return
    *   Returns the four parts C11,C12,C21 and C22 of the input capacity.
    */

  private def splitCapacitanceByVanWormerShortDuration(
      thermalCapacityPerLength: ThermalCapacityPerLength,
      innerDiameter: Length,
      outerDiameter: Length,
  ) = {
    val diameterMid = innerDiameter + (outerDiameter - innerDiameter) / 2
    val vanWormerJackFirstHalf =
      vanWormerCoefficientShortTermDurationTransients(
        innerDiameter,
        diameterMid,
      )
    val capacitanceC11 = thermalCapacityPerLength * vanWormerJackFirstHalf
    val capacitanceC12 = thermalCapacityPerLength * (1 - vanWormerJackFirstHalf)
    val vanWormerJackSecondHalf =
      vanWormerCoefficientShortTermDurationTransients(
        diameterMid,
        outerDiameter,
      )
    val capacitanceC21 = thermalCapacityPerLength * vanWormerJackSecondHalf
    val capacitanceC22 =
      thermalCapacityPerLength * (1 - vanWormerJackSecondHalf)
    (capacitanceC11, capacitanceC12, capacitanceC21, capacitanceC22)
  }

  def createAndCalcRCNetworkMvCableShortDuration(
      tick: Long,
      state: LineState,
      lineCurrent: ElectricCurrent,
      groundTemperature: Temperature,
  ): LineTemperatures = {
    val currentLineModel = state.currentLineSegmentThermalModel
    val cableSetup = state.cableSetup

    val temperatureDifference = Quantities.getQuantity(
      (state.lineTemperatures.currentLineTemp4 - groundTemperature).toCelsiusDegrees,
      CELSIUS,
    )

    val effectiveSoilResistivity =
      currentLineModel.soilType
        .currentThermalResistivity(temperatureDifference)
        .toSquants

    val (bedResistivity, cableBed) = currentLineModel.cableBed match {
      case Some(bed) =>
        val bedRes =
          bed.soilType
            .currentThermalResistivity(temperatureDifference)
            .toSquants
        (Some(bedRes), Some(bed))
      case None => (None, None)
    }

    val baseSoilResistivity =
      bedResistivity.getOrElse(effectiveSoilResistivity)

    // TODO DF check this
    val soilCapacitance = currentLineModel.thermalCapacityCe

    val acResistance = calcAcResistance(
      cableSetup.electricResistance,
      cableSetup.conductor.material.getElectricalResistivityTemperatureCoefficient,
      cableSetup,
      currentLineModel.distanceCables,
    )

    val conductorLosses = calcLossesConductor(acResistance, lineCurrent)

    val phaseToGroundVoltage = state.currentVoltage / sqrt(3)

    val dielectricLosses = calcDielectricLosses(
      phaseToGroundVoltage,
      cableSetup.frequency,
      cableSetup.tanDelta,
      cableSetup.electricCapacitance,
    )

    val (acResistanceSheath, sheatAverageDiameter) =
      cableSetup.screenLayer match {
        case Some(layer) =>
          val resistance = calcAcResistanceSheath(
            cableSetup.screenLayer,
            cableSetup.limitTemperature,
            state.currentLineSegmentThermalModel.thermalResistanceT1,
            conductorLosses,
            dielectricLosses,
          )
          val diameter = (layer.innerDiameter + layer.outerDiameter) / 2
          (resistance, diameter)

        case None => (OhmsPerMeter(0d), Millimeters(0d))
      }

    val sheathLossesLeadingPhase = calcLossesSheath(
      cableSetup.layoutFormation,
      acResistance,
      acResistanceSheath,
      currentLineModel.distanceCables,
      sheatAverageDiameter,
      "leading",
      cableSetup.eddyCurrentsLossFactorScreen,
      conductorLosses,
    )
    val sheathLossesMiddlePhase = calcLossesSheath(
      cableSetup.layoutFormation,
      acResistance,
      acResistanceSheath,
      currentLineModel.distanceCables,
      sheatAverageDiameter,
      "middle",
      cableSetup.eddyCurrentsLossFactorScreen,
      conductorLosses,
    )
    val sheathLossesLaggingPhase = calcLossesSheath(
      cableSetup.layoutFormation,
      acResistance,
      acResistanceSheath,
      currentLineModel.distanceCables,
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
    val t4Base = cableSetup.layoutFormation match {
      case "flat-distance" =>
        calcThermalResistanceToSoilThreeSingleCoreFlatFormationDistance(
          baseSoilResistivity,
          currentLineModel.depthCables,
          cableSetup.layersJackElements
            .map(_.outerDiameter)
            .maxOption
            .getOrElse(
              throw new IllegalArgumentException(
                "Jack layer expected but not found for thermal resistance to soil calculation"
              )
            ),
          currentLineModel.distanceCables,
          thermalTotalLossesLeadingCable,
          thermalTotalLossesMiddleCable,
          thermalTotalLossesLaggingCable,
        )
      case "flat-touching" =>
        calcThermalResistanceToSoilThreeSingleCoreFlatFormationTouching(
          baseSoilResistivity,
          currentLineModel.depthCables,
          cableSetup.layersJackElements
            .map(_.outerDiameter)
            .maxOption
            .getOrElse(
              throw new IllegalArgumentException(
                "Jack layer expected but not found for thermal resistance to soil calculation"
              )
            ),
        ) // FIXME DF add tests!
      case "trefoil-not-touching" => // FIXME DF Check for Trefoil
        throw new IllegalArgumentException(
          s"Trefoil not touching layout formation is currently not supported"
        )
      case "trefoil-touching" =>
        calcThermalResistanceToSoilThreeSingleCoreTrefoilTouching(
          baseSoilResistivity,
          currentLineModel.depthCables,
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

    val t4 = cableBed match {
      case Some(bed) =>
        val bedRes =
          bed.soilType
            .currentThermalResistivity(temperatureDifference)
            .toSquants
        val bedRadius = calcCableBedEquivalentRadius(bed.width, bed.height)
        val bedCenterDepth = Meters(-currentLineModel.depthCables.toMeters)
        val lossWeightFactor = cableSetup.layoutFormation match {
          case "flat-distance" =>
            val wSum =
              thermalTotalLossesLeadingCable.toWattsPerMeter +
                thermalTotalLossesMiddleCable.toWattsPerMeter +
                thermalTotalLossesLaggingCable.toWattsPerMeter
            wSum / thermalTotalLossesMiddleCable.toWattsPerMeter
          case _ => 3.0
        }
        val correction = calcCableBedT4OuterCorrection(
          effectiveSoilResistivity,
          bedRes,
          bedCenterDepth,
          bedRadius,
          lossWeightFactor,
        )
        t4Base + correction
      case None => t4Base
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

    /* Build the short-duration RC network. */
    // Van-Wormer splitting replaces each cable-shell resistance with two half-resistances.
    val g1 = 2d / t1.toKelvinMetersPerWatt
    val g2 = 2d / t1.toKelvinMetersPerWatt
    val g3 = 2d / t3.toKelvinMetersPerWatt
    val g4 = 2d / t3.toKelvinMetersPerWatt

    // the RC-Network can be simplified since all parallel capacitance can be merged
    // Conductor capacitance and first part of the first half of the dielectric
    val c1 =
      (currentLineModel.thermalCapacityCc + dielectricThermCapacitanceC11).toJoulesPerMeterKelvin // FIXME DF check conversion factor !
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
    val (
      nodeCount,
      matrixAData,
      vectorBData,
    ) = cableBed match {
      case None =>
        // 5-node network: cable nodes 1-4 and node 5 bundling the cable outer
        // surface (jack) with the outer soil. T4 connects node 5 to ground.
        val g5 = 1d / t4.toKelvinMetersPerWatt
        val c5 =
          (jackThermCapacitanceC22 + soilCapacitance).toJoulesPerMeterKelvin
        (
          5,
          Array(
            Array(-g1 / c1, g1 / c1, 0.0, 0.0, 0.0),
            Array(g1 / c2, (-g1 - g2) / c2, g2 / c2, 0.0, 0.0),
            Array(0.0, g2 / c3, (-g2 - g3) / c3, g3 / c3, 0.0),
            Array(0.0, 0.0, g3 / c4, (-g3 - g4) / c4, g4 / c4),
            Array(0.0, 0.0, 0.0, g4 / c5, (-g4 - g5) / c5),
          ),
          Array(
            (conductorLosses.toWattsPerMeter + dielectricLosses.toWattsPerMeter / 2) / c1,
            0d,
            (sheathLossesMiddlePhase.toWattsPerMeter + dielectricLosses.toWattsPerMeter / 2) / c3,
            0d,
            (g5 * groundTemperature.toCelsiusScale) / c5,
          ),
        )
      case Some(bed) =>
        // Ten-node network: five cable nodes, two bed cells, and three outer-soil cells.
        val bedRes =
          bed.soilType
            .currentThermalResistivity(temperatureDifference)
            .toSquants
        val bedRadius = calcCableBedEquivalentRadius(bed.width, bed.height)
        val bedCenterDepth = Meters(-currentLineModel.depthCables.toMeters)
        if bedCenterDepth <= bedRadius then {
          val message =
            s"Cable bed center depth $bedCenterDepth must exceed its equivalent radius $bedRadius."
          logger.error(message)
          throw new IllegalArgumentException(message)
        }
        val lossWeightFactor = cableSetup.layoutFormation match {
          case "flat-distance" =>
            val wSum =
              thermalTotalLossesLeadingCable.toWattsPerMeter +
                thermalTotalLossesMiddleCable.toWattsPerMeter +
                thermalTotalLossesLaggingCable.toWattsPerMeter
            wSum / thermalTotalLossesMiddleCable.toWattsPerMeter
          case _ => 3.0
        }
        val (bedResistance, soilResistance) = calcCableBedT4Split(
          t4Base,
          bedRes,
          effectiveSoilResistivity,
          bedCenterDepth,
          bedRadius,
          lossWeightFactor,
        )
        val cableOuterRadius =
          cableSetup.layersJackElements
            .map(_.outerDiameter)
            .maxOption
            .getOrElse(
              throw new IllegalArgumentException(
                "Jack layer expected for environmental resistance calculation"
              )
            ) / 2d
        val soilOuterRadius =
          calcSoilModelOuterRadius(bedCenterDepth, bedRadius)
        val cells = calcCableBedEnvironmentalCells(
          bedResistance,
          soilResistance,
          cableOuterRadius,
          bedRadius,
          soilOuterRadius,
          bed.soilType.getSpecificHeatCapacity.toSquants,
          currentLineModel.soilType.getSpecificHeatCapacity.toSquants,
        )
        val cellResistances = cells.map(_.resistance.toKelvinMetersPerWatt)
        val cellConductances = Array.tabulate(cells.size) { index =>
          if index == cells.size - 1 then 2d / cellResistances(index)
          else 2d / (cellResistances(index) + cellResistances(index + 1))
        }
        val cableToEnvironmentConductance = 2d / cellResistances.head
        val capacities = Array(
          c1,
          c2,
          c3,
          c4,
          jackThermCapacitanceC22.toJoulesPerMeterKelvin,
        ) ++ cells.map(_.thermalCapacity.toJoulesPerMeterKelvin)
        val matrix = Array.tabulate(10, 10)((_, _) => 0d)
        val cableConductances =
          Array(g1, g2, g3, g4, cableToEnvironmentConductance)
        for row <- 0 until 5 do {
          val leftConductance =
            if row == 0 then 0d else cableConductances(row - 1)
          val rightConductance = cableConductances(row)
          matrix(row)(row) =
            -(leftConductance + rightConductance) / capacities(row)
          if row > 0 then
            matrix(row)(row - 1) = leftConductance / capacities(row)
          matrix(row)(row + 1) = rightConductance / capacities(row)
        }
        for index <- cells.indices do {
          val row = index + 5
          val leftConductance =
            if index == 0 then cableToEnvironmentConductance
            else cellConductances(index - 1)
          val rightConductance = cellConductances(index)
          matrix(row)(row) =
            -(leftConductance + rightConductance) / capacities(row)
          matrix(row)(row - 1) = leftConductance / capacities(row)
          if index < cells.size - 1 then
            matrix(row)(row + 1) = rightConductance / capacities(row)
        }
        val vector = Array.fill(10)(0d)
        vector(0) =
          (conductorLosses.toWattsPerMeter + dielectricLosses.toWattsPerMeter / 2) / c1
        vector(2) =
          (sheathLossesMiddlePhase.toWattsPerMeter + dielectricLosses.toWattsPerMeter / 2) / c3
        vector(9) =
          cellConductances.last * groundTemperature.toCelsiusScale / capacities.last
        (10, matrix, vector)
    }

    val matrixA: RealMatrix = new Array2DRowRealMatrix(matrixAData)
    val vectorB: RealVector = new ArrayRealVector(vectorBData)

    val (eigenvalues, eigenvectors) =
      LineThermalModelNetworkSolver.determineEigenvaluesAndVectors(matrixA)

    val stateTemps = state.lineTemperatures
    val vStartEntries =
      if nodeCount == 5 then
        Array(
          stateTemps.currentLineTemp1.toCelsiusScale,
          stateTemps.currentLineTemp2.toCelsiusScale,
          stateTemps.currentLineTemp3.toCelsiusScale,
          stateTemps.currentLineTemp4.toCelsiusScale,
          stateTemps.currentLineTemp5.toCelsiusScale,
        )
      else
        Array(
          stateTemps.currentLineTemp1.toCelsiusScale,
          stateTemps.currentLineTemp2.toCelsiusScale,
          stateTemps.currentLineTemp3.toCelsiusScale,
          stateTemps.currentLineTemp4.toCelsiusScale,
          stateTemps.currentLineTemp5.toCelsiusScale,
          stateTemps.currentLineTemp6.toCelsiusScale,
          stateTemps.currentLineTemp7.toCelsiusScale,
          stateTemps.currentLineTemp8.toCelsiusScale,
          stateTemps.currentLineTemp9.toCelsiusScale,
          stateTemps.currentLineTemp10.toCelsiusScale,
        )

    val vStart: RealVector = new ArrayRealVector(vStartEntries)

    val luSolver = new LUDecomposition(matrixA).getSolver
    val vp: RealVector = luSolver.solve(vectorB.mapMultiply(-1.0))

    val vStartMinusVp: RealVector = vStart.subtract(vp)
    val evSolver = new LUDecomposition(eigenvectors).getSolver
    val c: RealVector = evSolver.solve(vStartMinusVp)

    def getNodeTemperature(t: Long, node: Int): Temperature = {
      var temperature =
        vp.getEntry(node)
      for i <- 0 until nodeCount do {
        temperature +=
          c.getEntry(i) * eigenvectors.getEntry(node, i) * math.exp(
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

    if nodeCount == 5 then
      LineTemperatures(
        getNodeTemperature(duration, 0),
        getNodeTemperature(duration, 1),
        getNodeTemperature(duration, 2),
        getNodeTemperature(duration, 3),
        getNodeTemperature(duration, 4),
        Celsius(0d),
        Celsius(0d),
        Celsius(0d),
        Celsius(0d),
        Celsius(0d),
      )
    else
      LineTemperatures(
        getNodeTemperature(duration, 0),
        getNodeTemperature(duration, 1),
        getNodeTemperature(duration, 2),
        getNodeTemperature(duration, 3),
        getNodeTemperature(duration, 4),
        getNodeTemperature(duration, 5),
        getNodeTemperature(duration, 6),
        getNodeTemperature(duration, 7),
        getNodeTemperature(duration, 8),
        getNodeTemperature(duration, 9),
      )
  }

  /** Calculates the equivalent radius of a rectangular cable bed cross-section
    * according to Anders (1997), Eq. (9.61). The rectangle sides are sorted so
    * that x <= y. The formula is valid only for y/x < 3.
    *
    * @param width
    *   The width of the rectangular cable bed.
    * @param height
    *   The height of the rectangular cable bed.
    * @return
    *   The equivalent radius in meters.
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

  /** Calculates the Kennelly outer geometric factor `ln(u + sqrt(u^2 - 1))`
    * with `u = L_b / r_b`, where `L_b` is the bed-center depth below the ground
    * surface and `r_b` the equivalent bed radius.
    *
    * @param bedCenterDepth
    *   The depth of the bedding center below the ground surface (positive).
    * @param bedRadius
    *   The equivalent radius of the cable bed.
    * @return
    *   The Kennelly outer geometric factor.
    */
  def calcCableBedOuterGeometricFactor(
      bedCenterDepth: Length,
      bedRadius: Length,
  ): Double = {
    val ratio = bedCenterDepth.toMeters / bedRadius.toMeters
    log(ratio + sqrt(pow(ratio, 2) - 1))
  }

  /** Calculates the outer-region T4 correction for a cable bed. The correction
    * accounts for the resistivity difference between the surrounding soil and
    * the bedding material in the region from the equivalent bedding radius to
    * the ground surface.
    *
    * @param soilResistivity
    *   The thermal resistivity of the surrounding soil.
    * @param bedResistivity
    *   The thermal resistivity of the cable bed material.
    * @param bedCenterDepth
    *   The depth of the bedding center below the ground surface (positive).
    * @param bedRadius
    *   The equivalent radius of the cable bed.
    * @param lossWeightFactor
    *   The loss weight factor sum(W_k / W_p). Equals 3 for equal losses.
    * @return
    *   The outer-region T4 correction in Kelvin * Meter / Watt per unit cable
    *   length.
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
    * (from the cable surface to the equivalent bed radius, in the bed material)
    * and the outer-soil-region part (from the equivalent bed radius to the
    * ground surface, in the soil material). Their sum equals the total T4.
    *
    * @param baseT4
    *   The base external resistance computed as if the entire path were made of
    *   the bed material (T4' in Anders 1997).
    *
    * @param bedResistivity
    *   The thermal resistivity of the cable bed material.
    * @param soilResistivity
    *   The thermal resistivity of the surrounding soil.
    * @param bedCenterDepth
    *   The depth of the bedding center below the ground surface (positive).
    * @param bedRadius
    *   The equivalent radius of the cable bed.
    * @param lossWeightFactor
    *   The loss weight factor sum(W_k / W_p). Equals 3 for equal losses.
    * @return
    *   A tuple of (bedding-region resistance, outer-soil-region resistance).
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
    * according to Diaz-Aguilo et al. (2014), Eq. (13): `d_m = L + sqrt(L^2 -
    * r^2)`, where `L` is the depth and `r` the inner radius of the soil region.
    *
    * @param depth
    *   The depth below the ground surface (positive).
    * @param innerRadius
    *   The inner radius of the soil region.
    * @return
    *   The resistance-matched outer radius of the soil model.
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
    * length as the area of the resistance-matched soil annulus times the soil's
    * volumetric specific heat capacity.
    *
    * @param specificHeatCapacity
    *   The volumetric specific heat capacity of the surrounding soil.
    * @param depth
    *   The depth below the ground surface (positive).
    * @param innerRadius
    *   The inner radius of the soil region (equivalent bed radius when a bed is
    *   present, otherwise the cable outer radius).
    *
    * @return
    *   The thermal capacity of the soil region in Joule / Kelvin per unit cable
    *   length.
    */
  def calcSoilThermalCapacity(
      specificHeatCapacity: ThermalCapacitance,
      depth: Length,
      innerRadius: Length,
  ): ThermalCapacitance = {
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
      bedHeatCapacity: ThermalCapacitance,
      soilHeatCapacity: ThermalCapacitance,
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
        heatCapacity: ThermalCapacitance,
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
