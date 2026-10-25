/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.model.grid.ampacity.LineThermalModelCalculations.{
  PI_OVER_FOUR,
  REFERENCE_TEMPERATURE,
  TWO_PI,
  calcLayFactor,
}
import edu.ie3.util.scala.quantities.QuantityConversionUtils.*
import edu.ie3.util.scala.quantities.{
  SpecificResistance,
  KelvinMetersPerWatt,
  OhmsPerMeter,
  PowerPerLength,
  SpecificCapacitance,
  ThermalResistivity,
  WattsPerMeter,
}
import edu.ie3.util.scala.quantities.SquantsUtils.{
  RichResistivity,
  RichSpecificCapacitance,
}
import squants.electro.*
import squants.space.{Length, Millimeters, SquareMeters}
import squants.thermal.Celsius
import squants.time.{Frequency, Hertz}
import squants.{ElectricCurrent, Kelvin, Temperature}

import scala.math.*

/** A collection of methods for calculating AC resistances and thermal losses
  * (conductor, sheath, armor, dielectric) of a cable segment.
  */
object CableLossCalculations {

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
      electricalResistanceCable: SpecificResistance,
      temperatureCorrectionFactor: Double,
      cableSetup: CableSetup,
      distanceCables: Length,
  ): SpecificResistance = {
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
  ): SpecificResistance = {
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
      rDc: SpecificResistance,
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
      rDc: SpecificResistance,
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
      acResistance: SpecificResistance,
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
      layoutFormation: LayoutFormation,
      r: SpecificResistance,
      rs: SpecificResistance,
      axialCableDistance: Length,
      averageDiameterSheath: Length,
      phase: String,
      eddyCurrentsSheathLossFactor: Double,
      conductorLosses: PowerPerLength,
  ): PowerPerLength = {
    val (x, xm) =
      calculateReactance(Hertz(50), axialCableDistance, averageDiameterSheath)
    val lambda1Dash = layoutFormation match {
      case LayoutFormation.FlatDistance =>
        calcLambda1DashFlatDistance(r, rs, x, xm, phase)
      case LayoutFormation.FlatTouching =>
        calcLambda1DashFlatDistance(r, rs, x, xm, phase)
      case LayoutFormation.TrefoilNotTouching =>
        throw new IllegalArgumentException(
          s"Trefoil not touching layout formation is currently not supported"
        )
      case LayoutFormation.TrefoilTouching =>
        calcLambda1DashTrefoilTouching(r, rs, x)
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
      r: SpecificResistance,
      rs: SpecificResistance,
      x: SpecificResistance,
      xm: SpecificResistance,
      phase: String,
  ): Double = {
    // Auxiliary quantity by IEC 60287
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
      r: SpecificResistance,
      rs: SpecificResistance,
      x: SpecificResistance,
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
  ): (SpecificResistance, SpecificResistance) = {
    val omega = f.toHertz * TWO_PI

    // 2*10^-7 results from μ0/(2*PI) in Henry per Meter (H/m)
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
    val lambdaTwo =
      circulatingArmorLossFactor + eddyCurrentsArmorLossFactor
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
}
