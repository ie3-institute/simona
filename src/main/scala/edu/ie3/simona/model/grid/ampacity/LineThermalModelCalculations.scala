/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.util.scala.quantities.QuantityConversionUtils.*
import edu.ie3.util.quantities.thermal.{KelvinMetersPerWatt, ThermalResistivity}
import squants.electro.{ElectricPotential, Kilovolts}
import squants.space.Length
import squants.thermal.Celsius
import squants.time.{Frequency, Hertz}
import squants.{Kelvin, Meters, Temperature}

import scala.math.*

/** A collection of methods used for calculation of the
  * [[LineSegmentThermalModel]].
  */
object LineThermalModelCalculations extends LazyLogging {

  // Constants for thermal calculations
  private[ampacity] val TWO_PI: Double = 2 * Pi
  private[ampacity] val PI_OVER_FOUR: Double = Pi / 4
  private[ampacity] val TREFOIL_COEFFICIENT: Double = 1.5
  private[ampacity] val TREFOIL_ADJUSTMENT: Double = 0.63
  private[ampacity] val REFERENCE_TEMPERATURE: Temperature = Celsius(20d)

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
      if cableSetup.layoutFormation == LayoutFormation.TrefoilTouching &&
        degreeOfCover < 0.5
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
    * members in the stranded conductor.
    * @param screenLayer
    *   Cable layer of the screen with type [[ScreenLayer]]
    * @return
    *   The lay factor.
    */
  private[ampacity] def calcLayFactor(
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
