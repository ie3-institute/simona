/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.util.scala.quantities.{
  JoulesPerMeterKelvin,
  KelvinMetersPerWatt,
  ThermalCapacitance,
  ThermalResistivity,
}
import squants.space.Length

import java.util.UUID

final case class CableSetup(
    uuid: UUID,
    id: String,
    conductorMaterial: String,
    conductorCapacitance: ThermalCapacitance,
    conductorDiameter: Length,
    dielectricResistivity: ThermalResistivity,
    dielectricCapacitance: ThermalCapacitance,
    dielectricDiameter: Length,
    fillerResistivity: ThermalResistivity,
    fillerCapacitance: ThermalCapacitance,
    fillerDiameter: Length,
    sheathMaterial: String,
    sheathCapacitance: ThermalCapacitance,
    sheathDiameter: Length,
    jackResistivity: ThermalResistivity,
    jackCapacitance: ThermalCapacitance,
    jackDiameter: Length,
    depthCables: Length,
    distanceCables: Length,
) {}

object CableSetup {

  def apply(
      conductorMaterialInput: String,
      conductorDiameter: Length,
      dielectricMaterial: String,
      dielectricDiameter: Length,
      fillerMaterial: String,
      fillerDiameter: Length,
      sheatMaterialInput: String,
      sheatDiameter: Length,
      jackMaterial: String,
      jackDiameter: Length,
      depthCables: Length,
      distanceCables: Length,
  ): CableSetup = {

    val uuid = UUID.randomUUID()

    val (conductorMaterial, conductorCapacitance) =
      conductorMaterialInput match {
        case "Cooper" => ("Cooper", JoulesPerMeterKelvin(3539200))
        // c = 385 J/(kg * K), rho= 8.96 g/cm³: https://de.wikipedia.org/wiki/Kupfer => 3539200 J / (m³ * K)
        case "Aluminium" => ("Aluminium", JoulesPerMeterKelvin(2420913))
        // c = 897 J/(kg * K), rho= 2.6989 g/cm³: https://de.wikipedia.org/wiki/Aluminium => 2420913 J / (m³ * K)
        case _ =>
          throw new IllegalArgumentException(
            s"Unknown conductor material: $conductorMaterialInput"
          )
      }

    val (dielectricResistivity, dielectricCapacitance) =
      dielectricMaterial match {
        case "XLPE" => (KelvinMetersPerWatt(3.5), JoulesPerMeterKelvin(2.4))
        case "PE"   => (KelvinMetersPerWatt(3.5), JoulesPerMeterKelvin(2.4))
        case "PVC"  => (KelvinMetersPerWatt(5.0), JoulesPerMeterKelvin(1.7))
        case "None" =>
          (KelvinMetersPerWatt(999), JoulesPerMeterKelvin(0.0)) // FIXME
        case _ =>
          throw new IllegalArgumentException(
            s"Unknown dielectric material: $dielectricMaterial"
          )
      }

    val (fillerResistivity, fillerCapacitance) = fillerMaterial match {
      case "XLPE" => (KelvinMetersPerWatt(3.5), JoulesPerMeterKelvin(2.4))
      case "PE"   => (KelvinMetersPerWatt(3.5), JoulesPerMeterKelvin(2.4))
      case "PVC"  => (KelvinMetersPerWatt(5.0), JoulesPerMeterKelvin(1.7))
      case "None" =>
        (KelvinMetersPerWatt(999), JoulesPerMeterKelvin(0.0)) // FIXME
      case _ =>
        throw new IllegalArgumentException(
          s"Unknown filler material: $fillerMaterial"
        )
    }

    val (sheatMaterial, sheatCapacitance) = sheatMaterialInput match {

      case "Cooper" => ("Cooper", JoulesPerMeterKelvin(3539200))
      // c = 385 J/(kg * K), rho= 8.96 g/cm³: https://de.wikipedia.org/wiki/Kupfer => 3539200 J / (m³ * K)
      case "Aluminium" => ("Aluminium", JoulesPerMeterKelvin(2420913))
      // c = 897 J/(kg * K), rho= 2.6989 g/cm³: https://de.wikipedia.org/wiki/Aluminium => 2420913 J / (m³ * K)
      case "None" => ("None", JoulesPerMeterKelvin(0.0)) // FIXME
      case _ =>
        throw new IllegalArgumentException(
          s"Unknown material for the sheat: $sheatMaterialInput"
        )
    }

    val (jackResistivity, jackCapacitance) = jackMaterial match {
      case "XLPE" => (KelvinMetersPerWatt(3.5), JoulesPerMeterKelvin(2.4))
      case "PE"   => (KelvinMetersPerWatt(3.5), JoulesPerMeterKelvin(2.4))
      case "PVC"  => (KelvinMetersPerWatt(5.0), JoulesPerMeterKelvin(1.7))
      case "None" =>
        (KelvinMetersPerWatt(999), JoulesPerMeterKelvin(0.0)) // FIXME
      case _ =>
        throw new IllegalArgumentException(
          s"Unknown material for the jack: $jackMaterial"
        )
    }

    new CableSetup(
      uuid,
      uuid.toString,
      conductorMaterial,
      conductorCapacitance,
      conductorDiameter,
      dielectricResistivity,
      dielectricCapacitance,
      dielectricDiameter,
      fillerResistivity,
      fillerCapacitance,
      fillerDiameter,
      sheatMaterial,
      sheatCapacitance,
      sheatDiameter,
      jackResistivity,
      jackCapacitance,
      jackDiameter,
      depthCables,
      distanceCables,
    )
  }
}
