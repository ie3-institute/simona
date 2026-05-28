/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.util.scala.quantities.*
import squants.Temperature
import squants.electro.{Capacitance, ElectricPotential, Resistivity}
import squants.space.{Area, Length}
import squants.time.Frequency

import java.util.UUID

final case class Layer(
    name: String,
    material: CableMaterial,
    innerDiameter: Length,
    outerDiameter: Length,
    thermalResistivity: ThermalResistivity,
    thermalCapacitance: ThermalCapacitance,
    area: Option[Area],
)

/** Screen layer with specific parameters for conductor shielding
  *
  * @param material
  *   Material of the screen
  * @param innerDiameter
  *   Inner diameter of the screen layer
  * @param outerDiameter
  *   Outer diameter of the screen layer
  * @param thermalResistivity
  *   Thermal resistivity of the material
  * @param thermalCapacitance
  *   Thermal capacitance per meter
  * @param area
  *   Optional cross-sectional area
  * @param wiresNumber
  *   Number of wires in the screen
  * @param wireDiameter
  *   Diameter of individual wire in the screen
  * @param lengthOfLay
  *   Length of lay (pitch) of the screen winding
  * @param materialResistivity
  *   Electrical resistivity specific to the screen material
  */
final case class ScreenLayer(
    material: CableMaterial,
    innerDiameter: Length,
    outerDiameter: Length,
    thermalResistivity: ThermalResistivity,
    thermalCapacitance: ThermalCapacitance,
    area: Option[Area],
    wiresNumber: Int,
    wireDiameter: Length,
    lengthOfLay: Option[Length],
    materialResistivity: Resistivity,
)

final case class CableSetup(
    uuid: UUID,
    id: String,
    conductor: Layer,
    layersIsolationElements: List[Layer],
    screenLayer: Option[ScreenLayer],
    layersFillerElements: List[Layer],
    layersArmorElements: List[Layer],
    layersJackElements: List[Layer],
    layoutFormation: String,
    depthCables: Length,
    distanceCables: Length,
    soilResistivity: ThermalResistivity,
    soilCapacitance: ThermalCapacitance,
    limitTemperature: Temperature,
    voltage: ElectricPotential,
    frequency: Frequency,
    electricResistance: ElectricalResistancePerLength,
    skinEffectCoefficient: Double,
    proximityEffectCoefficient: Double,
    electricCapacitance: Capacitance,
    tanDelta: Double,
    circulatingLossFactorScreen: Double,
    eddyCurrentsLossFactorScreen: Double,
)
object CableSetup {
  def materialProps(
      mat: CableMaterial
  ): (ThermalResistivity, ThermalCapacitance) =
    mat match {
      case CableMaterial.Copper =>
        (
          KelvinMetersPerWatt(
            1 / 384
          ), // 384 W/(m*K) https://en.wikipedia.org/wiki/Thermal_conductivity_and_resistivity
          JoulesPerMeterKelvin(3449600),
        ) // c = 385 J/(kg * K), rho= 8.96 g/cm³: https://de.wikipedia.org/wiki/Kupfer => 3449600 J / (m³ * K)
      case CableMaterial.Aluminium =>
        (
          KelvinMetersPerWatt(
            1 / 237
          ), // therm conductivity of Aluminum = 237 W/(m*K) https://en.wikipedia.org/wiki/Thermal_conductivity_and_resistivity
          JoulesPerMeterKelvin(2420913.3),
        ) // c = 897 J/(kg * K), rho= 2.6989 g/cm³: https://de.wikipedia.org/wiki/Aluminium => 2420913.3 J / (m³ * K)
      case CableMaterial.XLPE =>
        (
          KelvinMetersPerWatt(3.5),
          JoulesPerMeterKelvin(2.4),
        ) // (Anders 1997 p. 400)
      case CableMaterial.PE =>
        (
          KelvinMetersPerWatt(3.5),
          JoulesPerMeterKelvin(2.4),
        ) // (Anders 1997 p. 400)
      case CableMaterial.PVC =>
        (
          KelvinMetersPerWatt(5.0),
          JoulesPerMeterKelvin(1.7),
        ) // (Anders 1997 p. 400)
      case CableMaterial.SemiCondScreen =>
        (
          KelvinMetersPerWatt(2.5), // TB880 p.28
          JoulesPerMeterKelvin(
            2.4
          ), // Same as adjacent dielectric material (Anders 1997 p. 400)
        )
      case CableMaterial.ScTape =>
        (
          KelvinMetersPerWatt(6.0), // TB880 p.28
          JoulesPerMeterKelvin(
            2.4
          ), // Same as adjacent dielectric material (Anders 1997 p. 400)
        )

      case CableMaterial.Lead =>
        (
          KelvinMetersPerWatt(
            1 / 35.0
          ), // 34,7 - 35,3 (pure) https://en.wikipedia.org/wiki/List_of_thermal_conductivities
          JoulesPerMeterKelvin(
            2.4
          ), // Heat Capacity of Steel 3.756 J/(cm³K)  https://en.wikipedia.org/wiki/Table_of_specific_heat_capacities
        )
      case CableMaterial.Steel =>
        (
          KelvinMetersPerWatt(
            1 / 45
          ), // 45 W/(m*K) https://en.wikipedia.org/wiki/Thermal_conductivity_and_resistivity
          JoulesPerMeterKelvin(
            3756000
          ), // 3.756 J/(cm³K)  https://en.wikipedia.org/wiki/Table_of_specific_heat_capacities
        ) // Source
      case CableMaterial.PolyPropylen =>
        (
          KelvinMetersPerWatt(
            6.0
          ), // TB880 p.28
          JoulesPerMeterKelvin(2.0), // FIXME but close to PPL in Anders p. 400
        )

      case _ => (KelvinMetersPerWatt(999), JoulesPerMeterKelvin(0)) // FIXME
    }

  /** Get electrical resistivity for screen material at reference conditions
    */
  def screenMaterialElectricalResistivity(
      mat: CableMaterial
  ): Resistivity =
    mat match {
      case CableMaterial.Copper =>
        squants.electro.OhmMeters(
          1.68e-8
        ) // https://en.wikipedia.org/wiki/Electrical_resistivity
      case CableMaterial.Aluminium =>
        squants.electro.OhmMeters(
          2.82e-8
        ) // https://en.wikipedia.org/wiki/Electrical_resistivity
      case CableMaterial.Steel =>
        squants.electro.OhmMeters(
          46.0e-8
        ) // Grain oriented electrical steel
      case CableMaterial.Lead =>
        squants.electro.OhmMeters(
          2.2e-7
        ) // https://en.wikipedia.org/wiki/Electrical_resistivity
      case other =>
        throw new IllegalArgumentException(s"Unknown material: $other")
    }
}
