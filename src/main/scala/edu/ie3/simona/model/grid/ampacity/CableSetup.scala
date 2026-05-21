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
import squants.Temperature
import squants.electro.{Capacitance, ElectricPotential, ElectricalResistance}
import squants.space.{Area, Length}

import java.util.UUID

final case class Layer(
    material: CableMaterial,
    innerDiameter: Length,
    outerDiameter: Length,
    thermalResistivity: ThermalResistivity,
    thermalCapacitance: ThermalCapacitance,
    area: Option[Area],
)

final case class CableSetup(
    uuid: UUID,
    id: String,
    conductor: Layer,
    conductorScreen: Option[Layer],
    dielectric: Layer,
    insulationScreen: Option[Layer],
    filler: Option[Layer],
    screenTape: Option[Layer],
    screen: Option[Layer],
    jackTape: Option[Layer],
    jack: Option[Layer],
    outerCover: Option[Layer],
    layoutFormation: String,
    depthCables: Length,
    distanceCables: Length,
    soilResistivity: ThermalResistivity,
    soilCapacitance: ThermalCapacitance,
    limitTemperature: Temperature,
    voltage: ElectricPotential,
    electricResistance: ElectricalResistance,
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
      // c = 385 J/(kg * K), rho= 8.96 g/cm³: https://de.wikipedia.org/wiki/Kupfer => 3449600 J / (m³ * K)
      // therm conductivity of Copper = 384 W/(m*K) https://en.wikipedia.org/wiki/Thermal_conductivity_and_resistivity
      case CableMaterial.Copper =>
        (KelvinMetersPerWatt(1 / 384), JoulesPerMeterKelvin(3449600))
      // c = 897 J/(kg * K), rho= 2.6989 g/cm³: https://de.wikipedia.org/wiki/Aluminium => 2420913.3 J / (m³ * K)
      // therm conductivity of Aluminum = 237 W/(m*K) https://en.wikipedia.org/wiki/Thermal_conductivity_and_resistivity
      case CableMaterial.Aluminium =>
        (KelvinMetersPerWatt(1 / 237), JoulesPerMeterKelvin(2420913.3))
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
          KelvinMetersPerWatt(2.5),
          JoulesPerMeterKelvin(2.4),
        ) // TherRes: TB880 p.28, TherCapa: Same as adjacent dielectric material (Anders 1997 p. 400)
      case CableMaterial.ScTape =>
        (
          KelvinMetersPerWatt(6.0),
          JoulesPerMeterKelvin(2.4),
        ) // TherRes: TB880 p.28 TherCapa: Same as adjacent dielectric material (Anders 1997 p. 400)
      case _ => (KelvinMetersPerWatt(999), JoulesPerMeterKelvin(0)) // FIXME
    }
}
