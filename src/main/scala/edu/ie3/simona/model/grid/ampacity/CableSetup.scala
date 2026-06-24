/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.datamodel.models.input.connector.`type`.CableMaterial
import edu.ie3.simona.util.Coordinate3D
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

/** Screen layer with specific parameters for conductor shielding.
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

/** @param uuid
  * @param id
  * @param conductor
  * @param layersIsolationElements
  * @param screenLayer
  * @param layersFillerElements
  * @param layersArmorElements
  * @param layersJackElements
  * @param layoutFormation
  * @param depthCables
  *   The laying depth of the cables.
  * @param distanceCables
  * @param soilResistivity
  * @param soilCapacitance
  * @param limitTemperature
  * @param voltage
  * @param frequency
  * @param electricResistance
  * @param skinEffectCoefficient
  * @param proximityEffectCoefficient
  * @param electricCapacitance
  * @param tanDelta
  * @param circulatingLossFactorScreen
  * @param eddyCurrentsLossFactorScreen
  */
final case class CableSetup(
    uuid: UUID,
    id: String,
    pointA: Coordinate3D,
    pointB: Coordinate3D,
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
