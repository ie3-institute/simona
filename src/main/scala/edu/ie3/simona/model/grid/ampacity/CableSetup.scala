/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.datamodel.models.input.connector.`type`.CableMaterial
import edu.ie3.util.scala.quantities.*
import squants.Temperature
import squants.electro.{ElectricPotential, Resistivity}
import squants.space.{Area, Length}
import squants.time.Frequency

import java.util.UUID

/** Represents a single layer of a cable.
  *
  * @param name
  *   Name of the layer
  * @param material
  *   Material of the layer
  * @param innerDiameter
  *   Inner diameter of the layer
  * @param outerDiameter
  *   Outer diameter of the layer
  * @param thermalResistivity
  *   Thermal resistivity of the layer material
  * @param thermalCapacitance
  *   Thermal capacitance per meter of the layer
  * @param area
  *   Optional cross-sectional area of the layer
  */
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

/** Represents the complete setup of a cable for ampacity calculations.
  *
  * @param uuid
  *   Unique identifier of the cable setup
  * @param id
  *   Identifier string of the cable setup
  * @param conductor
  *   The conductor layer of the cable
  * @param layersIsolationElements
  *   List of insulation layers
  * @param screenLayer
  *   Optional screen layer for conductor shielding
  * @param layersFillerElements
  *   List of filler layers
  * @param layersArmorElements
  *   List of armor layers
  * @param layersJackElements
  *   List of jacket layers
  * @param layoutFormation
  *   Layout formation of the cable
  * @param limitTemperature
  *   Maximum allowable conductor temperature
  * @param voltage
  *   Rated voltage of the cable
  * @param frequency
  *   Operating frequency
  * @param electricResistance
  *   Electrical resistance per unit length
  * @param skinEffectCoefficient
  *   Coefficient accounting for the skin effect
  * @param proximityEffectCoefficient
  *   Coefficient accounting for the proximity effect
  * @param electricCapacitance
  *   Specific electrical capacitance of the cable
  * @param tanDelta
  *   Dielectric loss tangent
  * @param circulatingLossFactorScreen
  *   Loss factor for circulating currents in the screen
  * @param eddyCurrentsLossFactorScreen
  *   Loss factor for eddy currents in the screen
  */
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
    limitTemperature: Temperature,
    voltage: ElectricPotential,
    frequency: Frequency,
    electricResistance: ElectricalResistancePerLength,
    skinEffectCoefficient: Double,
    proximityEffectCoefficient: Double,
    electricCapacitance: SpecificCapacitance,
    tanDelta: Double,
    circulatingLossFactorScreen: Double,
    eddyCurrentsLossFactorScreen: Double,
)
