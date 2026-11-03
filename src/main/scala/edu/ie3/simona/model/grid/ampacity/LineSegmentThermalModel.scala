/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.datamodel.models.input.SoilType
import edu.ie3.simona.model.grid.ampacity.LineSegmentThermalModel.SoilLayerInfo
import edu.ie3.simona.model.grid.ampacity.LineThermalModelCalculations.LineTemperatures
import edu.ie3.simona.model.participant.ParticipantModel.ModelState
import edu.ie3.simona.util.{Coordinate, Coordinate3D}
import edu.ie3.util.quantities.thermal.{
  JoulesPerMeterKelvin,
  ThermalCapacityPerLength,
  ThermalResistivity,
}
import squants.Temperature
import squants.electro.ElectricPotential
import squants.space.Length

import java.util.UUID

/** A thermal model for a line segment
  *
  * @param uuid
  *   the element's uuid
  * @param id
  *   the element's human-readable id
  * @param lineUuid
  *   the uuid of the line this segment belongs to
  * @param cableSetup
  *   the setup of the cable in this line segment
  * @param pointA
  *   the 3d coordinate of the segment's starting point
  * @param pointB
  *   the 3d coordinate of the segment's ending point
  * @param depthCables
  *   the laying depth of the cables below ground level (negative)
  * @param distanceCables
  *   the distance between the cables
  * @param thermalResistanceT1
  *   the thermal resistance T1 of the inner cable elements between conductor
  *   and screen, per unit of length
  * @param thermalResistanceT2
  *   the thermal resistance T2 of the cable elements between screen and the
  *   outer cable covering (filler and armor), per unit of length
  * @param thermalResistanceT3
  *   the thermal resistance T3 of the outer cable covering, per unit of length
  * @param thermalResistanceT4
  *   the thermal resistance T4 between the cable and its surrounding soil, per
  *   unit of length
  * @param thermalCapacityCc
  *   the thermal capacity of the conductor per unit of length
  * @param thermalCapacityCd
  *   the thermal capacity of the dielectric (isolation and filler) per unit of
  *   length
  * @param thermalCapacityCs
  *   the thermal capacity of the screen (sheath) per unit of length
  * @param thermalCapacityCj
  *   the thermal capacity of the armor and jack (outer covering) per unit of
  *   length
  * @param thermalCapacityCb
  *   the thermal capacity of the cable bed per unit of length (zero if no bed)
  * @param thermalCapacityCe
  *   the thermal capacity of the surrounding soil per unit of length
  * @param upperBoundaryTemperature
  *   Upper boundary temperature
  * @param soilType
  *   the type of the surrounding soil, outside an optional existing cable bed
  * @param calculationPoint
  *   the calculation point of this segment
  * @param parallelInfluence
  *   the parallel influence of neighbouring cables as a sequence of (uuid,
  *   influence factor)
  * @param cableBed
  *   the optional cable bed surrounding the cable
  */
final case class LineSegmentThermalModel(
    uuid: UUID,
    id: String,
    lineUuid: UUID,
    cableSetup: CableSetup,
    pointA: Coordinate3D,
    pointB: Coordinate3D,
    depthCables: Length,
    distanceCables: Length,
    thermalResistanceT1: ThermalResistivity,
    thermalResistanceT2: ThermalResistivity,
    thermalResistanceT3: ThermalResistivity,
    thermalResistanceT4: ThermalResistivity,
    thermalCapacityCc: ThermalCapacityPerLength,
    thermalCapacityCd: ThermalCapacityPerLength,
    thermalCapacityCs: ThermalCapacityPerLength,
    thermalCapacityCj: ThermalCapacityPerLength,
    thermalCapacityCb: ThermalCapacityPerLength = JoulesPerMeterKelvin(0d),
    thermalCapacityCe: ThermalCapacityPerLength,
    upperBoundaryTemperature: Temperature,
    soilType: SoilType,
    calculationPoint: Coordinate,
    parallelInfluence: Seq[(UUID, Double)] = Seq.empty,
    cableBed: Option[LineSegmentThermalModel.CableBed] = None,
    soilLayers: Seq[SoilLayerInfo] = Seq.empty,
) {}

object LineSegmentThermalModel {

  /** Homogeneous rectangular bedding around a supported cable formation. The
    * horizontal center is aligned with the formation center; its vertical
    * center is located at the deployment cable depth.
    */
  final case class CableBed(
      width: Length,
      height: Length,
      soilType: SoilType,
  )

  /** Describes a single horizontal soil layer in the ordered layer sequence.
    * Layers are ordered from the ground surface (upperDepth closest to 0)
    * downwards (lowerDepth most negative).
    *
    * @param soilType
    *   The soil type of this layer.
    * @param upperDepth
    *   The depth of the upper boundary of this layer (negative below surface).
    * @param lowerDepth
    *   The depth of the lower boundary of this layer (negative below surface).
    */
  final case class SoilLayerInfo(
      soilType: SoilType,
      upperDepth: Length,
      lowerDepth: Length,
  )

  /** State of a thermal line segment model.
    *
    * @param tick
    *   Current tick.
    * @param lastTick
    *   Last tick of temperature change.
    * @param cableSetup
    *   The setup of the cable in this line segment.
    * @param currentLineSegmentThermalModel
    *   The current LineSegmentThermalModel.
    * @param groundTemperature
    *   The current ground temperature.
    * @param lineTemperatures
    *   The current temperatures of the cable layers.
    * @param currentVoltage
    *   The current voltage at the cable, i.e. the average of the voltages at
    *   both connected nodes from the last power flow result.
    * @param dryZoneActive
    *   True if a dry zone is active around the cable.
    */
  final case class LineState(
      override val tick: Long,
      lastTick: Long,
      cableSetup: CableSetup,
      currentLineSegmentThermalModel: LineSegmentThermalModel,
      groundTemperature: Temperature,
      lineTemperatures: LineTemperatures,
      currentVoltage: ElectricPotential,
      dryZoneActive: Boolean,
  ) extends ModelState
}
