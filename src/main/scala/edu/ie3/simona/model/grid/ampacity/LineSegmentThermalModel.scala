/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.result.thermal.ThermalLineSegmentResult
import edu.ie3.simona.model.participant.ParticipantModel.ModelState
import edu.ie3.simona.model.thermal.ThermalThreshold
import edu.ie3.simona.service.Data
import edu.ie3.simona.service.Data.SecondaryData.{CurrentVoltage, WeatherData}
import edu.ie3.simona.util.Coordinate3D
import edu.ie3.simona.util.TickUtil.toDateTime
import edu.ie3.util.scala.quantities.*
import squants.space.{Length, Meters}
import squants.{ElectricCurrent, Temperature}
import squants.electro.ElectricPotential

import java.time.ZonedDateTime
import java.util.UUID
import java.lang.Double as JDouble
import tech.units.indriya.quantity.Quantities

/** A thermal model for a line segment
  *
  * @param uuid
  *   the element's uuid
  * @param id
  *   the element's human-readable id
  * @param thermalResistanceT1
  *   the element's human-readable id
  * @param thermalResistanceT2
  *   the element's human-readable id
  * @param thermalResistanceT3
  *   the element's human-readable id
  * @param thermalResistanceT4
  *   the element's human-readable id
  * @param upperBoundaryTemperature
  *   Upper boundary temperature
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
    thermalResistanceT4: ThermalResistivity, // FIXME DF Think about to remove this one here, since it needs to be calculated everytime new in case of changes in surrounding conditions (e.g. parallel cables and their current load)
    thermalCapacityCc: ThermalCapacitance,
    thermalCapacityCd: ThermalCapacitance,
    thermalCapacityCs: ThermalCapacitance,
    thermalCapacityCj: ThermalCapacitance,
    thermalCapacityCe: ThermalCapacitance,
    upperBoundaryTemperature: Temperature,
    soilType: SoilType,
) {}

object LineSegmentThermalModel {

  /** Derives the ground temperature at cable depth from [[WeatherData]] using
    * weighting of ground temperature level 3 and 4.
    *
    * @param depthCables
    *   The laying depth of the cable (negative below ground level).
    * @param weather
    *   The weather data of the segment's calculation point.
    * @return
    *   The ground temperature at cable depth.
    */
  private def groundTemperatureFromWeather(
      depthCables: Length,
      weather: WeatherData,
  ): Temperature = {
    val (weightTempLvl3, weightTempLvl4) =
      determineWeightsGroundTemperatures(depthCables)

    weather.groundTempLvl3.getOrElse(
      throw new IllegalArgumentException(
        s"Ground Temperature Level 3 expected but not found."
      )
    ) * weightTempLvl3 +
      weather.groundTempLvl4.getOrElse(
        throw new IllegalArgumentException(
          s"Ground Temperature Level 4 expected but not found."
        )
      ) * weightTempLvl4
  }

  /** Determine the weight of both temperature data at level 3 and level 4 for a
    * specific depth of the cable under analysis.
    *
    * @param depthCables
    *   The laying depth of the cables.
    * @return
    *   A tuple of the weights for the ground temperature at level 3 and level
    *   4, respectively.
    */
  private def determineWeightsGroundTemperatures(
      depthCables: Length
  ): (Double, Double) = {
    require(
      depthCables <= Meters(0),
      s"The cable laying depth must be a negative value (depth below ground), but was ${depthCables.toMeters} m.",
    )
    depthCables match {
      case x if x <= Meters(-1.95) => (0.0, 1.0)
      case x if x > Meters(-1.95) && x <= Meters(-0.64) =>
        val min = Meters(-0.64)
        val max = Meters(-1.95)
        val t = (x - min) / (max - min)
        (1.0 - t, t)
      case x if x > Meters(-0.64) => (1.0, 0.0)
      case _ =>
        throw new IllegalArgumentException(
          s"This case should not happen when handling input for LineSegmentThermalModel"
        )
    }

  }

  object LineModelThreshold {
    final case class LineModelTemperatureUpperBoundaryReached(
        override val tick: Long
    ) extends ThermalThreshold
  }
}
