/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.model.grid.LineModel
import edu.ie3.simona.model.grid.ampacity.LineSegmentThermalModel.LineState
import edu.ie3.simona.model.grid.ampacity.LineThermalModelCalculations.*
import edu.ie3.simona.model.participant.ParticipantModel.ModelState
import edu.ie3.simona.model.thermal.ThermalThreshold
import edu.ie3.simona.service.Data
import edu.ie3.simona.service.Data.SecondaryData.{CurrentVoltage, WeatherData}
import edu.ie3.simona.util.Coordinate3D
import edu.ie3.simona.util.TickUtil.toDateTime
import edu.ie3.util.scala.quantities.*
import squants.motion.MetersPerSecond
import squants.space.{Length, Meters}
import squants.thermal.Celsius
import squants.{ElectricCurrent, Kelvin, Temperature}
import squants.electro.ElectricPotential

import java.time.ZonedDateTime
import java.util.UUID

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
    soilResistivity: ThermalResistivity,
    soilCapacitance: ThermalCapacitance,
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
) {

  /** Update the current state of the line segment
    */
  def determineState(
      tick: Long,
      lastLineState: LineState,
      lineCurrent: ElectricCurrent,
      simulationStart: ZonedDateTime,
  ): LineState = {

    val groundTemperature = lastLineState.groundTemperature

    val updatedLineTemperatures = createAndCalcRCNetworkMvCableShortDuration(
      tick,
      lastLineState,
      lineCurrent,
      groundTemperature,
    )

    val updatedLineState = lastLineState.copy(
      tick = tick,
      lastTick = lastLineState.tick,
      lineTemperatures = updatedLineTemperatures,
    )
    createResults(updatedLineState, tick.toDateTime(using simulationStart))

    updatedLineState
  }

  /** Handle incoming secondary data (e.g. weather, the current voltage at the
    * cable). This must be called before determineState for the weather- and
    * voltage-aware behaviour.
    *
    * @param state
    *   The current line state.
    * @param receivedData
    *   The received secondary data (e.g. weather and the current voltage).
    * @return
    *   The updated line state.
    */
  def handleInput(
      state: LineState,
      receivedData: Seq[Data],
  ): LineState = {

    val (voltage, weather) = receivedData.foldLeft(
      (Option.empty[ElectricPotential], Option.empty[WeatherData])
    ) {
      case ((voltage, weather), CurrentVoltage(_, receivedVoltage)) =>
        (voltage.orElse(Some(receivedVoltage)), weather)
      case ((voltage, weather), receivedWeather: WeatherData) =>
        (voltage, weather.orElse(Some(receivedWeather)))
      case (inputs, _) => inputs
    }

    val stateWithVoltage = voltage
      .map(receivedVoltage => state.copy(currentVoltage = receivedVoltage))
      .getOrElse(state)

    weather
      .map { newData =>
        val groundTemp = LineSegmentThermalModel.groundTemperatureFromWeather(
          depthCables,
          newData,
        )

        stateWithVoltage.copy(groundTemperature = groundTemp)
      }
      .getOrElse(stateWithVoltage)
  }

  /*
  /** Determine the next threshold, that will be reached.
   *
   * @param lineState
   *   State of a thermal line segment model.
   * @param electricCurrent
   *   The electric current of that line in this simulation step.
   * @return
   */
  def determineNextThreshold(
      lineState: LineState,
      electricCurrent: ElectricCurrent,
  ): Option[ThermalThreshold] = {
    ???
  }

  private def nextActivation(
      tick: Long
  ): Option[Long] = {
    ???
  }
   */
  /** Creates the results for the given state.
    *
    * @param state
    *   The current line state.
    * @param dateTime
    *   The date time of the current tick.
    * @return
    *   The results for the given state.
    */
  def createResults(
      state: LineState,
      dateTime: ZonedDateTime,
  ): Iterable[LineStateResult] = {
    Iterable(
      LineStateResult(
        dateTime,
        uuid,
        state.lineTemperatures.currentLineTemp1,
        state.groundTemperature,
      )
    )
  }

}

final case class LineStateResult(
    time: ZonedDateTime,
    lineSegmentUuid: UUID,
    lineSegmentTemperature: Temperature,
    groundTemperature: Temperature,
)

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
  def groundTemperatureFromWeather(
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
    */
  final case class LineState(
      override val tick: Long,
      lastTick: Long,
      cableSetup: CableSetup,
      currentLineSegmentThermalModel: LineSegmentThermalModel,
      groundTemperature: Temperature,
      lineTemperatures: LineTemperatures,
      currentVoltage: ElectricPotential,
  ) extends ModelState

  def initState(
      cableSetup: CableSetup,
      lineSegmentModel: LineSegmentThermalModel,
      initialGroundTemperature: Temperature,
  ): LineState = {

    val t1 = calcThermalResistanceT1(cableSetup, cableSetup.voltage)
    val t2 = calcThermalResistanceT2(cableSetup)
    val t3 = calcThermalResistanceT3(cableSetup)
    val t4 = calcThermalResistanceToSoilSingleCable(
      lineSegmentModel.soilResistivity,
      lineSegmentModel.depthCables,
      cableSetup.layersJackElements.last.outerDiameter,
    )

    val thermalCapacityCc = calcThermalCapacityCylindrical(
      cableSetup.conductor.thermalCapacitance,
      cableSetup.conductor.innerDiameter,
      cableSetup.conductor.outerDiameter,
    )

    val thermalCapacityCd1 =
      cableSetup.layersIsolationElements.foldLeft(
        JoulesPerCubicMeterKelvin(0)
      ) { (acc, layer) =>
        acc + calcThermalCapacityCylindrical(
          layer.thermalCapacitance,
          layer.innerDiameter,
          layer.outerDiameter,
        )
      }
    val thermalCapacityCd2 =
      cableSetup.layersFillerElements.foldLeft(JoulesPerCubicMeterKelvin(0)) {
        (acc, layer) =>
          acc + calcThermalCapacityCylindrical(
            layer.thermalCapacitance,
            layer.innerDiameter,
            layer.outerDiameter,
          )
      }

    val thermalCapacityCd = thermalCapacityCd1 + thermalCapacityCd2

    val thermalCapacityCs =
      cableSetup.screenLayer.fold(JoulesPerCubicMeterKelvin(0d))(layer =>
        calcThermalCapacityCylindrical(
          layer.thermalCapacitance,
          layer.innerDiameter,
          layer.outerDiameter,
        )
      )

    val thermalCapacityCj1 =
      cableSetup.layersArmorElements.foldLeft(JoulesPerCubicMeterKelvin(0)) {
        (acc, layer) =>
          acc + calcThermalCapacityCylindrical(
            layer.thermalCapacitance,
            layer.innerDiameter,
            layer.outerDiameter,
          )
      }

    val thermalCapacityCj2 =
      cableSetup.layersJackElements.foldLeft(JoulesPerCubicMeterKelvin(0)) {
        (acc, layer) =>
          acc + calcThermalCapacityCylindrical(
            layer.thermalCapacitance,
            layer.innerDiameter,
            layer.outerDiameter,
          )
      }

    val thermalCapacityCj = thermalCapacityCj1 + thermalCapacityCj2

    val thermalCapacityCe =
      lineSegmentModel.soilCapacitance // FIXME DF Is this necessary or does it not matter since there is the "voltage source" of the ambient ground temp?

    val initialLineSegmentThermalModel = new LineSegmentThermalModel(
      lineSegmentModel.uuid,
      lineSegmentModel.id,
      lineSegmentModel.lineUuid,
      cableSetup,
      lineSegmentModel.pointA,
      lineSegmentModel.pointB,
      lineSegmentModel.depthCables,
      lineSegmentModel.distanceCables,
      lineSegmentModel.soilResistivity,
      lineSegmentModel.soilCapacitance,
      t1,
      t2,
      t3,
      t4,
      thermalCapacityCc,
      thermalCapacityCd,
      thermalCapacityCs,
      thermalCapacityCj,
      thermalCapacityCe,
      lineSegmentModel.upperBoundaryTemperature,
    )

    val initLineTemperatures = LineTemperatures(
      initialGroundTemperature,
      initialGroundTemperature,
      initialGroundTemperature,
      initialGroundTemperature,
      initialGroundTemperature,
    )

    LineState(
      0L,
      -1L,
      cableSetup,
      initialLineSegmentThermalModel,
      initialGroundTemperature,
      initLineTemperatures,
      cableSetup.voltage,
    )
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
  def determineWeightsGroundTemperatures(
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
