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
import edu.ie3.simona.service.Data.SecondaryData.WeatherData
import edu.ie3.simona.util.{Coordinate, Coordinate3D}
import edu.ie3.simona.util.TickUtil.toDateTime
import edu.ie3.util.scala.quantities.*
import squants.motion.MetersPerSecond
import squants.radio.WattsPerSquareMeter
import squants.space.{Length, Meters}
import squants.thermal.Celsius
import squants.{ElectricCurrent, Kelvin, Temperature}

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
    thermalResistanceT1: ThermalResistivity,
    thermalResistanceT2: ThermalResistivity,
    thermalResistanceT3: ThermalResistivity,
    thermalResistanceT4: ThermalResistivity, // FIXME Think about to remove this one here, since it needs to be calculated everytime new in case of changes in surrounding conditions (e.g. parallel cables and their current load)
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

    val point = cableSetup.pointA // FIXME averaging between pointA and pointB?

    val groundTemperature =
      getGroundTemperature(tick, point)

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

  def getGroundTemperature(
      tick: Long,
      point: Coordinate3D,
  ): Temperature = {
    val (weightTempLvl3, weightTempLvl4) =
      LineSegmentThermalModel.determineWeightsGroundTemperatures(
        Meters(point.height)
      )

    // FIXME
    val newData: WeatherData = WeatherData(
      WattsPerSquareMeter(0d),
      WattsPerSquareMeter(0d),
      Celsius(0),
      MetersPerSecond(0),
      Some(Celsius(20)),
      Some(Celsius(20)),
    )

    val groundTempCableDepth = newData.groundTempLvl3.getOrElse(
      throw new IllegalArgumentException(
        s"Ground Temperature Level 3 expected but not found."
      )
    ) * weightTempLvl3 +
      newData.groundTempLvl4.getOrElse(
        throw new IllegalArgumentException(
          s"Ground Temperature Level 4 expected but not found."
        )
      ) * weightTempLvl4

    groundTempCableDepth

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
  def createResults(
      state: LineState,
      dateTime: ZonedDateTime,
  ): Iterable[LineStateResult] = {
    Iterable(
      LineStateResult(
        dateTime,
        lineUuid,
        uuid,
        state.lineTemperatures.currentLineTemp1,
      )
    )
  }

}

final case class LineStateResult(
    time: ZonedDateTime,
    lineUuid: UUID,
    lineSegmentUuid: UUID,
    lineSegmentTemperature: Temperature,
)

object LineSegmentThermalModel {
  protected def temperatureTolerance: Temperature = Kelvin(0.01d)

  def apply(
      input: LineModel
  ): LineSegmentThermalModel = {
    val cableSetup = ???
    new LineSegmentThermalModel(
      UUID.randomUUID(),
      input.id,
      input.uuid,
      cableSetup,
      KelvinMetersPerWatt(1),
      KelvinMetersPerWatt(1),
      KelvinMetersPerWatt(1),
      KelvinMetersPerWatt(1),
      JoulesPerMeterKelvin(1),
      JoulesPerMeterKelvin(1),
      JoulesPerMeterKelvin(1),
      JoulesPerMeterKelvin(1),
      JoulesPerMeterKelvin(1),
      Celsius(90),
    )
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
    */
  final case class LineState(
      override val tick: Long,
      lastTick: Long,
      cableSetup: CableSetup,
      currentLineSegmentThermalModel: LineSegmentThermalModel,
      groundTemperature: Temperature,
      lineTemperatures: LineTemperatures,
  ) extends ModelState

  def initState(
      groundTemperature: Temperature,
      cableSetup: CableSetup,
      lineSegmentModel: LineSegmentThermalModel,
  ): LineState = {
    val t1 = calcThermalResistanceT1(cableSetup, cableSetup.voltage)

    val t2 = cableSetup.screenLayer.fold(KelvinMetersPerWatt(0))(layer =>
      calcThermalResistanceCableShells(
        layer.thermalResistivity,
        layer.innerDiameter,
        layer.outerDiameter,
      )
    ) // FIXME Check if this is correct

    val t3 = calcThermalResistanceT3(cableSetup)

    val t4 = calcThermalResistanceToSoilSingleCable(
      cableSetup.soilResistivity,
      cableSetup.depthCables,
      cableSetup.layersJackElements.last.outerDiameter,
    )

    val thermalCapacityCc = calcThermalCapacityCylindrical(
      cableSetup.conductor.thermalCapacitance,
      cableSetup.conductor.innerDiameter,
      cableSetup.conductor.outerDiameter,
    )

    val thermalCapacityCd1 =
      cableSetup.layersIsolationElements.foldLeft(JoulesPerMeterKelvin(0)) {
        (acc, layer) =>
          acc + calcThermalCapacityCylindrical(
            layer.thermalCapacitance,
            layer.innerDiameter,
            layer.outerDiameter,
          )
      }
    val thermalCapacityCd2 =
      cableSetup.layersFillerElements.foldLeft(JoulesPerMeterKelvin(0)) {
        (acc, layer) =>
          acc + calcThermalCapacityCylindrical(
            layer.thermalCapacitance,
            layer.innerDiameter,
            layer.outerDiameter,
          )
      }

    val thermalCapacityCd = thermalCapacityCd1 + thermalCapacityCd2

    val thermalCapacityCs =
      cableSetup.screenLayer.fold(JoulesPerMeterKelvin(0d))(layer =>
        calcThermalCapacityCylindrical(
          layer.thermalCapacitance,
          layer.innerDiameter,
          layer.outerDiameter,
        )
      )

    val thermalCapacityCj1 =
      cableSetup.layersArmorElements.foldLeft(JoulesPerMeterKelvin(0)) {
        (acc, layer) =>
          acc + calcThermalCapacityCylindrical(
            layer.thermalCapacitance,
            layer.innerDiameter,
            layer.outerDiameter,
          )
      }

    val thermalCapacityCj2 =
      cableSetup.layersJackElements.foldLeft(JoulesPerMeterKelvin(0)) {
        (acc, layer) =>
          acc + calcThermalCapacityCylindrical(
            layer.thermalCapacitance,
            layer.innerDiameter,
            layer.outerDiameter,
          )
      }

    val thermalCapacityCj = thermalCapacityCj1 + thermalCapacityCj2

    val thermalCapacityCe =
      cableSetup.soilCapacitance // FIXME Is this necessary or does it not matter since there is the "voltage source" of the ambient ground temp?

    val initialLineSegmentThermalModel = new LineSegmentThermalModel(
      lineSegmentModel.uuid,
      lineSegmentModel.id,
      lineSegmentModel.lineUuid,
      cableSetup,
      t1,
      t2,
      t3,
      t4,
      thermalCapacityCc,
      thermalCapacityCd,
      thermalCapacityCs,
      thermalCapacityCj,
      thermalCapacityCe,
      Celsius(90),
    )

    val initLineTemperatures = LineTemperatures(
      groundTemperature,
      groundTemperature,
      groundTemperature,
      groundTemperature,
      groundTemperature,
    )

    LineState(
      0L,
      -1L,
      cableSetup,
      initialLineSegmentThermalModel,
      groundTemperature,
      initLineTemperatures,
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
    depthCables match {
      case x if x >= Meters(1.94) => (0.0, 1.0)
      case x if x < Meters(1.94) && x > Meters(0.64) =>
        val min = Meters(0.64)
        val max = Meters(1.95)
        val t = (x - min) / (max - min)
        (1.0 - t, t)
      case x if x <= Meters(0.64) => (1.0, 0.0)
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
