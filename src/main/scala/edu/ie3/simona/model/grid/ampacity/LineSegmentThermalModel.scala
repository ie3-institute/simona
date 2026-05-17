/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.model.grid.LineModel
import edu.ie3.simona.model.grid.ampacity.LineSegmentThermalModel.{
  LineState,
  temperatureTolerance,
}
import edu.ie3.simona.model.grid.ampacity.LineThermalModelCalculations.*
import edu.ie3.simona.model.participant.ParticipantModel.ModelState
import edu.ie3.simona.model.thermal.ThermalThreshold
import edu.ie3.util.scala.quantities.{
  JoulesPerMeterKelvin,
  KelvinMetersPerWatt,
  ThermalCapacitance,
  ThermalResistivity,
}
import squants.space.Millimeters
import squants.thermal.Celsius
import squants.{ElectricCurrent, Kelvin, Power, Seconds, Temperature}

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
    thermalResistanceT1: ThermalResistivity,
    thermalResistanceT2: ThermalResistivity,
    thermalResistanceT3: ThermalResistivity,
    thermalResistanceT4: ThermalResistivity,
    thermalCapacityCc: ThermalCapacitance,
    thermalCapacityCd: ThermalCapacitance,
    thermalCapacityCs: ThermalCapacitance,
    thermalCapacityCj: ThermalCapacitance,
    thermalCapacityCe: ThermalCapacitance,
    upperBoundaryTemperature: Temperature,
) {

  /** Check if the temperature of a line element is higher than the allowed
    * maximum temperature.
    *
    * @param lineTemperature
    *   The temperature of the line element.
    * @param boundaryTemperature
    *   The applied boundary temperature to check against.
    * @return
    *   True, if inner temperature is too high.
    */
  def isLineTemperatureTooHigh(
      lineTemperature: Temperature,
      boundaryTemperature: Temperature = Celsius(90),
  ): Boolean =
    lineTemperature > (
      boundaryTemperature - temperatureTolerance
    )

  /** Update the current state of the line segment
    */
  def determineState(
      tick: Long,
      lastLineState: LineState,
      qDot: Power,
  ): LineState = {
    val duration = Seconds(tick - lastLineState.tick)
    val updatedLineTemperature = ???

    lastLineState.copy(
      tick = tick,
      lastTick = lastLineState.tick,
      currentLineTemp1 = updatedLineTemperature,
      currentLineTemp2 = updatedLineTemperature,
      currentLineTemp3 = updatedLineTemperature,
      currentLineTemp4 = updatedLineTemperature,
      currentLineTemp5 = updatedLineTemperature,
    )
  }

  /** Determine the next threshold, that will be reached.
    *
    * @param thermalHouseState
    *   The applicable state of thermalHouse until this tick.
    * @param qDot
    *   The thermal feed in to the thermal house.
    * @return
    *   The next threshold, that will be reached.
    */
  def determineNextThreshold(
      lineState: LineState,
      current: ElectricCurrent,
  ): Option[ThermalThreshold] = {
    ???
  }

  private def nextActivation(
      tick: Long
  ): Option[Long] = {
    ???
  }
}

object LineSegmentThermalModel {
  protected def temperatureTolerance: Temperature = Kelvin(0.01d)

  def apply(input: LineModel): LineSegmentThermalModel =
    new LineSegmentThermalModel(
      input.uuid,
      input.id,
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

  /** State of a thermal line segment model.
    *
    * @param tick
    *   Last tick of temperature change.
    * @param groundTemperature
    *   The current ground temperature.
    * @param currentLineTemp1
    *   The current temperature of the conductor.
    * @param currentLineTemp2
    *   The current temperature of the dielectric.
    * @param currentLineTemp3
    *   The current temperature of the filler.
    * @param currentLineTemp4
    *   The current temperature of the jack.
    * @param currentLineTemp5
    *   The current temperature of the soil.
    */
  final case class LineState(
      override val tick: Long,
      lastTick: Long,
      cableSetup: CableSetup,
      currentLineSegmentThermalModel: LineSegmentThermalModel,
      groundTemperature: Temperature,
      currentLineTemp1: Temperature,
      currentLineTemp2: Temperature,
      currentLineTemp3: Temperature,
      currentLineTemp4: Temperature,
      currentLineTemp5: Temperature,
  ) extends ModelState

  def startingState(
      groundTemperature: Temperature,
      cableSetup: CableSetup,
  ): LineState = {
    val t1 = calcThermalResistanceCableShells(
      cableSetup.dielectricResistivity,
      cableSetup.conductorDiameter,
      cableSetup.dielectricDiameter,
    )
    val t2 = calcThermalResistanceCableShells(
      cableSetup.fillerResistivity,
      cableSetup.dielectricDiameter,
      cableSetup.fillerDiameter,
    )
    val t3 = calcThermalResistanceCableShells(
      cableSetup.jackResistivity,
      cableSetup.sheathDiameter,
      cableSetup.jackDiameter,
    )

    val soilResistivity = KelvinMetersPerWatt(1.0) // FIXME

    val t4 = calcThermalResistanceToSoilSingleCable(
      soilResistivity,
      cableSetup.jackDiameter,
      Millimeters(1000), // FIXME
    )

    val thermalCapacityCc = calcThermalCapacityCylindrical(
      cableSetup.conductorCapacitance,
      Millimeters(0),
      cableSetup.conductorDiameter,
    )
    val thermalCapacityCd = calcThermalCapacityCylindrical(
      cableSetup.dielectricCapacitance,
      cableSetup.conductorDiameter,
      cableSetup.dielectricDiameter,
    )
    val thermalCapacityCs =
      calcThermalCapacityCylindrical(
        cableSetup.sheathCapacitance,
        cableSetup.fillerDiameter,
        cableSetup.sheathDiameter,
      )
    val thermalCapacityCj =
      calcThermalCapacityCylindrical(
        cableSetup.jackCapacitance,
        cableSetup.sheathDiameter,
        cableSetup.jackDiameter,
      )

    val thermalCapacityCe = JoulesPerMeterKelvin(5.0)

    val initialLineSegmentThermalModel = new LineSegmentThermalModel(
      UUID.randomUUID(),
      "initialLineSegmentThermalModel",
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

    LineState(
      0L,
      -1L,
      cableSetup,
      initialLineSegmentThermalModel,
      groundTemperature,
      groundTemperature,
      groundTemperature,
      groundTemperature,
      groundTemperature,
      groundTemperature,
    )
  }

  object LineModelThreshold {
    final case class LineModelTemperatureUpperBoundaryReached(
        override val tick: Long
    ) extends ThermalThreshold
  }
}
