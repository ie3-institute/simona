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
import edu.ie3.util.scala.quantities.{KelvinMetersPerWatt, ThermalResistivity}
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

  /** Update the current state of the house.
    *
    * @param tick
    *   The tick that the houseState should be updated to.
    * @param lastThermalHouseState
    *   The applicable state of thermalHouse until this tick.
    * @param qDot
    *   The thermal feed in to the thermal house.
    * @return
    *   Updated state of the instance.
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
      lineTemperature = updatedLineTemperature,
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
      Celsius(90),
    )

  /** State of a thermal line segment model.
    *
    * @param tick
    *   Last tick of temperature change.
    * @param groundTemperature
    *   The current ground temperature.
    * @param lineTemperature
    *   The temperature of the line element.
    */
  final case class LineState(
      override val tick: Long,
      currentLineSegmentThermalMode: LineSegmentThermalModel,
      groundTemperature: Temperature,
      lineTemperature: Temperature,
  ) extends ModelState

  def startingState(
      groundTemperature: Temperature
  ): LineState = {
    val cableMaterialResistivity = KelvinMetersPerWatt(4)
    val cableGeoA = Millimeters(10)
    val cableGeoB = Millimeters(12)
    val t1 = calcThermalResistanceCableShells(
      cableMaterialResistivity,
      cableGeoA,
      cableGeoB,
    )
    val t4 = calcThermalResistanceToSoil(
      cableMaterialResistivity,
      cableGeoA,
      cableGeoB,
    )
    val initialLineSegmentThermalModel = new LineSegmentThermalModel(
      UUID.randomUUID(),
      "initialLineSegmentThermalModel",
      t1,
      t1,
      t1,
      t4,
      Celsius(90),
    )

    LineState(
      0L,
      initialLineSegmentThermalModel,
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
