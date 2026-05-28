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
import edu.ie3.simona.service.Data
import edu.ie3.simona.service.Data.SecondaryData.WeatherData
import edu.ie3.util.scala.quantities.{
  JoulesPerMeterKelvin,
  KelvinMetersPerWatt,
  ThermalCapacitance,
  ThermalResistivity,
}
import squants.thermal.Celsius
import squants.{ElectricCurrent, Kelvin, Temperature}

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
      lineCurrent: ElectricCurrent,
  ): LineState = {

    val updatedLineTemperatures = createAndCalcRCNetworkMvCableShortDuration(
      tick,
      lastLineState,
      lineCurrent,
    )

    lastLineState.copy(
      tick = tick,
      lastTick = lastLineState.tick,
      lineTemperatures = updatedLineTemperatures,
    )
  }

  def handleInput(
      state: LineState,
      receivedData: Seq[Data],
  ): LineState = {

    val depthCables = state.cableSetup.depthCables

    receivedData
      .collectFirst { case weatherData: WeatherData =>
        weatherData
      }
      .map(newData =>
        val groundTempCableDepth = newData.groundTempLvl3.getOrElse(
          throw new IllegalArgumentException(
            s"Ground Temperature Level 1 expected but not found."
          )
        ) * 0.5 +
          newData.groundTempLvl4.getOrElse(
            throw new IllegalArgumentException(
              s"Ground Temperature Level 2 expected but not found."
            )
          ) * 0.5
        // FIXME adapt calculation depending on cable depth

        state.copy(groundTemperature = groundTempCableDepth)
      )
      .getOrElse(state)
  }

  /** Determine the next threshold, that will be reached.
    *
    * @param lineState
    *
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

  def startingState(
      groundTemperature: Temperature,
      cableSetup: CableSetup,
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

  object LineModelThreshold {
    final case class LineModelTemperatureUpperBoundaryReached(
        override val tick: Long
    ) extends ThermalThreshold
  }
}
