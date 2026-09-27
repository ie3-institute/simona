/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.util.scala.quantities.{ThermalCapacitance, ThermalResistivity}
import squants.Temperature

import java.util.UUID

/** Represents the physical properties of a soil type.
  * @param uuid
  *   the element's unique identifier
  * @param id
  *   the element's human-readable id
  * @param thermalResistivityWet
  * @param thermalResistivityDry
  * @param specificHeatCapacity
  * @param criticalTemperatureDifference
  *   The over-temperature at the cable surface above the undisturbed soil
  *   temperature at which the soil transitions from the wet to the dry state.
  */
case class SoilType(
    uuid: UUID,
    id: String,
    thermalResistivityWet: ThermalResistivity,
    thermalResistivityDry: ThermalResistivity,
    specificHeatCapacity: ThermalCapacitance, // FIXME DF Check if required per volume or per weight
    criticalTemperatureDifference: Temperature,
) {

  /** Returns the current thermal conductivity based on the over-temperature of
    * the cable surface relative to the undisturbed soil temperature. This is
    * essential for the iterative calculation of the drying zones.
    *
    * @param temperatureDifference
    *   The difference between the cable outer surface temperature and the
    *   undisturbed soil temperature.
    * @return
    *   The dry thermal resistivity if the critical temperature difference is
    *   reached or exceeded, otherwise the wet thermal resistivity.
    */
  def currentThermalResistivity(
      temperatureDifference: Temperature
  ): ThermalResistivity = {
    if temperatureDifference >= criticalTemperatureDifference then
      thermalResistivityDry
    else thermalResistivityWet
  }

}
