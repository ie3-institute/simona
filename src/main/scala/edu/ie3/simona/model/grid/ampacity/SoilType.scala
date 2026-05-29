/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.util.scala.quantities.{SpecificHeatCapacity, ThermalResistivity}
import squants.Temperature

import java.util.UUID

/** Repräsentiert die physikalischen Eigenschaften eines Bodentyps.
  */
case class SoilType(
    uuid: UUID,
    id: String,
    thermalResistivityWet: ThermalResistivity,
    thermalResistivityDry: ThermalResistivity,
    specificHeatCapacity: SpecificHeatCapacity, // FIXME Check if required per volume or per weight
    criticalTemperature: Temperature,
) {

  /** Gibt die aktuelle Wärmeleitfähigkeit basierend auf der Bodentemperatur
    * zurück. Das ist essenziell für die iterative Berechnung der
    * Austrocknungszonen.
    */
  def currentThermalResistitivy(
      temperature: Temperature
  ): ThermalResistivity = {
    if temperature >= criticalTemperature then thermalResistivityDry
    else thermalResistivityWet
  }

}
