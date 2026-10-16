/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.datamodel.models.input.container.SoilType
import tech.units.indriya.ComparableQuantity
import javax.measure.quantity.Temperature
import edu.ie3.util.quantities.interfaces.ThermalResistivity

/** Small extension providing functionality previously available in the PSDM
  * SoilType class. The method mirrors the semantics described in the original
  * Java implementation: it returns the dry thermal resistivity when the
  * supplied over-temperature is greater than or equal to the soil's critical
  * temperature difference, otherwise the wet thermal resistivity.
  */
object SoilTypeExtensions {

  extension (soilType: SoilType) {

    def currentThermalResistivity(
        temperatureDifference: ComparableQuantity[Temperature]
    ): ComparableQuantity[ThermalResistivity] = {
      if temperatureDifference == null then {
        throw new IllegalArgumentException(
          "temperatureDifference must not be null"
        )
      }

      val criticalTemperatureDifference =
        soilType.getCriticalTemperatureDifference
      if criticalTemperatureDifference == null then {
        throw new IllegalStateException(
          "Cannot determine current thermal resistivity: criticalTemperatureDifference is not set for SoilType '" +
            soilType.getUuid + "'."
        )
      }

      if temperatureDifference.compareTo(criticalTemperatureDifference) >= 0
      then soilType.getThermalResistivityDry
      else soilType.getThermalResistivityWet
    }
  }
}
