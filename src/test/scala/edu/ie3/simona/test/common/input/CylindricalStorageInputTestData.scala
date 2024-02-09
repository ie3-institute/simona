/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.thermal.{
  CylindricalStorageInput,
  ThermalBusInput,
}
import tech.units.indriya.quantity.Quantities.getQuantity

import java.util.UUID

trait CylindricalStorageInputTestData {

  protected val csInputModel = new CylindricalStorageInput(
    UUID.randomUUID(),
    "ThermalStorage",
    new ThermalBusInput(UUID.randomUUID(), "ThermalBus"),
    getQuantity(100, StandardUnits.VOLUME),
    getQuantity(20, StandardUnits.VOLUME),
    getQuantity(30, StandardUnits.TEMPERATURE),
    getQuantity(40, StandardUnits.TEMPERATURE),
    getQuantity(1.15, StandardUnits.SPECIFIC_HEAT_CAPACITY),
  )
}
