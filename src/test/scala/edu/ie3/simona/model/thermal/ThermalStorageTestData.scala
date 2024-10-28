/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.thermal.{
  CylindricalStorageInput,
  ThermalBusInput,
}
import tech.units.indriya.quantity.Quantities.getQuantity

import java.util.UUID

trait ThermalStorageTestData extends ThermalGridTestData {
  protected val thermalStorageInput: CylindricalStorageInput =
    new CylindricalStorageInput(
      UUID.randomUUID(),
      "ThermalStorage",
      new ThermalBusInput(
        UUID.fromString("ad2db5ab-8f90-4bc1-aa2c-30b31b843ab2"),
        "TestThermalBus",
      ),
      getQuantity(100, StandardUnits.VOLUME),
      getQuantity(0, StandardUnits.VOLUME),
      getQuantity(30, StandardUnits.TEMPERATURE),
      getQuantity(40, StandardUnits.TEMPERATURE),
      getQuantity(1.15, StandardUnits.SPECIFIC_HEAT_CAPACITY),
    )

  protected val thermalStorage: CylindricalThermalStorage =
    CylindricalThermalStorage(thermalStorageInput)

  protected val expectedStorageStartingState
      : ThermalStorage.ThermalStorageState = thermalStorage.startingState
}
