/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.thermal.{DomesticHotWaterStorageInput, ThermalHouseInput}
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.util.quantities.PowerSystemUnits
import squants.energy.{KilowattHours, Megawatts}
import squants.thermal.Celsius
import tech.units.indriya.quantity.Quantities.getQuantity
import tech.units.indriya.unit.Units

import java.util.UUID

trait ThermalHouseTestData extends ThermalGridTestData with DefaultTestData {
  protected val thermalHouseInput: ThermalHouseInput = new ThermalHouseInput(
    UUID.randomUUID(),
    "Thermal House",
    thermalBusInput,
    getQuantity(0.05, StandardUnits.THERMAL_TRANSMISSION),
    getQuantity(15.0, StandardUnits.HEAT_CAPACITY),
    getQuantity(19d, Units.CELSIUS),
    getQuantity(21d, Units.CELSIUS),
    getQuantity(18d, Units.CELSIUS),
    "house",
    2.0,
  )

  protected val thermalHouse: ThermalHouse = ThermalHouse(thermalHouseInput)

  protected val domesticWaterStorageInput: DomesticHotWaterStorageInput =
    new DomesticHotWaterStorageInput(
      UUID.randomUUID(),
      "Domestic Hot Water Storage",
      thermalBusInput,
      getQuantity(100, StandardUnits.VOLUME),
      getQuantity(30, StandardUnits.TEMPERATURE),
      getQuantity(40, StandardUnits.TEMPERATURE),
      getQuantity(1.16, StandardUnits.SPECIFIC_HEAT_CAPACITY),
      getQuantity(11.0, PowerSystemUnits.KILOWATT),
    )

  protected val domesticWaterStorage: ThermalStorage = DomesticHotWaterStorage(domesticWaterStorageInput)

  protected val expectedHouseStartingState: ThermalHouseState =
    ThermalHouseState(
      -1L,
      Celsius(19d),
      Megawatts(0d),
    )

  protected val expectedDomesticHotWaterStorageStartingState: ThermalStorageState =
    ThermalStorageState(
      -1L,
      KilowattHours(0d),
      Megawatts(0d),
    )
}
