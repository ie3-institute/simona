/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.thermal.ThermalHouseInput
import edu.ie3.simona.model.thermal.ThermalHouse.{
  ThermalHouseOperatingPoint,
  ThermalHouseState,
}
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import squants.thermal.Celsius
import tech.units.indriya.quantity.Quantities.getQuantity
import tech.units.indriya.unit.Units

import java.util.UUID

trait ThermalHouseTestData extends ThermalGridTestData {
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

  protected val expectedHouseStartingState: ThermalHouseState =
    ThermalHouseState(
      -1L,
      testGridAmbientTemperature,
      ThermalHouseOperatingPoint(zeroKW),
      Celsius(19d),
    )
}
