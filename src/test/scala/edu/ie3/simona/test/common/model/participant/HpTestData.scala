/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.model.participant

import edu.ie3.datamodel.models.input.container.ThermalGrid
import edu.ie3.datamodel.models.{OperationTime, StandardUnits}
import edu.ie3.datamodel.models.input.{NodeInput, OperatorInput}
import edu.ie3.datamodel.models.input.system.HpInput
import edu.ie3.datamodel.models.input.system.`type`.HpTypeInput
import edu.ie3.datamodel.models.input.system.characteristic.ReactivePowerCharacteristic
import edu.ie3.datamodel.models.input.thermal.{
  ThermalBusInput,
  ThermalHouseInput,
  ThermalStorageInput,
}
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.test.common.DefaultTestData
import tech.units.indriya.quantity.Quantities

import java.util.UUID
import scala.jdk.CollectionConverters.SeqHasAsJava

trait HpTestData extends DefaultTestData {
  protected val nodeInput = new NodeInput(
    UUID.fromString("d396cf12-5ede-41e0-b6c0-b8cadfdd5f13"),
    "NS node",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    Quantities.getQuantity(1d, StandardUnits.VOLTAGE_MAGNITUDE),
    false,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.LV,
    2,
  )

  protected val thermalBusInput = new ThermalBusInput(
    UUID.fromString("48fa6e8d-c07f-45cd-9ad7-094a1f2a7489"),
    "thermal bus",
  )

  protected val typeInput = new HpTypeInput(
    UUID.fromString("9802bf35-2a4e-4ff5-be9b-cd9e6a78dcd6"),
    "hp type",
    Quantities.getQuantity(0.0, StandardUnits.CAPEX),
    Quantities.getQuantity(0.0, StandardUnits.ENERGY_PRICE),
    Quantities.getQuantity(15.0, StandardUnits.ACTIVE_POWER_IN),
    0.97,
    Quantities.getQuantity(11.0, StandardUnits.ACTIVE_POWER_IN),
  )

  protected val hpInputModel = new HpInput(
    UUID.fromString("7832dea4-8703-4b37-8752-e67b86e957df"),
    "test hp",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    nodeInput,
    thermalBusInput,
    ReactivePowerCharacteristic.parse("cosPhiFixed:{(0.00,0.98)}"),
    typeInput,
  )

  protected val thermalHouse = new ThermalHouseInput(
    UUID.fromString("91940626-bdd0-41cf-96dd-47c94c86b20e"),
    "thermal house",
    thermalBusInput,
    Quantities.getQuantity(0.325, StandardUnits.THERMAL_TRANSMISSION),
    Quantities.getQuantity(75, StandardUnits.HEAT_CAPACITY),
    Quantities.getQuantity(21.0, StandardUnits.TEMPERATURE),
    Quantities.getQuantity(22.0, StandardUnits.TEMPERATURE),
    Quantities.getQuantity(20.0, StandardUnits.TEMPERATURE),
  )

  protected val thermalGrid = new ThermalGrid(
    thermalBusInput,
    Seq(thermalHouse).asJava,
    Seq.empty[ThermalStorageInput].asJava,
  )
}
