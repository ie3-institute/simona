/*
 * © 2022-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.datamodel.models.input.container.ThermalGrid
import edu.ie3.datamodel.models.input.system.`type`.chargingpoint.ChargingPointTypeUtils
import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.datamodel.models.input.system.`type`.{
  HpTypeInput,
  StorageTypeInput,
}
import edu.ie3.datamodel.models.input.system.characteristic.{
  CosPhiFixed,
  ReactivePowerCharacteristic,
}
import edu.ie3.datamodel.models.input.system.{EvcsInput, HpInput, StorageInput}
import edu.ie3.datamodel.models.input.thermal.{
  ThermalHouseInput,
  ThermalStorageInput,
}
import edu.ie3.datamodel.models.input.{EmInput, OperatorInput}
import edu.ie3.datamodel.models.{OperationTime, StandardUnits}
import edu.ie3.simona.model.InputModelContainer.{
  SimpleInputContainer,
  WithHeatInputContainer,
}
import edu.ie3.util.quantities.PowerSystemUnits.*
import tech.units.indriya.quantity.Quantities

import java.util.UUID
import scala.jdk.CollectionConverters.SeqHasAsJava

trait EmInputTestData
    extends NodeInputTestData
    with PvInputTestData
    with LoadInputTestData
    with HpInputTestData {

  protected val emInput = new EmInput(
    UUID.randomUUID(),
    "Dummy_EmModel",
    new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
    OperationTime.notLimited(),
    "PRIORITIZED",
    null,
  )

  protected val evcsInput = new EvcsInput(
    UUID.randomUUID(),
    "Dummy_EvcsModel",
    new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
    OperationTime.notLimited(),
    nodeInputNoSlackNs04KvA,
    CosPhiFixed.CONSTANT_CHARACTERISTIC,
    emInput,
    ChargingPointTypeUtils.ChargingStationType2,
    2,
    0.95,
    EvcsLocationType.HOME,
    true,
  )

  protected val storageTypeInput = new StorageTypeInput(
    UUID.randomUUID(),
    "Dummy_StorageTypeInput",
    Quantities.getQuantity(4000d, EURO),
    Quantities.getQuantity(0.05d, EURO_PER_MEGAWATTHOUR),
    Quantities.getQuantity(15d, KILOWATTHOUR),
    Quantities.getQuantity(5d, KILOVOLTAMPERE),
    0.997,
    Quantities.getQuantity(5d, KILOWATT),
    Quantities.getQuantity(0.03, PU_PER_HOUR),
    Quantities.getQuantity(0.95, PU),
  )

  protected val storageInput = new StorageInput(
    UUID.randomUUID(),
    "Dummy_StorageInput",
    new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
    OperationTime.notLimited(),
    nodeInputNoSlackNs04KvA,
    CosPhiFixed.CONSTANT_CHARACTERISTIC,
    emInput,
    storageTypeInput,
  )

  protected val storageInputContainer = SimpleInputContainer(storageInput)

  protected val hpTypeInputEmIT = new HpTypeInput(
    UUID.fromString("a42f567a-f132-43d3-8b9f-2d5703673647"),
    "hp type",
    Quantities.getQuantity(0.0, StandardUnits.CAPEX),
    Quantities.getQuantity(0.0, StandardUnits.ENERGY_PRICE),
    Quantities.getQuantity(5.0, StandardUnits.ACTIVE_POWER_IN),
    0.97,
    Quantities.getQuantity(7.5, StandardUnits.ACTIVE_POWER_IN),
  )

  protected val hpInputModelEmIT = new HpInput(
    UUID.fromString("f2d207b4-d70b-4cf0-a140-4cc76df24695"),
    "test hp",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    nodeInputNoSlackNs04KvA,
    thermalBusInput,
    ReactivePowerCharacteristic.parse("cosPhiFixed:{(0.00,0.98)}"),
    emInput,
    hpTypeInputEmIT,
  )

  /* Set inner temperature of house a bit lower */
  val thermalHouseEmIT = new ThermalHouseInput(
    UUID.fromString("bdd682c0-7ad2-4b06-9751-fe3051a35825"),
    "thermal house",
    thermalBusInput,
    Quantities.getQuantity(0.15, StandardUnits.THERMAL_TRANSMISSION),
    Quantities.getQuantity(75, StandardUnits.HEAT_CAPACITY),
    Quantities.getQuantity(20.3, StandardUnits.TEMPERATURE),
    Quantities.getQuantity(22.0, StandardUnits.TEMPERATURE),
    Quantities.getQuantity(20.0, StandardUnits.TEMPERATURE),
    "house",
    2.0,
  )
  val thermalGridEmIT = new ThermalGrid(
    thermalBusInput,
    Seq(thermalHouseEmIT).asJava,
    Seq.empty[ThermalStorageInput].asJava,
    Seq[ThermalStorageInput](defaultDomesticHotWaterStorageInput).asJava,
  )

  protected val withHeatContainerEmIT =
    WithHeatInputContainer(hpInputModelEmIT, thermalGridEmIT)
}
