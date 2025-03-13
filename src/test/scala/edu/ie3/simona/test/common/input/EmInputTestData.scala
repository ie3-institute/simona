/*
 * © 2022. TU Dortmund University,
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
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.{
  SimpleInputContainer,
  WithHeatInputContainer,
}
import edu.ie3.simona.config.RuntimeConfig.EmRuntimeConfig
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.util.quantities.PowerSystemUnits._
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

  protected val householdStorageTypeInput = new StorageTypeInput(
    UUID.randomUUID(),
    "Dummy_Household_StorageTypeInput",
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
    "Dummy_Household_StorageInput",
    new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
    OperationTime.notLimited(),
    nodeInputNoSlackNs04KvA,
    CosPhiFixed.CONSTANT_CHARACTERISTIC,
    emInput,
    householdStorageTypeInput,
  )

  protected val storageInputContainer = SimpleInputContainer(storageInput)

  protected val simonaConfig: SimonaConfig = createSimonaConfig()

  private val configUtil = ConfigUtil.EmConfigUtil(
    simonaConfig.simona.runtime.em
  )

  protected val defaultOutputConfig: NotifierConfig =
    NotifierConfig(
      simonaConfig.simona.output.participant.defaultConfig.simulationResult,
      simonaConfig.simona.output.participant.defaultConfig.powerRequestReply,
      simonaConfig.simona.output.participant.defaultConfig.flexResult,
    )

  protected val modelConfig: EmRuntimeConfig =
    configUtil.getOrDefault(emInput.getUuid)

  protected val adaptedTypeInput = new HpTypeInput(
    UUID.fromString("9802bf35-2a4e-4ff5-be9b-cd9e6a78dcd6"),
    "hp type",
    Quantities.getQuantity(0.0, StandardUnits.CAPEX),
    Quantities.getQuantity(0.0, StandardUnits.ENERGY_PRICE),
    Quantities.getQuantity(5.0, StandardUnits.ACTIVE_POWER_IN),
    0.97,
    Quantities.getQuantity(7.5, StandardUnits.ACTIVE_POWER_IN),
  )

  protected val adaptedHpInputModel = new HpInput(
    UUID.fromString("7832dea4-8703-4b37-8752-e67b86e957df"),
    "test hp",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    nodeInputNoSlackNs04KvA,
    thermalBusInput,
    ReactivePowerCharacteristic.parse("cosPhiFixed:{(0.00,0.98)}"),
    emInput,
    adaptedTypeInput,
  )

  /* Set inner temperature of house a bit lower */
  val adaptedThermalHouse = new ThermalHouseInput(
    UUID.fromString("91940626-bdd0-41cf-96dd-47c94c86b20e"),
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
  val adaptedThermalGrid = new ThermalGrid(
    thermalBusInput,
    Seq(adaptedThermalHouse).asJava,
    Seq.empty[ThermalStorageInput].asJava,
    Seq.empty[ThermalStorageInput].asJava,
  )
}
