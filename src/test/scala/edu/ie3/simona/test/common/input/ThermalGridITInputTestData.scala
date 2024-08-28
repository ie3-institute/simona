/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.datamodel.models.input.system.HpInput
import edu.ie3.datamodel.models.input.system.`type`.HpTypeInput
import edu.ie3.datamodel.models.input.system.characteristic.ReactivePowerCharacteristic
import edu.ie3.datamodel.models.input.thermal.{
  DomesticHotWaterStorageInput,
  ThermalHouseInput,
  ThermalStorageInput,
}
import edu.ie3.datamodel.models.input.{EmInput, OperatorInput, container}
import edu.ie3.datamodel.models.{OperationTime, StandardUnits}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.model.participant.load.{LoadModelBehaviour, LoadReference}
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.util.quantities.PowerSystemUnits
import squants.energy.Kilowatts
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.util.UUID
import scala.jdk.CollectionConverters.SeqHasAsJava

trait ThermalGridITInputTestData
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

  protected val simonaConfig: SimonaConfig =
    createSimonaConfig(
      LoadModelBehaviour.FIX,
      LoadReference.ActivePower(
        Kilowatts(0.0)
      ),
    )

  private val configUtil = ConfigUtil.ParticipantConfigUtil(
    simonaConfig.simona.runtime.participant
  )

  protected val defaultOutputConfig: NotifierConfig =
    NotifierConfig(
      simonaConfig.simona.output.participant.defaultConfig.simulationResult,
      simonaConfig.simona.output.participant.defaultConfig.powerRequestReply,
      simonaConfig.simona.output.participant.defaultConfig.flexResult,
    )

  protected val modelConfig: SimonaConfig.EmRuntimeConfig =
    configUtil.getOrDefault[SimonaConfig.EmRuntimeConfig](
      emInput.getUuid
    )

  protected val adaptedTypeInput = new HpTypeInput(
    UUID.fromString("8ca556f8-58d1-4ec2-9c20-d163c55a1e2f"),
    "hp type",
    Quantities.getQuantity(10000d, PowerSystemUnits.EURO),
    Quantities.getQuantity(200d, PowerSystemUnits.EURO_PER_MEGAWATTHOUR),
    Quantities.getQuantity(5.0, StandardUnits.ACTIVE_POWER_IN),
    0.97,
    Quantities.getQuantity(7.5, StandardUnits.ACTIVE_POWER_IN),
  )

  protected val adaptedHpInputModel = new HpInput(
    UUID.fromString("101e6598-8260-41f3-9c70-4f1ba4797a19"),
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
  val adaptedThermalHouseIT = new ThermalHouseInput(
    UUID.fromString("64a27dea-ffcd-4100-9619-86f5212500a9"),
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

  protected val littleDomesticHotWaterStorageInput =
    new DomesticHotWaterStorageInput(
      UUID.fromString("e5997094-958a-486a-b4ea-863bf6cf42ec"),
      "domestic hot water storage to storage less than demand of one day for one person",
      OperatorInput.NO_OPERATOR_ASSIGNED,
      OperationTime.notLimited(),
      thermalBusInput,
      Quantities.getQuantity(28.7, Units.LITRE),
      Quantities.getQuantity(55.0, StandardUnits.TEMPERATURE),
      Quantities.getQuantity(10.0, StandardUnits.TEMPERATURE),
      Quantities.getQuantity(1.16, StandardUnits.SPECIFIC_HEAT_CAPACITY),
      Quantities.getQuantity(11.0, PowerSystemUnits.KILOWATT),
    )

  protected val thermalGridForThermalGridIT = new container.ThermalGrid(
    thermalBusInput,
    Seq(typicalThermalHouse).asJava,
    Seq[ThermalStorageInput](typicalThermalStorage).asJava,
    Seq[ThermalStorageInput](littleDomesticHotWaterStorageInput).asJava,
  )
}
