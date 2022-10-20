/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.`type`.StorageTypeInput
import edu.ie3.datamodel.models.input.system.`type`.chargingpoint.ChargingPointTypeUtils
import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.system.{EmInput, EvcsInput, StorageInput}
import edu.ie3.datamodel.models.{ControlStrategy, OperationTime}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.model.participant.load.{LoadModelBehaviour, LoadReference}
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.util.quantities.PowerSystemUnits._
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units._

import java.util.UUID

trait EmInputTestData
    extends NodeInputTestData
    with PvInputTestData
    with LoadInputTestData {

  protected val evcsInput = new EvcsInput(
    UUID.randomUUID(),
    "Dummy_EvcsModel",
    new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
    OperationTime.notLimited(),
    nodeInputNoSlackNs04KvA,
    CosPhiFixed.CONSTANT_CHARACTERISTIC,
    ChargingPointTypeUtils.ChargingStationType2,
    2,
    0.95,
    EvcsLocationType.HOME,
    true
  )

  protected val householdStorageTypeInput = new StorageTypeInput(
    UUID.randomUUID(),
    "Dummy_Household_StorageTypeInput",
    Quantities.getQuantity(100d, EURO),
    Quantities.getQuantity(101d, EURO_PER_MEGAWATTHOUR),
    Quantities.getQuantity(15d, KILOWATTHOUR),
    Quantities.getQuantity(5d, KILOVOLTAMPERE),
    0.997,
    Quantities.getQuantity(5d, KILOWATT),
    Quantities.getQuantity(0.03, PU_PER_HOUR),
    Quantities.getQuantity(0.95, PU),
    Quantities.getQuantity(20d, PERCENT),
    Quantities.getQuantity(50000d, HOUR),
    100000
  )

  protected val householdStorageInput = new StorageInput(
    UUID.randomUUID(),
    "Dummy_Household_StorageInput",
    new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
    OperationTime.notLimited(),
    nodeInputNoSlackNs04KvA,
    CosPhiFixed.CONSTANT_CHARACTERISTIC,
    householdStorageTypeInput
  )

  protected val emInput = new EmInput(
    UUID.randomUUID(),
    "Dummy_EmModel",
    new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
    OperationTime.notLimited(),
    nodeInputNoSlackNs04KvA,
    CosPhiFixed.CONSTANT_CHARACTERISTIC,
    Array.empty,
    ControlStrategy.DefaultControlStrategies.NO_CONTROL_STRATEGY // FIXME adapt once available
  )

  protected val simonaConfig: SimonaConfig =
    createSimonaConfig(
      LoadModelBehaviour.FIX,
      LoadReference.ActivePower(
        Quantities.getQuantity(0d, KILOWATT)
      )
    )

  private val configUtil = ConfigUtil.ParticipantConfigUtil(
    simonaConfig.simona.runtime.participant
  )

  protected val defaultOutputConfig: NotifierConfig =
    NotifierConfig(
      simonaConfig.simona.output.participant.defaultConfig.simulationResult,
      simonaConfig.simona.output.participant.defaultConfig.powerRequestReply
    )

  protected val modelConfig: SimonaConfig.EmRuntimeConfig =
    configUtil.getOrDefault[SimonaConfig.EmRuntimeConfig](
      emInput.getUuid
    )

}
