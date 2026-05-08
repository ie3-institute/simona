/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.StorageInput
import edu.ie3.datamodel.models.input.system.`type`.StorageTypeInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.simona.config.RuntimeConfig.StorageRuntimeConfig
import edu.ie3.simona.model.participant.storage.StorageModel
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.util.quantities.PowerSystemUnits.*
import tech.units.indriya.quantity.Quantities.getQuantity

import java.util.UUID

trait StorageInputTestData extends DefaultTestData with NodeInputTestData {

  protected def createTestModel(
      targetSoc: Option[Double] = Option.empty
  ): StorageModel =
    StorageModel
      .Factory(
        storageInput,
        StorageRuntimeConfig(targetSoc = targetSoc),
      )
      .create()

  protected val typeInput: StorageTypeInput = new StorageTypeInput(
    UUID.fromString("fbee4995-24dd-45e4-9c85-7d986fe99ff3"),
    "Test_StorageTypeInput",
    getQuantity(10000d, EURO),
    getQuantity(0.05d, EURO_PER_MEGAWATTHOUR),
    getQuantity(100d, KILOWATTHOUR),
    getQuantity(13d, KILOVOLTAMPERE),
    0.997,
    getQuantity(10d, KILOWATT),
    getQuantity(0.03, PU_PER_HOUR),
    getQuantity(0.9, PU),
  )

  protected val storageInput: StorageInput = new StorageInput(
    UUID.randomUUID(),
    "Test_StorageInput",
    new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
    OperationTime.notLimited(),
    nodeInputNoSlackNs04KvA,
    CosPhiFixed.CONSTANT_CHARACTERISTIC,
    null,
    typeInput,
  )

}
