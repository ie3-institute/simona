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
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.util.quantities.PowerSystemUnits.*
import tech.units.indriya.quantity.Quantities

import java.util.UUID

trait StorageInputTestData extends DefaultTestData with NodeInputTestData {

  protected val storageTypeInput = new StorageTypeInput(
    UUID.fromString("fbee4995-24dd-45e4-9c85-7d986fe99ff3"),
    "Dummy_StorageTypeInput",
    Quantities.getQuantity(15000d, EURO),
    Quantities.getQuantity(0.05d, EURO_PER_MEGAWATTHOUR),
    Quantities.getQuantity(200d, KILOWATTHOUR),
    Quantities.getQuantity(13d, KILOVOLTAMPERE),
    0.997,
    Quantities.getQuantity(12.961, KILOWATT),
    Quantities.getQuantity(0.03, PU_PER_HOUR),
    Quantities.getQuantity(0.92, PU),
  )

  protected val storageInput = new StorageInput(
    UUID.randomUUID(),
    "Dummy_StorageInput",
    new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
    OperationTime.notLimited(),
    nodeInputNoSlackNs04KvA,
    CosPhiFixed.CONSTANT_CHARACTERISTIC,
    null,
    storageTypeInput,
  )

}
