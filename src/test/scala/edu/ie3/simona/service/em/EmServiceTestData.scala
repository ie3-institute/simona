/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.datamodel.models.input.system.`type`.StorageTypeInput
import edu.ie3.datamodel.models.input.system.characteristic.{
  CosPhiFixed,
  ReactivePowerCharacteristic,
}
import edu.ie3.datamodel.models.input.system.{LoadInput, StorageInput}
import edu.ie3.datamodel.models.input.{EmInput, OperatorInput}
import edu.ie3.datamodel.models.profile.BdewStandardLoadProfile
import edu.ie3.datamodel.models.{OperationTime, StandardUnits}
import edu.ie3.simona.model.InputModelContainer.SimpleInputContainer
import edu.ie3.simona.test.common.input.NodeInputTestData
import edu.ie3.util.quantities.PowerSystemUnits.*
import tech.units.indriya.quantity.Quantities

import java.util.UUID

trait EmServiceTestData extends NodeInputTestData {

  protected val emInput = new EmInput(
    UUID.randomUUID(),
    "Dummy_EmModel",
    new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
    OperationTime.notLimited(),
    "PROPORTIONAL",
    null,
  )

  protected val controlledEmInput = new EmInput(
    UUID.randomUUID(),
    "ControlledEmModel",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    "PROPORTIONAL",
    emInput,
  )

  protected val controlledEmInput2 = new EmInput(
    UUID.randomUUID(),
    "ControlledEmModel2",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    "PROPORTIONAL",
    emInput,
  )

  protected val loadInput = new LoadInput(
    UUID.fromString("4eeaf76a-ec17-4fc3-872d-34b7d6004b03"),
    "testLoad",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    nodeInputNoSlackNs04KvA,
    new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
    controlledEmInput2,
    BdewStandardLoadProfile.H0.getKey,
    Quantities.getQuantity(3000d, KILOWATTHOUR),
    Quantities.getQuantity(282.74d, VOLTAMPERE),
    0.95,
  )

  protected val loadInputContainer: SimpleInputContainer[LoadInput] =
    SimpleInputContainer(loadInput)

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
    controlledEmInput,
    storageTypeInput,
  )

  protected val storageInputContainer = SimpleInputContainer(storageInput)
}
