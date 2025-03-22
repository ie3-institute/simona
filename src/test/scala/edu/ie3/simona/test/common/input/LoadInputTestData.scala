/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.profile.BdewStandardLoadProfile
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.SimpleInputContainer
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits.{KILOWATTHOUR, VOLTAMPERE}
import tech.units.indriya.quantity.Quantities

import java.util.UUID

/** Exemplary instances of [[LoadInput]] to be used in tests
  */
trait LoadInputTestData extends NodeInputTestData {

  protected val loadInput = new LoadInput(
    UUID.fromString("4eeaf76a-ec17-4fc3-872d-34b7d6004b03"),
    "testLoad",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    nodeInputNoSlackNs04KvA,
    new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
    null,
    BdewStandardLoadProfile.H0,
    Quantities.getQuantity(3000d, KILOWATTHOUR),
    Quantities.getQuantity(282.74d, VOLTAMPERE),
    0.95,
  )

  protected val loadInputContainer: SimpleInputContainer[LoadInput] =
    SimpleInputContainer(loadInput)

  private val operationTime: OperationTime =
    OperationTime
      .builder()
      .withStart(TimeUtil.withDefaults.toZonedDateTime("2020-01-01T10:00:00Z"))
      .withEnd(TimeUtil.withDefaults.toZonedDateTime("2020-01-01T13:00:00Z"))
      .build()

  protected val loadInputWithLimitedOperationTime: LoadInput = loadInput
    .copy()
    .uuid(UUID.fromString("62f4b2cb-76ee-4900-a908-5073c6c51fc7"))
    .id("testLoadWithLimitedOperationTime")
    .operationTime(operationTime)
    .build()

  protected val loadInputContainerWithLimitedOperationTime
      : SimpleInputContainer[LoadInput] =
    SimpleInputContainer(loadInputWithLimitedOperationTime)
}
