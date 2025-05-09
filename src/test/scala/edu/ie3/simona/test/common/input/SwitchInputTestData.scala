/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import java.util.UUID

import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.connector.SwitchInput
import edu.ie3.simona.test.common.DefaultTestData

/** //ToDo: Class Description
  *
  * @version 0.1
  * @since 23.06.20
  */
trait SwitchInputTestData extends NodeInputTestData with DefaultTestData {

  protected val switchInput: SwitchInput = new SwitchInput(
    UUID.fromString("60b15606-cd88-4434-9f33-99fcf4fdafcc"),
    "SimpleTestSwitch",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    nodeInputNoSlackNs04KvA,
    nodeInputNoSlackNs04KvB,
    true,
  )

  protected val loopSwitchInput: SwitchInput = new SwitchInput(
    UUID.fromString("60b15606-cd88-4434-9f33-99fcf4fdafcc"),
    "SimpleTestSwitch",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    nodeInputNoSlackNs04KvA,
    nodeInputNoSlackNs04KvA,
    true,
  )

  protected val invalidSwitchInput: SwitchInput = new SwitchInput(
    UUID.fromString("a66e1c49-ca76-4f1e-9cce-a38da3ea7ace"),
    "SimpleTestSwitch",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    nodeInputNoSlackNs04KvA,
    nodeInputNoSlackMs10Kv,
    true,
  )
}
