/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import java.util.UUID

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.FixedFeedInInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.util.quantities.PowerSystemUnits.VOLTAMPERE
import tech.units.indriya.quantity.Quantities

/** //ToDo: Class Description
  *
  * @version 0.1
  * @since 23.06.20
  */
trait FixedFeedInputTestData extends NodeInputTestData {
  val fixedFeedInput =
    new FixedFeedInInput(
      UUID.fromString("9abe950d-362e-4efe-b686-500f84d8f368"),
      "testFixedFeed",
      OperatorInput.NO_OPERATOR_ASSIGNED,
      OperationTime.notLimited(),
      nodeInputNoSlackNs04KvA,
      new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
      null,
      Quantities.getQuantity(282.74d, VOLTAMPERE),
      0.95,
    )
}
