/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import java.util.UUID
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.profile.BdewStandardLoadProfile
import edu.ie3.util.quantities.PowerSystemUnits.{KILOWATTHOUR, VOLTAMPERE}
import tech.units.indriya.quantity.Quantities

/** //ToDo: Class Description
  *
  * @version 0.1
  * @since 23.06.20
  */
trait LoadInputTestData extends NodeInputTestData {
  val loadInput =
    new LoadInput(
      UUID.fromString("4eeaf76a-ec17-4fc3-872d-34b7d6004b03"),
      "testLoad",
      OperatorInput.NO_OPERATOR_ASSIGNED,
      OperationTime.notLimited(),
      nodeInputNoSlackNs04KvA,
      new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
      BdewStandardLoadProfile.H0,
      false,
      Quantities.getQuantity(3000d, KILOWATTHOUR),
      Quantities.getQuantity(282.74d, VOLTAMPERE),
      0.95
    )
}
