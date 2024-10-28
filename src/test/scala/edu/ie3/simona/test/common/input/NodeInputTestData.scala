/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import java.util.UUID

import edu.ie3.datamodel.models.input.{NodeInput, OperatorInput}
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.util.quantities.PowerSystemUnits.PU
import tech.units.indriya.quantity.Quantities

/** //ToDo: Class Description
  *
  * @version 0.1
  * @since 23.06.20
  */
trait NodeInputTestData extends DefaultTestData {

  // 0.4 kV node input models
  protected val nodeInputNoSlackNs04KvA = new NodeInput(
    UUID.fromString("e5c1cde5-c161-4a4f-997f-fcf31fecbf57"),
    "TestNodeInputModel",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    Quantities.getQuantity(1d, PU),
    false,
    defaultLatLong,
    GermanVoltageLevelUtils.LV,
    -1,
  )
  protected val nodeInputNoSlackNs04KvB = new NodeInput(
    UUID.fromString("ad39d0b9-5ad6-4588-8d92-74c7d7de9ace"),
    "TestNodeInputModel",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    Quantities.getQuantity(1d, PU),
    false,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.LV,
    -1,
  )
  protected val nodeInputNoSlackNs04KvWrongVTarget = new NodeInput(
    UUID.fromString("7be605eb-fc14-4fdc-a580-a2c2a9abd5f7"),
    "TestNodeInputModel",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    Quantities.getQuantity(-10d, PU),
    false,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.LV,
    -1,
  )

  // 10 kV node input models
  protected val nodeInputNoSlackMs10Kv = new NodeInput(
    UUID.fromString("2e024451-0cae-4518-a75c-70fc673aca7a"),
    "nodeInputModelNoSlackMs10Kv",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    Quantities.getQuantity(1.0, PU),
    false,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.MV_10KV,
    -1,
  )

  // 20 kV node input models
  protected val nodeInputNoSlackMs20Kv =
    new NodeInput(
      UUID.fromString("12361495-7fca-44e8-b7e9-f89760ecdecf"),
      "nodeInputModelNoSlackMs20Kv",
      OperatorInput.NO_OPERATOR_ASSIGNED,
      defaultOperationTime,
      Quantities.getQuantity(1.0, PU),
      false,
      NodeInput.DEFAULT_GEO_POSITION,
      GermanVoltageLevelUtils.MV_20KV,
      1,
    )

  // 110 kV node input models
  protected val nodeInputNoSlackMs110Kv =
    new NodeInput(
      UUID.fromString("8e5c8a62-0ec7-411a-b2d8-ee8df5bcecbc"),
      "nodeInputModelNoSlackMs110Kv",
      OperatorInput.NO_OPERATOR_ASSIGNED,
      defaultOperationTime,
      Quantities.getQuantity(1.0, PU),
      false,
      NodeInput.DEFAULT_GEO_POSITION,
      GermanVoltageLevelUtils.HV,
      1,
    )
}
