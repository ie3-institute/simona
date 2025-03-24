/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.datamodel.models.input.{NodeInput, OperatorInput}
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.util.quantities.PowerSystemUnits.PU
import org.locationtech.jts.geom.{Coordinate, GeometryFactory}
import tech.units.indriya.quantity.Quantities

import java.util.UUID

/** Default values for NodeInput's to be used in tests. Should be extended as
  * needed.
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

  // PvModelITSpec node input models
  val geometryFactory = new GeometryFactory()

  protected val nodeInputPvModel1 =
    new NodeInput(
      UUID.fromString("19a4861e-cbf5-4dcc-bc1d-98611fda6eb5"),
      "luebeck_1",
      OperatorInput.NO_OPERATOR_ASSIGNED,
      defaultOperationTime,
      Quantities.getQuantity(1.0, PU),
      true,
      geometryFactory.createPoint(new Coordinate(10.3837, 53.9209)),
      GermanVoltageLevelUtils.MV_10KV,
      3,
    )

  protected val nodeInputPvModel2 = new NodeInput(
    UUID.fromString("9a2524f1-3639-4e90-a547-81a259712f8c"),
    "hannover_2",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    Quantities.getQuantity(1d, PU),
    true,
    geometryFactory.createPoint(new Coordinate(9.7342, 52.3393)),
    GermanVoltageLevelUtils.MV_10KV,
    2,
  )

  protected val nodeInputPvModel3 = new NodeInput(
    UUID.fromString("022a94c6-2d60-4400-875c-ab9db1ae2736"),
    "hannover_1",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    Quantities.getQuantity(1d, PU),
    true,
    geometryFactory.createPoint(new Coordinate(10.1991, 52.1861)),
    GermanVoltageLevelUtils.MV_10KV,
    1,
  )

  protected val nodeInputPvModel4 = new NodeInput(
    UUID.fromString("9354b02c-a4a9-4e9d-905a-e48110b04d88"),
    "luebeck_2",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    Quantities.getQuantity(1d, PU),
    true,
    geometryFactory.createPoint(new Coordinate(10.8245, 53.6567)),
    GermanVoltageLevelUtils.MV_10KV,
    4,
  )

  protected val nodeInputPvModel5: NodeInput = new NodeInput(
    UUID.fromString("393eb0e3-7873-4d51-a830-2f1d98ff5a60"),
    "hannover_3",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    Quantities.getQuantity(1d, PU),
    true,
    geometryFactory.createPoint(new Coordinate(10.0134, 51.9644)),
    GermanVoltageLevelUtils.MV_10KV,
    16,
  )

  protected val nodeInputPvModel6 = new NodeInput(
    UUID.fromString("7607ca0b-959f-48b2-9c5c-1cf7c4ce0dc0"),
    "luebeck_3",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    Quantities.getQuantity(1d, PU),
    true,
    geometryFactory.createPoint(new Coordinate(10.4729, 53.6454)),
    GermanVoltageLevelUtils.MV_10KV,
    17,
  )
}
