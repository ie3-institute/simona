/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.datamodel.models.input.{NodeInput, OperatorInput}
import edu.ie3.datamodel.models.{OperationTime, StandardUnits}
import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.PowerSystemUnits.{
  DEGREE_GEOM,
  KILOVOLTAMPERE,
  PU,
}
import org.locationtech.jts.geom.{Coordinate, GeometryFactory, Point}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.quantity.Quantities.getQuantity
import tech.units.indriya.unit.Units.PERCENT

import java.util.UUID

trait PvPlantsTestData extends NodeInputTestData {

  /*protected val pvPlantInputSouth1 = new PvInput(
    UUID.fromString("7ac5bb15-36ee-42b0-902b-9cd520e241b3"),
    "pv_south_1",
    0.2, // albedo
    16.09490984119475, // azimuth
    0.95, // cosphi_rated
    Quantities.getQuantity(91.23978812713176, PowerSystemUnits.WATT_PER_SQUAREMETRE), // eta_conv
    51.75144341774285, // elevation_angle
    0.9, // k_g
    1, // k_t
    false, // market_reaction
    nodeInputNoSlackNs04KvA// node
  )

  protected val pvPlantInputSouth2 = new PvInput(
    UUID.fromString("939d254a-98b9-43d9-939d-dac9d91e7d73"),
    "pv_south_2",
    0.2,
    -11.883286549709737,
    0.95,
    Quantities.getQuantity(93.55452200165019, PowerSystemUnits.WATT_PER_SQUAREMETRE),
    50.710754711180925,
    0.9,
    1,
    false,
    UUID.fromString("9a2524f1-3639-4e90-a547-81a259712f8c")
  )

  protected val pvPlantInputSouth3 = new PvInput(
    UUID.fromString("e3b34366-9a4b-4e8f-b46d-fccdd3c318b3"),
    "pv_south_3",
    0.2,
    -3.6445723846554756,
    0.95,
    Quantities.getQuantity(90.07983175106347, PowerSystemUnits.WATT_PER_SQUAREMETRE),
    50.727743320167065,
    0.9,
    1,
    false,
    UUID.fromString("9354b02c-a4a9-4e9d-905a-e48110b04d88")
  )

  protected val pvPlantInputSouth4 = new PvInput(
    UUID.fromString("403a3d17-0b71-4631-bcc4-71416c6376ed"),
    "pv_south_4",
    0.2,
    -16.249216510914266,
    0.95,
    Quantities.getQuantity(90.57719040894882, PowerSystemUnits.WATT_PER_SQUAREMETRE),
    53.588559702038765,
    0.9,
    1,
    false,
    UUID.fromString("7607ca0b-959f-48b2-9c5c-1cf7c4ce0dc0")
  )

  protected val pvPlantInputEast1 = new PvInput(
    UUID.fromString("82925101-834d-4a43-b0b1-02d2156e8cd9"),
    "pv_east_1",
    0.2,
    -32.24606798030436,
    0.95,
    Quantities.getQuantity(90.48722543753683, PowerSystemUnits.WATT_PER_SQUAREMETRE),
    48.813238479197025,
    0.9,
    1,
    false,
    UUID.fromString("393eb0e3-7873-4d51-a830-2f1d98ff5a60")
  )

  protected val pvPlantInputEast2 = new PvInput(
    UUID.fromString("f9122147-bad0-48ed-9e14-2404fcf07d3c"),
    "pv_east_2",
    0.2,
    -54.537164978682995,
    0.95,
    Quantities.getQuantity(94.0201214211993, PowerSystemUnits.WATT_PER_SQUAREMETRE),
    43.081511203199625,
    0.9,
    1,
    false,
    UUID.fromString("19a4861e-cbf5-4dcc-bc1d-98611fda6eb5")
  )

  protected val pvPlantInputWest1 = new PvInput(
    UUID.fromString("da7bd11d-84d5-4db0-9f3f-2e5f1e7c77de"),
    "pv_west_1",
    0.2,
    64.42003592383116,
    0.95,
    Quantities.getQuantity(90.72157185757533, PowerSystemUnits.WATT_PER_SQUAREMETRE),
    54.5845761615783,
    0.9,
    1,
    false,
    UUID.fromString("9a2524f1-3639-4e90-a547-81a259712f8c")
  )

  protected val pvPlantInputWest2 = new PvInput(
    UUID.fromString("6d7542a3-4921-437d-94cb-37c1a7e79f9f"),
    "pv_west_2",
    0.2,
    41.11365003045648,
    0.95,
    Quantities.getQuantity(90.21663937252015, PowerSystemUnits.WATT_PER_SQUAREMETRE),
    52.37527455203235,
    0.9,
    1,
    false,
    UUID.fromString("9354b02c-a4a9-4e9d-905a-e48110b04d88")
  )

  protected val setOfPvs: Set[PvInput] = Set (
    pvPlantInputSouth1,
    pvPlantInputSouth2,
    pvPlantInputSouth3,
    pvPlantInputSouth4,
    pvPlantInputEast1,
    pvPlantInputEast2,
    pvPlantInputWest1,
    pvPlantInputWest2,
  )*/

  val geometryFactory = new GeometryFactory()
  val p: Point = geometryFactory.createPoint(new Coordinate(13.2491, 53.457909))
  val nodeInput = new NodeInput(
    UUID.fromString("85f8b517-8a2d-4c20-86c6-3ff3c5823e6d"),
    "NodeInputModel for PvModel Test",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    getQuantity(1, PU),
    false,
    p,
    GermanVoltageLevelUtils.MV_20KV,
    11,
  )

  protected val pvInput = new PvInput(
    UUID.fromString("adb4eb23-1dd6-4406-a5e7-02e1e4c9dead"),
    "Pv Model Test",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited,
    nodeInput,
    new CosPhiFixed("cosPhiFixed:{(0.0,0.9)}"),
    null,
    0.20000000298023224,
    getQuantity(-8.926613807678223, DEGREE_GEOM),
    getQuantity(97, PERCENT),
    getQuantity(41.01871871948242, DEGREE_GEOM),
    0.8999999761581421,
    1,
    false,
    getQuantity(10, KILOVOLTAMPERE),
    0.8999999761581421,
  )

  protected val setOfPvsTest: Set[PvInput] = Set(
    pvInput
  )

}
