/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.datamodel.models.input.system.characteristic.{CosPhiFixed, QV}
import edu.ie3.datamodel.models.{OperationTime, StandardUnits}
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.util.TimeUtil
import edu.ie3.util.interval.ClosedInterval
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID

/** Simple test data to be used in tests for PvModel. Should be extended as
  * needed.
  */
trait PvInputTestData
    extends DefaultTestData
    with NodeInputTestData
    with MockitoSugar {

  protected val pvInputMock: PvInput = mock[PvInput]
  when(pvInputMock.getUuid)
    .thenReturn(UUID.fromString("0c912248-9f70-445e-84ad-37b46cfb7111"))
  when(pvInputMock.getId).thenReturn("TestPvInputModel")

  protected val pvInputModel04Kv: PvInput = mock[PvInput]
  when(pvInputModel04Kv.getUuid)
    .thenReturn(UUID.fromString("d272b25b-107c-49d3-b5fe-5703f71671b0"))
  when(pvInputModel04Kv.getId).thenReturn("TestPvInputModel_0.4_kV")
  when(pvInputModel04Kv.getNode).thenReturn(nodeInputNoSlackNs04KvA)

  protected val pvInput = new PvInput(
    UUID.randomUUID(),
    "Dummy_PvModel",
    new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
    OperationTime.notLimited(),
    nodeInputNoSlackNs04KvA,
    CosPhiFixed.CONSTANT_CHARACTERISTIC,
    null,
    0.2,
    Quantities.getQuantity(12, StandardUnits.AZIMUTH),
    Quantities.getQuantity(90, StandardUnits.EFFICIENCY),
    Quantities.getQuantity(45, StandardUnits.SOLAR_ELEVATION_ANGLE),
    0.9,
    1.0,
    false,
    Quantities.getQuantity(10, StandardUnits.S_RATED),
    0.95,
  )

  private val operationTimeBuilder = OperationTime.builder()

  private val interval = new ClosedInterval[ZonedDateTime](
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T11:00:00Z"),
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T14:00:00Z"),
  )
  private val operationTime: OperationTime =
    operationTimeBuilder.withOperationTime(interval).build()

  protected val pvInputWithQCharacteristicLimitedOperationTime = new PvInput(
    UUID.randomUUID(),
    "Dummy_PvModel_With_Q_Characteristic",
    new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
    operationTime,
    nodeInputNoSlackNs04KvA,
    new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
    null,
    0.2,
    Quantities.getQuantity(12, StandardUnits.AZIMUTH),
    Quantities.getQuantity(90, StandardUnits.EFFICIENCY),
    Quantities.getQuantity(45, StandardUnits.SOLAR_ELEVATION_ANGLE),
    0.9,
    1.0,
    false,
    Quantities.getQuantity(10, StandardUnits.S_RATED),
    0.95,
  )

  protected val pvSouth1 = new PvInput(
    UUID.fromString("7ac5bb15-36ee-42b0-902b-9cd520e241b3"),
    "pv_south_1",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited,
    nodeInputPvModel3,
    CosPhiFixed.CONSTANT_CHARACTERISTIC,
    null,
    0.2,
    Quantities.getQuantity(16.09490984119475, StandardUnits.AZIMUTH),
    Quantities.getQuantity(91.23978812713176, StandardUnits.EFFICIENCY),
    Quantities
      .getQuantity(51.75144341774285, StandardUnits.SOLAR_ELEVATION_ANGLE),
    0.9,
    1.0,
    false,
    Quantities.getQuantity(100, StandardUnits.S_RATED),
    0.95,
  )

  protected val pvSouth2 = new PvInput(
    UUID.fromString("939d254a-98b9-43d9-939d-dac9d91e7d73"),
    "pv_south_2",
    new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
    OperationTime.notLimited(),
    nodeInputPvModel2,
    CosPhiFixed.CONSTANT_CHARACTERISTIC,
    null,
    0.2,
    Quantities.getQuantity(-11.883286549709737, StandardUnits.AZIMUTH),
    Quantities.getQuantity(93.55452200165019, StandardUnits.EFFICIENCY),
    Quantities
      .getQuantity(50.710754711180925, StandardUnits.SOLAR_ELEVATION_ANGLE),
    0.9,
    1.0,
    false,
    Quantities.getQuantity(100, StandardUnits.S_RATED),
    0.95,
  )

  protected val pvSouth3 = new PvInput(
    UUID.fromString("e3b34366-9a4b-4e8f-b46d-fccdd3c318b3"),
    "pv_south_3",
    new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
    OperationTime.notLimited(),
    nodeInputPvModel4,
    CosPhiFixed.CONSTANT_CHARACTERISTIC,
    null,
    0.2,
    Quantities.getQuantity(-3.6445723846554756, StandardUnits.AZIMUTH),
    Quantities.getQuantity(90.07983175106347, StandardUnits.EFFICIENCY),
    Quantities
      .getQuantity(50.727743320167065, StandardUnits.SOLAR_ELEVATION_ANGLE),
    0.9,
    1.0,
    false,
    Quantities.getQuantity(100, StandardUnits.S_RATED),
    0.95,
  )

  protected val pvSouth4 = new PvInput(
    UUID.fromString("403a3d17-0b71-4631-bcc4-71416c6376ed"),
    "pv_south_4",
    new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
    OperationTime.notLimited(),
    nodeInputPvModel6,
    CosPhiFixed.CONSTANT_CHARACTERISTIC,
    null,
    0.2,
    Quantities.getQuantity(-16.249216510914266, StandardUnits.AZIMUTH),
    Quantities.getQuantity(90.57719040894882, StandardUnits.EFFICIENCY),
    Quantities
      .getQuantity(53.588559702038765, StandardUnits.SOLAR_ELEVATION_ANGLE),
    0.9,
    1.0,
    false,
    Quantities.getQuantity(100, StandardUnits.S_RATED),
    0.95,
  )

  protected val pvEast1 = new PvInput(
    UUID.fromString("82925101-834d-4a43-b0b1-02d2156e8cd9"),
    "pv_east_1",
    new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
    OperationTime.notLimited(),
    nodeInputPvModel5,
    CosPhiFixed.CONSTANT_CHARACTERISTIC,
    null,
    0.2,
    Quantities.getQuantity(-32.24606798030436, StandardUnits.AZIMUTH),
    Quantities.getQuantity(90.48722543753683, StandardUnits.EFFICIENCY),
    Quantities
      .getQuantity(48.813238479197025, StandardUnits.SOLAR_ELEVATION_ANGLE),
    0.9,
    1.0,
    false,
    Quantities.getQuantity(100, StandardUnits.S_RATED),
    0.95,
  )

  protected val pvEast2 = new PvInput(
    UUID.fromString("f9122147-bad0-48ed-9e14-2404fcf07d3c"),
    "pv_east_2",
    new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
    OperationTime.notLimited(),
    nodeInputPvModel1,
    CosPhiFixed.CONSTANT_CHARACTERISTIC,
    null,
    0.2,
    Quantities.getQuantity(-54.537164978682995, StandardUnits.AZIMUTH),
    Quantities.getQuantity(94.0201214211993, StandardUnits.EFFICIENCY),
    Quantities
      .getQuantity(43.081511203199625, StandardUnits.SOLAR_ELEVATION_ANGLE),
    0.9,
    1.0,
    false,
    Quantities.getQuantity(100, StandardUnits.S_RATED),
    0.95,
  )

  protected val pvWest1 = new PvInput(
    UUID.fromString("da7bd11d-84d5-4db0-9f3f-2e5f1e7c77de"),
    "pv_west_1",
    new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
    OperationTime.notLimited(),
    nodeInputPvModel2,
    CosPhiFixed.CONSTANT_CHARACTERISTIC,
    null,
    0.2,
    Quantities.getQuantity(64.42003592383116, StandardUnits.AZIMUTH),
    Quantities.getQuantity(90.72157185757533, StandardUnits.EFFICIENCY),
    Quantities
      .getQuantity(54.5845761615783, StandardUnits.SOLAR_ELEVATION_ANGLE),
    0.9,
    1.0,
    false,
    Quantities.getQuantity(100, StandardUnits.S_RATED),
    0.95,
  )

  protected val pvWest2 = new PvInput(
    UUID.fromString("6d7542a3-4921-437d-94cb-37c1a7e79f9f"),
    "pv_west_2",
    new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
    OperationTime.notLimited(),
    nodeInputPvModel4,
    CosPhiFixed.CONSTANT_CHARACTERISTIC,
    null,
    0.2,
    Quantities.getQuantity(41.11365003045648, StandardUnits.AZIMUTH),
    Quantities.getQuantity(90.21663937252015, StandardUnits.EFFICIENCY),
    Quantities
      .getQuantity(52.37527455203235, StandardUnits.SOLAR_ELEVATION_ANGLE),
    0.9,
    1.0,
    false,
    Quantities.getQuantity(100, StandardUnits.S_RATED),
    0.95,
  )

  protected val pvInputsTest: Set[PvInput] = Set(
    pvSouth1,
    pvSouth2,
    pvSouth3,
    pvSouth4,
    pvEast1,
    pvEast2,
    pvWest1,
    pvWest2,
  )
}
