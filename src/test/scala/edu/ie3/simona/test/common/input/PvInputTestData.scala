/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.{OperationTime, StandardUnits}
import edu.ie3.simona.test.common.DefaultTestData
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import tech.units.indriya.quantity.Quantities

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
    1,
    Quantities.getQuantity(12, StandardUnits.AZIMUTH),
    Quantities.getQuantity(10, StandardUnits.EFFICIENCY),
    Quantities.getQuantity(100, StandardUnits.SOLAR_ELEVATION_ANGLE),
    12,
    11,
    false,
    Quantities.getQuantity(10, StandardUnits.S_RATED),
    0.95,
  )
}
