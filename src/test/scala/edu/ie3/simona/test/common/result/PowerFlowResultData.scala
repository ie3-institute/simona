/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.result

import java.util.UUID
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.result.{
  CongestionResult,
  NodeResult,
  ResultEntity,
}
import edu.ie3.datamodel.models.result.connector.{
  LineResult,
  SwitchResult,
  Transformer2WResult,
}
import edu.ie3.datamodel.models.result.system.PvResult
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.time.ZonedDateTime

trait PowerFlowResultData {

  protected val dummyTime: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-30T17:26:44Z")
  protected val dummyInputModel: UUID =
    UUID.fromString("e5ac84d3-c7a5-4870-a42d-837920aec9bb")
  protected val dummyPvResultModel: UUID =
    UUID.fromString("ade2fb2a-eac2-4a01-9012-02d58dfda212")
  protected val dummyNodeResultModel: UUID =
    UUID.fromString("b9006ed8-60bb-4b58-beeb-f3efd9086f16")
  protected val dummySwitchResultModel: UUID =
    UUID.fromString("52ea0504-a5fc-455b-88a3-47afcbf68610")
  protected val dummyTrafo2WResultModel: UUID =
    UUID.fromString("d562e2aa-6eb7-4af4-ab6c-287b49fe3595")
  protected val dummyLineResultModel: UUID =
    UUID.fromString("7919f27b-3fef-485a-b732-7d461b2aae1a")

  given Conversion[ResultEntity, Map[UUID, Iterable[ResultEntity]]] =
    (res: ResultEntity) => Map(res.getInputModel -> Iterable(res))

  given Conversion[Iterable[ResultEntity], Map[UUID, Iterable[ResultEntity]]] =
    (res: Iterable[ResultEntity]) => res.groupBy(_.getInputModel)

  val dummyPvResult = new PvResult(
    dummyTime,
    dummyPvResultModel,
    Quantities.getQuantity(10, StandardUnits.ACTIVE_POWER_IN),
    Quantities.getQuantity(10, StandardUnits.REACTIVE_POWER_IN),
  )

  val dummyPvResultDataString =
    "ade2fb2a-eac2-4a01-9012-02d58dfda212,0.01,0.01,2020-01-30T17:26:44Z"

  val dummyNodeResult = new NodeResult(
    dummyTime,
    dummyNodeResultModel,
    Quantities.getQuantity(1.0, PowerSystemUnits.PU),
    Quantities.getQuantity(10, PowerSystemUnits.DEGREE_GEOM),
  )

  val dummyNodeResult2 = new NodeResult(
    dummyTime,
    dummyNodeResultModel,
    Quantities.getQuantity(1.01, PowerSystemUnits.PU),
    Quantities.getQuantity(10, PowerSystemUnits.DEGREE_GEOM),
  )

  val dummyNodeResultPlusHour = new NodeResult(
    dummyTime.plusHours(1),
    dummyNodeResultModel,
    Quantities.getQuantity(1.0, PowerSystemUnits.PU),
    Quantities.getQuantity(10, PowerSystemUnits.DEGREE_GEOM),
  )

  val dummyNodeResult2PlusHour = new NodeResult(
    dummyTime.plusHours(1),
    dummyNodeResultModel,
    Quantities.getQuantity(1.01, PowerSystemUnits.PU),
    Quantities.getQuantity(10, PowerSystemUnits.DEGREE_GEOM),
  )

  val dummyNodeCongestionResult = new CongestionResult(
    dummyTime,
    dummyNodeResultModel,
    CongestionResult.InputModelType.NODE,
    -1,
    Quantities.getQuantity(1.11, PowerSystemUnits.PU),
    Quantities.getQuantity(0.9, PowerSystemUnits.PU),
    Quantities.getQuantity(1.1, PowerSystemUnits.PU),
  )

  val dummyNodeCongestionResultPlusHour = new CongestionResult(
    dummyTime.plusHours(1),
    dummyNodeResultModel,
    CongestionResult.InputModelType.NODE,
    -1,
    Quantities.getQuantity(1.11, PowerSystemUnits.PU),
    Quantities.getQuantity(0.9, PowerSystemUnits.PU),
    Quantities.getQuantity(1.1, PowerSystemUnits.PU),
  )

  val dummyNodeResultString =
    "b9006ed8-60bb-4b58-beeb-f3efd9086f16,2020-01-30T17:26:44Z,10.0,1.0"

  val dummySwitchResult = new SwitchResult(
    dummyTime,
    dummySwitchResultModel,
    true,
  )

  val dummySwitchResultString =
    "true,52ea0504-a5fc-455b-88a3-47afcbf68610,2020-01-30T17:26:44Z"

  val dummyTrafo2wResult = new Transformer2WResult(
    dummyTime,
    dummyTrafo2WResultModel,
    Quantities.getQuantity(100, Units.AMPERE),
    Quantities.getQuantity(100, PowerSystemUnits.DEGREE_GEOM),
    Quantities.getQuantity(100, Units.AMPERE),
    Quantities.getQuantity(100, PowerSystemUnits.DEGREE_GEOM),
    0,
  )

  val dummyTrafo2wResultDataString =
    "100.0,100.0,100.0,100.0,d562e2aa-6eb7-4af4-ab6c-287b49fe3595,0,2020-01-30T17:26:44Z"

  val dummyLineResult = new LineResult(
    dummyTime,
    dummyLineResultModel,
    Quantities.getQuantity(100, Units.AMPERE),
    Quantities.getQuantity(100, PowerSystemUnits.DEGREE_GEOM),
    Quantities.getQuantity(100, Units.AMPERE),
    Quantities.getQuantity(100, PowerSystemUnits.DEGREE_GEOM),
  )

  val dummyLineResultDataString =
    "100.0,100.0,100.0,100.0,7919f27b-3fef-485a-b732-7d461b2aae1a,2020-01-30T17:26:44Z"

}
