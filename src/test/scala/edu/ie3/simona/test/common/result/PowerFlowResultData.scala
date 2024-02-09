/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.result

import java.util.UUID

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.result.NodeResult
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

trait PowerFlowResultData {

  private val dummyTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-30 17:26:44")
  private val dummyInputModel =
    UUID.fromString("e5ac84d3-c7a5-4870-a42d-837920aec9bb")

  val dummyPvResult = new PvResult(
    UUID.fromString("7f404c4c-fc12-40de-95c9-b5827a40f18b"),
    dummyTime,
    dummyInputModel,
    Quantities.getQuantity(10, StandardUnits.ACTIVE_POWER_IN),
    Quantities.getQuantity(10, StandardUnits.REACTIVE_POWER_IN),
  )

  val dummyPvResultDataString =
    "7f404c4c-fc12-40de-95c9-b5827a40f18b,e5ac84d3-c7a5-4870-a42d-837920aec9bb,0.01,0.01,2020-01-30T17:26:44Z[UTC]"

  val dummyNodeResult = new NodeResult(
    UUID.fromString("7f404c4c-fc13-40de-95c9-b5827a40f18b"),
    dummyTime,
    dummyInputModel,
    Quantities.getQuantity(1.0, PowerSystemUnits.PU),
    Quantities.getQuantity(10, PowerSystemUnits.DEGREE_GEOM),
  )

  val dummyNodeResultString =
    "7f404c4c-fc13-40de-95c9-b5827a40f18b,e5ac84d3-c7a5-4870-a42d-837920aec9bb,2020-01-30T17:26:44Z[UTC],10.0,1.0"

  val dummySwitchResult = new SwitchResult(
    UUID.fromString("647efb19-ec38-4e01-812b-0d751f0150e8"),
    dummyTime,
    dummyInputModel,
    true,
  )

  val dummySwitchResultString =
    "647efb19-ec38-4e01-812b-0d751f0150e8,true,e5ac84d3-c7a5-4870-a42d-837920aec9bb,2020-01-30T17:26:44Z[UTC]"

  val dummyTrafo2wResult = new Transformer2WResult(
    UUID.fromString("7f404c4c-fc13-40de-95c9-b5827a40f18c"),
    dummyTime,
    dummyInputModel,
    Quantities.getQuantity(100, Units.AMPERE),
    Quantities.getQuantity(100, PowerSystemUnits.DEGREE_GEOM),
    Quantities.getQuantity(100, Units.AMPERE),
    Quantities.getQuantity(100, PowerSystemUnits.DEGREE_GEOM),
    0,
  )

  val dummyTrafo2wResultDataString =
    "7f404c4c-fc13-40de-95c9-b5827a40f18c,100.0,100.0,100.0,100.0,e5ac84d3-c7a5-4870-a42d-837920aec9bb,0,2020-01-30T17:26:44Z[UTC]"

  val dummyLineResult = new LineResult(
    UUID.fromString("8f404c4c-fc13-40de-95c9-b5827a40f18c"),
    dummyTime,
    dummyInputModel,
    Quantities.getQuantity(100, Units.AMPERE),
    Quantities.getQuantity(100, PowerSystemUnits.DEGREE_GEOM),
    Quantities.getQuantity(100, Units.AMPERE),
    Quantities.getQuantity(100, PowerSystemUnits.DEGREE_GEOM),
  )

  val dummyLineResultDataString =
    "8f404c4c-fc13-40de-95c9-b5827a40f18c,100.0,100.0,100.0,100.0,e5ac84d3-c7a5-4870-a42d-837920aec9bb,2020-01-30T17:26:44Z[UTC]"

}
