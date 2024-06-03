/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import java.util.UUID
import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.WecInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.util.TimeUtil

import java.time.ZonedDateTime

trait WecInputTestData extends WecTypeInputTestData {

  val wecInputModel = new WecInput(
    UUID.randomUUID(),
    "Dummy_WecInput",
    new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
    OperationTime.notLimited(),
    nodeInputNoSlackNs04KvB,
    CosPhiFixed.CONSTANT_CHARACTERISTIC,
    null,
    wecTypeInputEnerconE82,
    false,
  )

}
