/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.model.participant

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.simona.model.SystemComponent

import java.time.ZonedDateTime
import edu.ie3.simona.test.common.input.LoadInputTestData
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.OperationInterval

/** Trait to supply different test data environments for testing a load agent
  */
trait LoadTestData extends LoadInputTestData {
  protected implicit val simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z")
  protected val simulationEndDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T01:00:00Z")
  protected val operationTime: OperationTime = OperationTime
    .builder()
    .withStart(simulationStartDate)
    .withEnd(simulationEndDate)
    .build()
  protected val operationInterval: OperationInterval =
    SystemComponent.determineOperationInterval(
      simulationStartDate,
      simulationEndDate,
      operationTime,
    )
}
