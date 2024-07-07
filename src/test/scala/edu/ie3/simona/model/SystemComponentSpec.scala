/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.simona.exceptions.InvalidParameterException
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.OperationInterval
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.time.ZonedDateTime

class SystemComponentSpec extends AnyFlatSpec with Matchers {

  def setup(): OperationTime.OperationTimeBuilder = {
    OperationTime.builder()
  }

  "SystemComponent" should "determine the correct operation interval" in {

    val defaultSimulationStart: ZonedDateTime =
      TimeUtil.withDefaults.toZonedDateTime("2019-01-01T00:00:00Z")
    val defaultSimulationEnd: ZonedDateTime =
      TimeUtil.withDefaults.toZonedDateTime("2019-01-02T00:00:00Z")

    val testCases = Seq(
      (
        Some(TimeUtil.withDefaults.toZonedDateTime("2019-01-01T00:00:00Z")),
        Some(TimeUtil.withDefaults.toZonedDateTime("2019-01-02T00:00:00Z")),
        OperationInterval(0L, 86400L),
      ),
      (
        Some(TimeUtil.withDefaults.toZonedDateTime("2019-01-02T00:00:00Z")),
        Some(TimeUtil.withDefaults.toZonedDateTime("2019-01-02T00:00:00Z")),
        OperationInterval(86400L, 86400L),
      ),
      (
        Some(TimeUtil.withDefaults.toZonedDateTime("2019-01-01T00:00:00Z")),
        Some(TimeUtil.withDefaults.toZonedDateTime("2019-01-01T00:00:00Z")),
        OperationInterval(0L, 0L),
      ),
      (
        None,
        Some(TimeUtil.withDefaults.toZonedDateTime("2019-01-01T00:00:00Z")),
        OperationInterval(0L, 0L),
      ),
      (
        Some(TimeUtil.withDefaults.toZonedDateTime("2019-01-02T00:00:00Z")),
        None,
        OperationInterval(86400L, 86400L),
      ),
    )

    for ((operationStart, operationEnd, expected) <- testCases) {
      val operationTimeBuilder = setup()

      operationStart.foreach(operationTimeBuilder.withStart)
      operationEnd.foreach(operationTimeBuilder.withEnd)

      val operationTime: OperationTime = operationTimeBuilder.build()

      val interval: OperationInterval =
        SystemComponent.determineOperationInterval(
          defaultSimulationStart,
          defaultSimulationEnd,
          operationTime,
        )

      interval should be(expected)
    }
  }

  it should "reject an operation end that is before the operation start" in {
    val defaultSimulationStart: ZonedDateTime =
      TimeUtil.withDefaults.toZonedDateTime("2019-01-01T00:00:00Z")
    val defaultSimulationEnd: ZonedDateTime =
      TimeUtil.withDefaults.toZonedDateTime("2019-01-02T00:00:00Z")

    val operationTimeBuilder = setup()

    operationTimeBuilder.withStart(defaultSimulationEnd)
    operationTimeBuilder.withEnd(defaultSimulationStart)
    val operationTime: OperationTime = operationTimeBuilder.build()

    val exception = intercept[InvalidParameterException] {
      SystemComponent.determineOperationInterval(
        defaultSimulationStart,
        defaultSimulationEnd,
        operationTime,
      )
    }

    exception.getMessage should be(
      "The defined operation end is before it's operation start."
    )
  }

}
