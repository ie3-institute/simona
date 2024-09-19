/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.simona.exceptions.InvalidParameterException

import java.util.UUID
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.grid.SystemComponentSpec.SystemComponentMock
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.OperationInterval

import java.time.ZonedDateTime
import scala.util.Try

/** Test for abstract class [[SystemComponent]]
  */
class SystemComponentSpec extends UnitSpec with DefaultTestData {
  sealed trait ValidSystemComponent {
    val systemComponent: SystemComponentMock = SystemComponentMock(
      operationInterval = OperationInterval(0L, 7200L)
    )
  }

  "A SystemComponent" should {

    "result in Failure when an already disabled SystemComponent should be disabled again" in new ValidSystemComponent {

      val tryVal: Try[String] = systemComponent.disable()
      tryVal.failure.exception.getMessage shouldBe s"${systemComponent.getClass.getSimpleName} ${systemComponent.id} is already out of operation!"

    }

    "be disabled by default after construction" in new ValidSystemComponent {

      systemComponent.isInOperation shouldBe false
    }

    "be disabled when disable() is called" in new ValidSystemComponent {

      systemComponent.enable()
      systemComponent.disable()

      systemComponent.isInOperation shouldBe false

    }

    "be enabled after enabled is called" in new ValidSystemComponent {

      systemComponent.enable()

      systemComponent.isInOperation shouldBe true
    }

    "result in Failure when enabled is called on an already enabled SystemComponent" in new ValidSystemComponent {

      systemComponent.enable()
      val tryVal: Try[String] = systemComponent.enable()
      tryVal.failure.exception.getMessage shouldBe s"${systemComponent.getClass.getSimpleName} ${systemComponent.id} is already in operation!"

    }

    def setup(): OperationTime.OperationTimeBuilder = {
      OperationTime.builder()
    }

    "determine the correct operation interval" in {

      val simulationEnd: ZonedDateTime =
        TimeUtil.withDefaults.toZonedDateTime("2019-01-02T00:00:00Z")

      val testCases = Table(
        ("operationStart", "operationEnd", "expectedInterval"),
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

      forAll(testCases) {
        (
            operationStart: Option[ZonedDateTime],
            operationEnd: Option[ZonedDateTime],
            expected: OperationInterval,
        ) =>
          val operationTimeBuilder = setup()
          operationStart.foreach(operationTimeBuilder.withStart)
          operationEnd.foreach(operationTimeBuilder.withEnd)
          val operationTime: OperationTime = operationTimeBuilder.build()
          val interval: OperationInterval =
            SystemComponent.determineOperationInterval(
              defaultSimulationStart,
              simulationEnd,
              operationTime,
            )
          interval should be(expected)
      }
    }

    "reject an operation end that is before the operation start" in {

      val simulationEnd: ZonedDateTime =
        TimeUtil.withDefaults.toZonedDateTime("2019-01-02T00:00:00Z")

      val operationTimeBuilder = setup()

      operationTimeBuilder.withStart(defaultSimulationEnd)
      operationTimeBuilder.withEnd(defaultSimulationStart)
      val operationTime: OperationTime = operationTimeBuilder.build()

      val exception = intercept[InvalidParameterException] {
        SystemComponent.determineOperationInterval(
          defaultSimulationStart,
          simulationEnd,
          operationTime,
        )
      }

      exception.getMessage should be(
        "The defined operation end is before it's operation start."
      )
    }
  }

}

object SystemComponentSpec {
  final case class SystemComponentMock(
      uuid: UUID = UUID.fromString("94b633a2-dfc0-4c28-acf5-d756150e5cde"),
      id: String = "SystemComponentMock",
      operationInterval: OperationInterval,
  ) extends SystemComponent(
        uuid,
        id,
        operationInterval,
      ) // concrete implementation for testing
}
