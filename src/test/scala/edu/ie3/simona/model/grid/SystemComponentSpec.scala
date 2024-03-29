/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import java.util.UUID

import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.grid.SystemComponentSpec.SystemComponentMock
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.OperationInterval

import scala.util.Try

/** Test for abstract class [[SystemComponent]]
  */
class SystemComponentSpec extends UnitSpec {
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
