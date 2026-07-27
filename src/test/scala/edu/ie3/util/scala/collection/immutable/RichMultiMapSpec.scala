/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.collection.immutable

import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.collection.immutable.RichMultiMap.*

class RichMultiMapSpec extends UnitSpec {

  private val testSet: Map[Int, Set[String]] =
    Map(1 -> Set("a", "b"), 2 -> Set("c"))

  "Functionality for a multi map" should {

    "test for containment correctly" in {

      testSet.contains(1, "a") shouldBe true
      testSet.contains(1, "c") shouldBe false
      testSet.contains(3, "a") shouldBe false

    }

    "add items correctly" in {

      testSet.added(1, "c") shouldBe testSet.updated(1, Set("a", "b", "c"))
      testSet.added(3, "d") shouldBe testSet.updated(3, Set("d"))

    }

    "remove items correctly" in {

      testSet.removed(1, "a") shouldBe testSet.updated(1, Set("b"))
      testSet.removed(2, "c") shouldBe testSet.removed(2)
      testSet.removed(3, "a") shouldBe testSet

    }

  }

}
