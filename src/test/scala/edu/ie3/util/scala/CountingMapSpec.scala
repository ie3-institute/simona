/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala

import edu.ie3.simona.test.common.UnitSpec

class CountingMapSpec extends UnitSpec {
  private type Key = Int

  "A CountingMap" should {
    "be created correctly emptily" in {
      val emptyMap = CountingMap.empty[Key]

      emptyMap.isEmpty shouldBe true
      emptyMap.minKeyOption shouldBe None
      emptyMap.get(0) shouldBe None
      emptyMap.contains(0) shouldBe false
    }

    "be created correctly with initial values" in {
      val map = CountingMap.from[Key](
        Iterable(
          0 -> 1L,
          1 -> 1L
        )
      )

      map.isEmpty shouldBe false
      map.minKeyOption shouldBe Some(0)
      map.get(0) shouldBe Some(1L)
      map.contains(0) shouldBe true
      map.get(1) shouldBe Some(1L)
      map.contains(1) shouldBe true
    }

    "behave correctly when adding to an empty map" in {
      val map = CountingMap.empty[Key]

      map.add(0)

      map.isEmpty shouldBe false
      map.minKeyOption shouldBe Some(0)
      map.get(0) shouldBe Some(1L)
      map.contains(0) shouldBe true
      map.contains(1) shouldBe false
    }

    "behave correctly when adding twice to a key" in {
      val map = CountingMap.empty[Key]

      map.add(0)
      map.add(0)

      map.isEmpty shouldBe false
      map.minKeyOption shouldBe Some(0)
      map.get(0) shouldBe Some(2L)
    }

    "behave correctly when adding an item to a non-existing key in a non-empty map" in {
      val map = CountingMap.empty[Key]

      map.add(0)
      map.add(1)

      map.isEmpty shouldBe false
      map.minKeyOption shouldBe Some(0)
      map.get(0) shouldBe Some(1L)
      map.get(1) shouldBe Some(1L)

      map.subtract(0)
      map.minKeyOption shouldBe Some(1)
    }

    "behave correctly when removing an existing item resulting in a non-empty map" in {
      val map = CountingMap.empty[Key]
      map.add(0)
      map.add(0)

      map.subtract(0)

      map.minKeyOption shouldBe Some(0)
      map.get(0) shouldBe Some(1L)
    }

    "behave correctly when removing an existing item resulting in an empty map" in {
      val map = CountingMap.empty[Key]

      map.add(1)
      map.subtract(1)

      map.minKeyOption shouldBe None
      map.get(1) shouldBe None
    }

    "behave correctly when removing a non-existing item" in {
      val map = CountingMap.empty[Key]
      map.add(0)
      map.add(1)

      map.subtract(2)

      map.minKeyOption shouldBe Some(0)
      map.get(0) shouldBe Some(1L)
      map.get(1) shouldBe Some(1L)
    }
  }
}
