/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.collection.mutable

import edu.ie3.simona.test.common.UnitSpec

import scala.collection.SortedSet

class PriorityMultiBiSetSpec extends UnitSpec {
  private type Key = Int
  private type Value = String

  private val item1: Value = "test1"
  private val item2: Value = "test2"
  private val item3: Value = "test3"
  private val item4: Value = "test4"
  private val item5: Value = "test5"
  private val item6: Value = "test6"

  "A PriorityMultiBiSet" should {
    "be created correctly emptily" in {
      val prioSet = PriorityMultiBiSet.empty[Key, Value]

      prioSet.isEmpty shouldBe true
      prioSet.nonEmpty shouldBe false
      prioSet.keySet shouldBe SortedSet.empty[Key]
      prioSet.getAndRemoveSet(0) shouldBe Set.empty[Value]
      prioSet.headKeyOption shouldBe None
    }

    "behave correctly when adding to an empty map" in {
      val prioSet = PriorityMultiBiSet.empty[Key, Value]

      prioSet.set(0, item1)

      prioSet.isEmpty shouldBe false
      prioSet.nonEmpty shouldBe true
      prioSet.headKeyOption shouldBe Some(0)
      prioSet.keySet shouldBe SortedSet(0)
      prioSet.getKeyOf(item1) shouldBe Some(0)
      prioSet.getKeyOf(item2) shouldBe None

      prioSet.getAndRemoveSet(0) shouldBe Set(item1)
      prioSet.isEmpty shouldBe true
      prioSet.keySet shouldBe SortedSet.empty[Key]
      prioSet.getKeyOf(item1) shouldBe None
    }

    "behave correctly when adding and retrieving multiple values" in {
      // trying the second constructor here
      val prioSet = PriorityMultiBiSet.empty[Key, Value](5)

      prioSet.set(3, item3)

      prioSet.isEmpty shouldBe false
      prioSet.nonEmpty shouldBe true
      prioSet.headKeyOption shouldBe Some(3)
      prioSet.keySet shouldBe SortedSet(3)
      prioSet.getKeyOf(item3) shouldBe Some(3)

      prioSet.set(1, item1)

      prioSet.isEmpty shouldBe false
      prioSet.nonEmpty shouldBe true
      prioSet.headKeyOption shouldBe Some(1)
      prioSet.keySet shouldBe SortedSet(1, 3)
      prioSet.getKeyOf(item1) shouldBe Some(1)
      prioSet.getKeyOf(item3) shouldBe Some(3)

      prioSet.set(3, item2)

      prioSet.isEmpty shouldBe false
      prioSet.nonEmpty shouldBe true
      prioSet.headKeyOption shouldBe Some(1)
      prioSet.keySet shouldBe SortedSet(1, 3)
      prioSet.getKeyOf(item2) shouldBe Some(3)

      prioSet.set(3, item3)

      prioSet.isEmpty shouldBe false
      prioSet.nonEmpty shouldBe true
      prioSet.headKeyOption shouldBe Some(1)
      prioSet.keySet shouldBe SortedSet(1, 3)
      prioSet.getKeyOf(item3) shouldBe Some(3)

      prioSet.getAndRemoveSet(1) shouldBe Set(item1)

      prioSet.isEmpty shouldBe false
      prioSet.nonEmpty shouldBe true
      prioSet.headKeyOption shouldBe Some(3)
      prioSet.keySet shouldBe SortedSet(3)

      prioSet.getAndRemoveSet(1) shouldBe Set.empty[Value]
      prioSet.getAndRemoveSet(3) shouldBe Set(item2, item3)

      prioSet.isEmpty shouldBe true
      prioSet.nonEmpty shouldBe false
      prioSet.headKeyOption shouldBe None
      prioSet.keySet shouldBe SortedSet.empty[Key]

      prioSet.getAndRemoveSet(3) shouldBe Set.empty[Value]
    }

    "behave correctly when removing items" in {
      val prioSet = PriorityMultiBiSet.empty[Key, Value]

      prioSet.set(0, item1)
      prioSet.set(3, item3)
      // overwrites 0 -> item1
      prioSet.set(1, item1)
      prioSet.set(2, item4)
      prioSet.set(3, item2)
      prioSet.set(5, item5)

      prioSet.isEmpty shouldBe false
      prioSet.nonEmpty shouldBe true
      prioSet.headKeyOption shouldBe Some(1)
      prioSet.keySet shouldBe SortedSet(1, 2, 3, 5)
      prioSet.getKeyOf(item1) shouldBe Some(1)
      prioSet.getKeyOf(item2) shouldBe Some(3)
      prioSet.getKeyOf(item3) shouldBe Some(3)
      prioSet.getKeyOf(item4) shouldBe Some(2)
      prioSet.getKeyOf(item5) shouldBe Some(5)

      // does not exist
      prioSet.remove(item6) shouldBe false

      prioSet.isEmpty shouldBe false
      prioSet.nonEmpty shouldBe true
      prioSet.headKeyOption shouldBe Some(1)
      prioSet.keySet shouldBe SortedSet(1, 2, 3, 5)

      prioSet.remove(item4) shouldBe true

      prioSet.isEmpty shouldBe false
      prioSet.nonEmpty shouldBe true
      prioSet.headKeyOption shouldBe Some(1)
      prioSet.keySet shouldBe SortedSet(1, 3, 5)

      prioSet.getAndRemoveSet(1) shouldBe Set(item1)

      prioSet.isEmpty shouldBe false
      prioSet.nonEmpty shouldBe true
      prioSet.headKeyOption shouldBe Some(3)
      prioSet.keySet shouldBe SortedSet(3, 5)

      prioSet.remove(item3) shouldBe true

      prioSet.isEmpty shouldBe false
      prioSet.nonEmpty shouldBe true
      prioSet.headKeyOption shouldBe Some(3)
      prioSet.keySet shouldBe SortedSet(3, 5)

      prioSet.getAndRemoveSet(3) shouldBe Set(item2)

      prioSet.isEmpty shouldBe false
      prioSet.nonEmpty shouldBe true
      prioSet.headKeyOption shouldBe Some(5)
      prioSet.keySet shouldBe SortedSet(5)

      prioSet.remove(item5) shouldBe true

      prioSet.isEmpty shouldBe true
      prioSet.nonEmpty shouldBe false
      prioSet.headKeyOption shouldBe None
      prioSet.keySet shouldBe SortedSet.empty[Key]

      prioSet.getAndRemoveSet(5) shouldBe Set.empty[Value]

      // test if the table has been depleted as well -
      // if it is, adding should work as expected
      prioSet.set(1, item2)

      prioSet.isEmpty shouldBe false
      prioSet.nonEmpty shouldBe true
      prioSet.headKeyOption shouldBe Some(1)
      prioSet.keySet shouldBe SortedSet(1)

      prioSet.getAndRemoveSet(1) shouldBe Set(item2)

      prioSet.isEmpty shouldBe true
      prioSet.nonEmpty shouldBe false
      prioSet.headKeyOption shouldBe None
      prioSet.keySet shouldBe SortedSet.empty[Key]
      prioSet.getKeyOf(item1) shouldBe None
      prioSet.getKeyOf(item2) shouldBe None
      prioSet.getKeyOf(item3) shouldBe None
      prioSet.getKeyOf(item4) shouldBe None
      prioSet.getKeyOf(item5) shouldBe None

    }
  }
}
