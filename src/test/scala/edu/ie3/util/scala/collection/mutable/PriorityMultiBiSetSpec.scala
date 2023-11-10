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

  "A PriorityMultiBiSet" should {
    "be created correctly emptily" in {
      val emptyQueue = PriorityMultiBiSet.empty[Key, Value]

      emptyQueue.isEmpty shouldBe true
      emptyQueue.nonEmpty shouldBe false
      emptyQueue.keySet shouldBe SortedSet.empty[Key]
      emptyQueue.getAndRemoveSet(0) shouldBe Set.empty[Value]
      emptyQueue.headKeyOption shouldBe None
    }

    "behave correctly when adding to an empty map" in {
      val queue = PriorityMultiBiSet.empty[Key, Value]

      queue.set(0, item1)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(0)
      queue.keySet shouldBe SortedSet(0)
      queue.getKeyOf(item1) shouldBe Some(0)
      queue.getKeyOf(item2) shouldBe None

      queue.getAndRemoveSet(0) shouldBe Set(item1)
      queue.isEmpty shouldBe true
      queue.keySet shouldBe SortedSet.empty[Key]
      queue.getKeyOf(item1) shouldBe None
    }

    "behave correctly when adding and retrieving multiple values" in {
      // trying the second constructor here
      val queue = PriorityMultiBiSet.empty[Key, Value](5)

      queue.set(3, item3)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(3)
      queue.keySet shouldBe SortedSet(3)
      queue.getKeyOf(item3) shouldBe Some(3)

      queue.set(1, item1)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(1)
      queue.keySet shouldBe SortedSet(1, 3)
      queue.getKeyOf(item1) shouldBe Some(1)
      queue.getKeyOf(item3) shouldBe Some(3)

      queue.set(3, item2)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(1)
      queue.keySet shouldBe SortedSet(1, 3)
      queue.getKeyOf(item2) shouldBe Some(3)

      queue.set(3, item3)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(1)
      queue.keySet shouldBe SortedSet(1, 3)
      queue.getKeyOf(item3) shouldBe Some(3)

      queue.getAndRemoveSet(1) shouldBe Set(item1)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(3)
      queue.keySet shouldBe SortedSet(3)

      queue.getAndRemoveSet(1) shouldBe Set.empty[Value]
      queue.getAndRemoveSet(3) shouldBe Set(item2, item3)

      queue.isEmpty shouldBe true
      queue.nonEmpty shouldBe false
      queue.headKeyOption shouldBe None
      queue.keySet shouldBe SortedSet.empty[Key]

      queue.getAndRemoveSet(3) shouldBe Set.empty[Value]
    }

    "behave correctly when removing items" in {
      val queue = PriorityMultiBiSet.empty[Key, Value]

      queue.set(0, item1)
      queue.set(3, item3)
      // overwrites 0 -> item1
      queue.set(1, item1)
      queue.set(2, item4)
      queue.set(3, item2)
      queue.set(5, item5)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(1)
      queue.keySet shouldBe SortedSet(1, 2, 3, 5)
      queue.getKeyOf(item1) shouldBe Some(1)
      queue.getKeyOf(item2) shouldBe Some(3)
      queue.getKeyOf(item3) shouldBe Some(3)
      queue.getKeyOf(item4) shouldBe Some(2)
      queue.getKeyOf(item5) shouldBe Some(5)

      // does not exist
      queue.remove(2, item1) shouldBe false

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(1)
      queue.keySet shouldBe SortedSet(1, 2, 3, 5)

      queue.remove(2, item4) shouldBe true

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(1)
      queue.keySet shouldBe SortedSet(1, 3, 5)

      queue.getAndRemoveSet(1) shouldBe Set(item1)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(3)
      queue.keySet shouldBe SortedSet(3, 5)

      queue.remove(3, item3) shouldBe true

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(3)
      queue.keySet shouldBe SortedSet(3, 5)

      queue.getAndRemoveSet(3) shouldBe Set(item2)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(5)
      queue.keySet shouldBe SortedSet(5)

      queue.remove(5, item5) shouldBe true

      queue.isEmpty shouldBe true
      queue.nonEmpty shouldBe false
      queue.headKeyOption shouldBe None
      queue.keySet shouldBe SortedSet.empty[Key]

      queue.getAndRemoveSet(5) shouldBe Set.empty[Value]

      // test if the table has been depleted as well -
      // if it is, adding should work as expected
      queue.set(1, item2)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(1)
      queue.keySet shouldBe SortedSet(1)

      queue.getAndRemoveSet(1) shouldBe Set(item2)

      queue.isEmpty shouldBe true
      queue.nonEmpty shouldBe false
      queue.headKeyOption shouldBe None
      queue.keySet shouldBe SortedSet.empty[Key]
      queue.getKeyOf(item1) shouldBe None
      queue.getKeyOf(item2) shouldBe None
      queue.getKeyOf(item3) shouldBe None
      queue.getKeyOf(item4) shouldBe None
      queue.getKeyOf(item5) shouldBe None

    }
  }
}
