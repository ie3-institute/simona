/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.collection.mutable

import edu.ie3.simona.test.common.UnitSpec

import scala.collection.SortedSet

class PriorityMultiSetSpec extends UnitSpec {
  private type Value = String

  private val item1: Value = "test1"
  private val item2: Value = "test2"
  private val item3: Value = "test3"
  private val item4: Value = "test4"
  private val item5: Value = "test5"

  "A PriorityMultiQueue" should {
    "be created correctly emptily" in {
      val emptyQueue = PriorityMultiSet.empty[Value]

      emptyQueue.isEmpty shouldBe true
      emptyQueue.nonEmpty shouldBe false
      emptyQueue.keySet shouldBe empty
      emptyQueue.allValues shouldBe empty
    }

    "behave correctly when adding to an empty map" in {
      val queue = PriorityMultiSet.empty[Value]

      queue.add(0L, item1)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(0L)
      queue.keySet shouldBe SortedSet(0L)
      queue.allValues shouldBe Iterable(item1)

      queue.pollTo(99L) shouldBe Iterable(item1)
      queue.isEmpty shouldBe true
      queue.allValues shouldBe empty
    }

    "behave correctly when adding multiple values to multiple keys" in {
      val queue = PriorityMultiSet.empty[Value]

      queue.add(3L, item3)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(3L)
      queue.keySet shouldBe SortedSet(3L)
      queue.allValues shouldBe Iterable(item3)

      queue.add(1L, item1)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(1L)
      queue.keySet shouldBe SortedSet(1L, 3L)
      queue.get(1L) shouldBe Some(List(item1))
      queue.allValues shouldBe Iterable(item1, item3)

      queue.add(3L, item2)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(1L)
      queue.keySet shouldBe SortedSet(1L, 3L)
      queue.allValues should contain only (item1, item2, item3)

      queue.add(3L, item4)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(1L)
      queue.keySet shouldBe SortedSet(1L, 3L)
      queue.allValues should contain only (item1, item3, item2, item4)

      queue.pollTo(1L) shouldBe Iterable(item1)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(3L)
      queue.keySet shouldBe SortedSet(3L)
      queue.allValues should contain only (item3, item2, item4)

      queue.pollTo(4L) should contain only (item3, item2, item4)

      queue.isEmpty shouldBe true
      queue.nonEmpty shouldBe false
      queue.headKeyOption shouldBe None
      queue.keySet shouldBe empty
      queue.allValues shouldBe empty

      queue.pollTo(99L) shouldBe empty
    }

    "behave correctly when polling up to a certain key" in {
      val queue = PriorityMultiSet.empty[Value]

      queue.add(3L, item3)
      queue.add(1L, item1)
      queue.add(2L, item2)
      queue.add(3L, item5)
      queue.add(5L, item4)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(1L)
      queue.keySet shouldBe SortedSet(1L, 2L, 3L, 5L)
      queue.allValues should contain only (item1, item2, item3, item4, item5)

      queue.pollTo(2L) should contain only (item1, item2)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(3L)
      queue.keySet shouldBe SortedSet(3L, 5L)
      queue.allValues should contain only (item3, item4, item5)

      queue.pollTo(3L) should contain only (item3, item5)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(5L)
      queue.keySet shouldBe SortedSet(5L)
      queue.allValues should contain only item4

      queue.pollTo(5L) should contain only item4

      queue.isEmpty shouldBe true
      queue.nonEmpty shouldBe false
      queue.headKeyOption shouldBe None
      queue.keySet shouldBe empty
      queue.allValues shouldBe empty

      queue.pollTo(Integer.MAX_VALUE) shouldBe empty

      // test if the table has been depleted as well -
      // if it is, adding should work as expected
      queue.add(1L, item2)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(1L)
      queue.keySet shouldBe SortedSet(1L)
      queue.allValues should contain only item2

    }

    "behave correctly when removing values" in {
      val queue = PriorityMultiSet.empty[Value]

      queue.add(3L, item3)
      queue.add(1L, item1)
      queue.add(2L, item2)
      queue.add(3L, item5)
      queue.add(5L, item4)

      queue.isEmpty shouldBe false
      queue.keySet shouldBe SortedSet(1L, 2L, 3L, 5L)
      queue.allValues should contain only (item1, item2, item3, item4, item5)

      queue.remove(3L, item5)

      queue.isEmpty shouldBe false
      queue.keySet shouldBe SortedSet(1L, 2L, 3L, 5L)
      queue.allValues should contain only (item1, item2, item3, item4)

      queue.remove(2L, item2)

      queue.isEmpty shouldBe false
      queue.keySet shouldBe SortedSet(1L, 3L, 5L)
      queue.allValues should contain only (item1, item3, item4)

      // removing non-existing
      queue.remove(5L, item2)

      queue.isEmpty shouldBe false
      queue.keySet shouldBe SortedSet(1L, 3L, 5L)
      queue.allValues should contain only (item1, item3, item4)

      queue.remove(5L, item4)

      queue.isEmpty shouldBe false
      queue.keySet shouldBe SortedSet(1L, 3L)
      queue.allValues should contain only (item1, item3)

      queue.remove(3L, item3)

      queue.isEmpty shouldBe false
      queue.keySet shouldBe SortedSet(1L)
      queue.allValues should contain only item1

      queue.add(1L, item2)

      queue.isEmpty shouldBe false
      queue.keySet shouldBe SortedSet(1L)
      queue.allValues should contain only (item1, item2)
    }
  }
}
