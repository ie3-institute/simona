/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala

import edu.ie3.simona.test.common.UnitSpec

import scala.collection.SortedSet

class PriorityMultiQueueSpec extends UnitSpec {
  private type Key = Int
  private type Value = String

  private val item1: Value = "test1"
  private val item2: Value = "test2"
  private val item3: Value = "test3"

  "A PriorityMultiQueue" should {
    "be created correctly emptily" in {
      val emptyQueue = PriorityMultiQueue.empty[Key, Value]

      emptyQueue.isEmpty shouldBe true
      emptyQueue.nonEmpty shouldBe false
      emptyQueue.keySet shouldBe SortedSet.empty[Key]
      emptyQueue.allValues shouldBe Iterable.empty[Value]
    }

    "behave correctly when adding to an empty map" in {
      val queue = PriorityMultiQueue.empty[Key, Value]

      queue.add(0, item1)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(0)
      queue.keySet shouldBe SortedSet(0)
      queue.allValues shouldBe Iterable(item1)

      queue.poll() shouldBe Some(item1)
      queue.isEmpty shouldBe true
      queue.allValues shouldBe Iterable.empty[Value]
    }

    "behave correctly when adding multiple values to multiple keys" in {
      val queue = PriorityMultiQueue.empty[Key, Value]

      queue.add(3, item3)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(3)
      queue.keySet shouldBe SortedSet(3)
      queue.allValues shouldBe Iterable(item3)

      queue.add(1, item1)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(1)
      queue.keySet shouldBe SortedSet(1, 3)
      queue.allValues shouldBe Iterable(item1, item3)

      queue.add(3, item2)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(1)
      queue.keySet shouldBe SortedSet(1, 3)
      queue.allValues shouldBe Iterable(item1, item3, item2)

      queue.add(3, item3)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(1)
      queue.keySet shouldBe SortedSet(1, 3)
      queue.allValues shouldBe Iterable(item1, item3, item2, item3)

      queue.poll() shouldBe Some(item1)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(3)
      queue.keySet shouldBe SortedSet(3)
      queue.allValues shouldBe Iterable(item3, item2, item3)

      queue.poll() shouldBe Some(item3)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(3)
      queue.keySet shouldBe SortedSet(3)
      queue.allValues shouldBe Iterable(item2, item3)

      queue.poll() shouldBe Some(item2)

      queue.isEmpty shouldBe false
      queue.nonEmpty shouldBe true
      queue.headKeyOption shouldBe Some(3)
      queue.keySet shouldBe SortedSet(3)
      queue.allValues shouldBe Iterable(item3)

      queue.poll() shouldBe Some(item3)

      queue.isEmpty shouldBe true
      queue.nonEmpty shouldBe false
      queue.headKeyOption shouldBe None
      queue.keySet shouldBe SortedSet.empty[Key]
      queue.allValues shouldBe Iterable.empty[Value]

      queue.poll() shouldBe None
    }
  }
}
