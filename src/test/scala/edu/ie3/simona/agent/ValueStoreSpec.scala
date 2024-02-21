/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent

import edu.ie3.simona.test.common.UnitSpec

import scala.collection.SortedMap

class ValueStoreSpec extends UnitSpec {
  "An empty value store" should {
    val emptyValueStore: ValueStore[String] = ValueStore[String](Long.MaxValue)

    "be properly instantiated" in {
      val storeGetter =
        PrivateMethod[Map[Long, String]](Symbol("store"))
      (emptyValueStore invokePrivate storeGetter()) shouldBe Map
        .empty[Long, String]
      emptyValueStore.maxTickSpan shouldBe Long.MaxValue
    }

    "return None on request of last known tick" in {
      emptyValueStore.lastKnownTick(100L) shouldBe None
    }

    "return None on request of the last known entry" in {
      emptyValueStore.last(100L) shouldBe None
    }

    "return None on request of the overall last known entry" in {
      emptyValueStore.last() shouldBe None
    }

    "return an empty map on request of tick window" in {
      emptyValueStore.get(50L, 100L) shouldBe Map
        .empty[Long, String]
    }
  }

  "A filled value store" should {
    val filledValueStore: ValueStore[String] = ValueStore[String](
      5,
      SortedMap(1L -> "One", 2L -> "Two", 3L -> "Three", 4L -> "Four"),
    )

    "be properly instantiated" in {
      val storeGetter =
        PrivateMethod[Map[Long, String]](Symbol("store"))
      (filledValueStore invokePrivate storeGetter()) shouldBe Map(
        1L -> "One",
        2L -> "Two",
        3L -> "Three",
        4L -> "Four",
      )
      filledValueStore.maxTickSpan shouldBe 5
    }

    "return 4 on request of last known tick" in {
      filledValueStore.lastKnownTick(100L) shouldBe Some(4)
    }

    "return (4L, \"Four\") on request of the last known entry" in {
      filledValueStore.last(100L) shouldBe Some((4L, "Four"))
    }

    "return (4L, \"Four\") on request of the overall last known entry" in {
      filledValueStore.last() shouldBe Some((4L, "Four"))
    }

    "return an empty map on request of tick window" in {
      filledValueStore.get(2L, 3L) shouldBe Map(
        2L -> "Two",
        3L -> "Three",
      )
    }

    "update and truncate correctly" in {
      val updated = ValueStore.updateValueStore(filledValueStore, 5L, "Five")
      val updatedOnceAgain = ValueStore.updateValueStore(updated, 6L, "Six")
      val storeGetter =
        PrivateMethod[Map[Long, String]](Symbol("store"))
      (updatedOnceAgain invokePrivate storeGetter()) shouldBe Map(
        2L -> "Two",
        3L -> "Three",
        4L -> "Four",
        5L -> "Five",
        6L -> "Six",
      )
    }
  }
}
