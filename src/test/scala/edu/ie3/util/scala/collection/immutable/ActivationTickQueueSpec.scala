/*
 * © 2021-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.collection.immutable

import edu.ie3.simona.test.common.UnitSpec

class ActivationTickQueueSpec extends UnitSpec {

  "An activation tick queue" should {
    "return an empty sequence, if queried" in {
      val empty = ActivationTickQueue.empty

      empty.nextTick shouldBe None
      empty.length shouldBe 0
    }

    "be built correctly from empty input sequence" in {
      val empty = ActivationTickQueue(Seq.empty[Long])

      empty.nextTick shouldBe None
      empty.length shouldBe 0
    }

    "be built correctly from unordered input sequence" in {
      val inputSequence = Seq(5L, 3L, 1L, 2L, 4L)
      val expectedSequence = Seq(1L, 2L, 3L, 4L, 5L)

      inside(ActivationTickQueue(inputSequence)) {
        case activationTicks @ ActivationTickQueue(ticks) =>
          activationTicks.nextTick shouldBe Some(1L)
          activationTicks.length shouldBe 5
          ticks shouldBe expectedSequence
      }
    }

    "pop nothing from empty sequence" in {
      val sequence = ActivationTickQueue.empty

      inside(sequence.dropFirst) {
        case remainingTicks @ ActivationTickQueue(ticks) =>
          remainingTicks.nextTick shouldBe None
          remainingTicks.length shouldBe 0
          ticks should be(empty)
      }
    }

    "pop correct from sequence with one entry" in {
      val sequence = ActivationTickQueue(Seq(1L))

      inside(sequence.dropFirst) {
        case remainingTicks @ ActivationTickQueue(ticks) =>
          remainingTicks.nextTick shouldBe None
          remainingTicks.length shouldBe 0
          ticks should be(empty)
      }
    }

    "pop correct from sequence with more than one entry" in {
      val sequence = ActivationTickQueue(Seq(1L, 2L, 3L, 4L))

      inside(sequence.dropFirst) {
        case remainingTicks @ ActivationTickQueue(ticks) =>
          remainingTicks.nextTick shouldBe Some(2L)
          remainingTicks.length shouldBe 3
          ticks shouldBe Seq(2L, 3L, 4L)
      }
    }
  }

}
