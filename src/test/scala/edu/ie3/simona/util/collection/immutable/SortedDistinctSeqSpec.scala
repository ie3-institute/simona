/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util.collection.immutable

import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.collection.immutable.SortedDistinctSeq

class SortedDistinctSeqSpec extends UnitSpec {
  "A sorted and distinct sequence" should {
    "return an empty sequence, if queried" in {
      inside(SortedDistinctSeq.empty) {
        case SortedDistinctSeq(internalSequence) =>
          internalSequence.isEmpty shouldBe true
      }
    }

    "be built correctly from empty input sequence" in {
      def inputSequence = Seq.empty[Long]

      inside(SortedDistinctSeq(inputSequence)) {
        case SortedDistinctSeq(internalSequence) =>
          internalSequence.isEmpty shouldBe true
      }
    }

    "be built correctly from unordered input sequence" in {
      def inputSequence = Seq(5L, 3L, 1L, 2L, 4L)
      def expectedSequence = Seq(1L, 2L, 3L, 4L, 5L)

      inside(SortedDistinctSeq(inputSequence)) {
        case SortedDistinctSeq(internalSequence) =>
          internalSequence shouldBe expectedSequence
      }
    }

    "pop nothing from empty sequence" in {
      def sequence: SortedDistinctSeq[Long] = SortedDistinctSeq.empty[Long]

      sequence.pop match {
        case (maybeNextTick, remainderTicks) =>
          maybeNextTick shouldBe None
          inside(remainderTicks) { case SortedDistinctSeq(internalSequence) =>
            internalSequence.isEmpty shouldBe true
          }
      }
    }

    "pop correct from sequence with one entry" in {
      def sequence: SortedDistinctSeq[Long] = SortedDistinctSeq(Seq(1L))

      sequence.pop match {
        case (maybeNextTick, remainderTicks) =>
          maybeNextTick shouldBe Some(1L)
          inside(remainderTicks) { case SortedDistinctSeq(internalSequence) =>
            internalSequence.isEmpty shouldBe true
          }
      }
    }

    "pop correct from sequence with more than one entry" in {
      def sequence: SortedDistinctSeq[Long] =
        SortedDistinctSeq(Seq(1L, 2L, 3L, 4L))

      sequence.pop match {
        case (maybeNextTick, remainderTicks) =>
          maybeNextTick shouldBe Some(1L)
          inside(remainderTicks) { case SortedDistinctSeq(internalSequence) =>
            internalSequence shouldBe Seq(2L, 3L, 4L)
          }
      }
    }
  }
}
