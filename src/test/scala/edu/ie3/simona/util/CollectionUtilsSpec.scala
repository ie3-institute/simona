/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.util.CollectionUtils._
import squants.Each
import squants.Dimensionless

class CollectionUtilsSpec extends UnitSpec {

  "The collection utils are capable" should {
    "of detecting duplicate entries in a list" in {
      val input = List("a", "b", "c", "d", "b")
      isUniqueList(input) shouldBe false
    }

    "of detecting lists with unique entries" in {
      val input = List("a", "b", "c", "d", "e")
      isUniqueList(input) shouldBe true
    }

    "of computing the closest key-value pairs, given a map and a key" in {
      val map = Map.from(
        List(
          (Each(1d), Each(2d)),
          (Each(2d), Each(4d)),
          (Each(3d), Each(8d)),
        )
      )

      def returnedSequence1: Seq[
        (Dimensionless, Dimensionless)
      ] = closestKeyValuePairs(map, Each(1.5))
      def returnedSequence2: Seq[
        (Dimensionless, Dimensionless)
      ] = closestKeyValuePairs(map, Each(2.5))
      def returnedSequence3: Seq[
        (Dimensionless, Dimensionless)
      ] = closestKeyValuePairs(map, Each(3d))

      returnedSequence1 shouldBe Seq(
        (Each(1d), Each(2d)),
        (Each(2d), Each(4d)),
      )
      returnedSequence2 shouldBe Seq(
        (Each(2d), Each(4d)),
        (Each(3d), Each(8d)),
      )
      returnedSequence3 shouldBe Seq((Each(3d), Each(8d)))
    }
  }

}
