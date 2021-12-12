/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.util.CollectionUtils._
import edu.ie3.util.quantities.PowerSystemUnits._
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities.getQuantity

import javax.measure.quantity.Dimensionless

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
          (getQuantity(1, PU), getQuantity(2, PU)),
          (getQuantity(2, PU), getQuantity(4, PU)),
          (getQuantity(3, PU), getQuantity(8, PU))
        )
      )

      def returnedSequence1: Seq[
        (ComparableQuantity[Dimensionless], ComparableQuantity[Dimensionless])
      ] = closestKeyValuePairs(map, getQuantity(1.5, PU))
      def returnedSequence2: Seq[
        (ComparableQuantity[Dimensionless], ComparableQuantity[Dimensionless])
      ] = closestKeyValuePairs(map, getQuantity(2.5, PU))
      def returnedSequence3: Seq[
        (ComparableQuantity[Dimensionless], ComparableQuantity[Dimensionless])
      ] = closestKeyValuePairs(map, getQuantity(3, PU))

      returnedSequence1 shouldBe Seq(
        (getQuantity(1, PU), getQuantity(2, PU)),
        (getQuantity(2, PU), getQuantity(4, PU))
      )
      returnedSequence2 shouldBe Seq(
        (getQuantity(2, PU), getQuantity(4, PU)),
        (getQuantity(3, PU), getQuantity(8, PU))
      )
      returnedSequence3 shouldBe Seq((getQuantity(3, PU), getQuantity(8, PU)))
    }
  }

}
