/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.matchers

import org.scalactic.TolerantNumerics
import org.scalatest.matchers.{MatchResult, Matcher}

trait DoubleMatchers extends TolerantNumerics {

  class DoubleMatcher(right: Double, implicit val tolerance: Double)
      extends Matcher[Double] {
    private val equality = tolerantDoubleEquality(tolerance)

    override def apply(left: Double): MatchResult = MatchResult(
      equality.areEqual(left, right),
      s"The values $left and $right differ more than $tolerance in value",
      s"The values $left and $right differ less than $tolerance in value",
    )
  }

  def approximate(right: Double)(implicit tolerance: Double) =
    new DoubleMatcher(right, tolerance)

}
