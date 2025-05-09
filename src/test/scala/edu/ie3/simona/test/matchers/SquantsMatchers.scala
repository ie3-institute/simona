/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.matchers

import org.scalatest.matchers.{MatchResult, Matcher}
import squants.Quantity

/** Trait, to simplify test coding, that is reliant on squants */
trait SquantsMatchers {
  class SquantsMatcher[Q <: Quantity[Q]](right: Q, implicit val tolerance: Q)
      extends Matcher[Quantity[Q]] {
    override def apply(left: Quantity[Q]): MatchResult = MatchResult(
      left =~ right,
      s"The quantities $left and $right differ more than $tolerance in value",
      s"The quantities $left and $right differ less than $tolerance in value",
    )
  }

  def approximate[Q <: Quantity[Q]](right: Q)(implicit tolerance: Q) =
    new SquantsMatcher(right, tolerance)
}
