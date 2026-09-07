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

  class OptionalSquantsMatcher[Q <: Quantity[Q]](
      right: Option[Q],
      implicit val tolerance: Q,
  ) extends Matcher[Option[Q]] {
    override def apply(left: Option[Q]): MatchResult = {
      (left, right) match {
        case (Some(leftValue), Some(rightValue)) =>
          MatchResult(
            leftValue =~ rightValue,
            s"The quantities $leftValue and $rightValue differ more than $tolerance in value",
            s"The quantities $leftValue and $rightValue differ less than $tolerance in value",
          )
        case (None, _) =>
          MatchResult(
            false,
            s"Expected $right but got None",
            s"Got None when a value was expected",
          )
        case (Some(v), None) =>
          MatchResult(
            false,
            s"Expected None but got Some($v)",
            s"Got a value when None was expected",
          )
      }
    }
  }

  def approximate[Q <: Quantity[Q]](right: Q)(implicit
      tolerance: Q
  ): Matcher[Q] =
    new SquantsMatcher(right, tolerance)

  def approximate[Q <: Quantity[Q]](right: Option[Q])(implicit
      tolerance: Q
  ): Matcher[Option[Q]] =
    new OptionalSquantsMatcher(right, tolerance)
}
