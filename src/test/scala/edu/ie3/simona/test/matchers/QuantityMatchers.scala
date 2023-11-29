/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.matchers

import edu.ie3.util.quantities.QuantityUtil
import org.scalatest.matchers.{MatchResult, Matcher}

import javax.measure.Quantity

/** Trait, to simplify test coding, that is reliant on [[Quantity]] s
  */
trait QuantityMatchers {
  class QuantityMatcher[Q <: Quantity[Q]](right: Quantity[Q], tolerance: Double)
      extends Matcher[Quantity[Q]]
      with QuantityMatchers {
    override def apply(left: Quantity[Q]): MatchResult = MatchResult(
      QuantityUtil.equals(left, right, tolerance),
      QuantityMatchers.assembleRawFailureMessage(left, right, tolerance),
      QuantityMatchers.assembleNegatedFailureMessage(left, right, tolerance)
    )
  }

  def equalWithTolerance[Q <: Quantity[Q]](
      right: Quantity[Q],
      tolerance: Double = 1e-10
  ) = new QuantityMatcher(right, tolerance)
}

case object QuantityMatchers extends QuantityMatchers {
  private def assembleRawFailureMessage[Q <: Quantity[Q]](
      lhs: Quantity[Q],
      rhs: Quantity[Q],
      tolerance: Double
  ) = s"The quantities $lhs and $rhs differ more than $tolerance in value"
  private def assembleNegatedFailureMessage[Q <: Quantity[Q]](
      lhs: Quantity[Q],
      rhs: Quantity[Q],
      tolerance: Double
  ) = s"The quantities $lhs and $rhs differ less than $tolerance in value"
}
