/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.matchers

import edu.ie3.simona.test.matchers.QuantityMatchers.EqualMatcher
import edu.ie3.util.quantities.QuantityUtil
import org.scalatest.matchers.{MatchResult, Matcher}

import javax.measure.Quantity

@deprecated("Use implementation in power system utils package")
/** Trait, to simplify test coding, that is reliant on [[Quantity]] s
  */
trait QuantityMatchers {
  def equalWithTolerance[Q <: Quantity[Q]](
      right: Quantity[Q],
      tolerance: Double = 1e-10
  ) = new EqualMatcher(right, tolerance)
}

object QuantityMatchers {
  class EqualMatcher[Q <: Quantity[Q]](right: Quantity[Q], tolerance: Double)
      extends Matcher[Quantity[Q]] {
    override def apply(left: Quantity[Q]): MatchResult = MatchResult(
      QuantityUtil.equals(left, right, tolerance),
      s"The quantities $left and $right differ more than $tolerance in value",
      s"The quantities $left and $right differ less than $tolerance in value"
    )
  }
}
