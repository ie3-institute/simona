/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.matchers

import edu.ie3.simona.test.matchers.QuantityMatchers.{
  EqualMatcher,
  GreaterMatcher,
  LessMatcher
}
import edu.ie3.util.quantities.QuantityUtil

import javax.measure.Quantity
import org.scalatest.matchers.{MatchResult, Matcher}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

/** Trait, to simplify test coding, that is reliant on [[Quantity]] s
  */
trait QuantityMatchers {
  def equalWithTolerance[Q <: Quantity[Q]](
      right: Quantity[Q],
      tolerance: Double = 1e-10
  ) = new EqualMatcher(right, tolerance)

  def beLessThanWithTolerance[Q <: Quantity[Q]](
      right: ComparableQuantity[Q],
      tolerance: Double = 1e-10
  ) = new LessMatcher[Q](right, tolerance)

  def beGreaterThanWithTolerance[Q <: Quantity[Q]](
      right: ComparableQuantity[Q],
      tolerance: Double = 1e-10
  ) = new GreaterMatcher[Q](right, tolerance)
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

  class LessMatcher[Q <: Quantity[Q]](
      right: ComparableQuantity[Q],
      tolerance: Double
  ) extends Matcher[ComparableQuantity[Q]] {
    override def apply(left: ComparableQuantity[Q]): MatchResult = compare(
      left,
      right,
      tolerance,
      (right: ComparableQuantity[Q], tolerance: ComparableQuantity[Q]) =>
        right.add(tolerance),
      (left: ComparableQuantity[Q], right: ComparableQuantity[Q]) =>
        left.isLessThan(right),
      (
          lhs: ComparableQuantity[Q],
          rhs: ComparableQuantity[Q],
          toleranceQuantity: ComparableQuantity[Q]
      ) => s"The quantity $lhs is not less than $rhs + $toleranceQuantity.",
      (
          lhs: ComparableQuantity[Q],
          rhs: ComparableQuantity[Q],
          toleranceQuantity: ComparableQuantity[Q]
      ) => s"The quantity $lhs is less than $rhs + $toleranceQuantity."
    )
  }

  class GreaterMatcher[Q <: Quantity[Q]](
      right: ComparableQuantity[Q],
      tolerance: Double
  ) extends Matcher[ComparableQuantity[Q]] {
    override def apply(left: ComparableQuantity[Q]): MatchResult = compare(
      left,
      right,
      tolerance,
      (right: ComparableQuantity[Q], tolerance: ComparableQuantity[Q]) =>
        right.subtract(tolerance),
      (left: ComparableQuantity[Q], right: ComparableQuantity[Q]) =>
        left.isGreaterThan(right),
      (
          lhs: ComparableQuantity[Q],
          rhs: ComparableQuantity[Q],
          toleranceQuantity: ComparableQuantity[Q]
      ) => s"The quantity $lhs is not greater than $rhs - $toleranceQuantity.",
      (
          lhs: ComparableQuantity[Q],
          rhs: ComparableQuantity[Q],
          toleranceQuantity: ComparableQuantity[Q]
      ) => s"The quantity $lhs is greater than $rhs - $toleranceQuantity."
    )
  }

  /** Compares two Quantities with tolerance
    *
    * @param left
    *   Left hand side of the comparison
    * @param right
    *   Right hand side of the comparison
    * @param tolerance
    *   Numerical tolerance to be applied to the right hand side
    * @param rightWithToleranceFun
    *   Function, how to build the right hand side with tolerance
    * @param compareFun
    *   Compare function (lhs, rhs) => Condition
    * @param rawFailureMessage
    *   Failure message in case condition is not satisfied
    * @param rawNegatedFailureMessage
    *   Failure message in case the condition is negated
    * @tparam Q
    *   Type of [[ComparableQuantity]] to compare
    * @return
    *   A [[MatchResult]]
    */
  private def compare[Q <: Quantity[Q]](
      left: ComparableQuantity[Q],
      right: ComparableQuantity[Q],
      tolerance: Double,
      rightWithToleranceFun: (
          ComparableQuantity[Q],
          ComparableQuantity[Q]
      ) => ComparableQuantity[Q],
      compareFun: (ComparableQuantity[Q], ComparableQuantity[Q]) => Boolean,
      rawFailureMessage: (
          ComparableQuantity[Q],
          ComparableQuantity[Q],
          ComparableQuantity[Q]
      ) => String,
      rawNegatedFailureMessage: (
          ComparableQuantity[Q],
          ComparableQuantity[Q],
          ComparableQuantity[Q]
      ) => String
  ): MatchResult = {
    val toleranceQuantity = Quantities.getQuantity(tolerance, right.getUnit)
    val rightWithTolerance = rightWithToleranceFun(right, toleranceQuantity)
    MatchResult(
      compareFun(left, rightWithTolerance),
      rawFailureMessage(left, right, toleranceQuantity),
      rawNegatedFailureMessage(left, right, toleranceQuantity)
    )
  }
}
