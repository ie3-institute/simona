/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import tech.units.indriya.function.DefaultNumberSystem

/** This number system simply delegates the method calls to it's parent class.
  * The only difference is, that [[BigDecimal]] is transferred to
  * [[java.math.BigDecimal]] and back upon necessity. Due to its functionality
  * as an extension of the [[DefaultNumberSystem]] it CANNOT be an object!
  */
final class ScalaNumberSystem extends DefaultNumberSystem {
  override def add(x: Number, y: Number): Number =
    x.doubleValue + y.doubleValue

  override def subtract(x: Number, y: Number): Number =
    x.doubleValue - y.doubleValue

  override def multiply(x: Number, y: Number): Number =
    x.doubleValue * y.doubleValue

  override def divide(x: Number, y: Number): Number =
    x.doubleValue / y.doubleValue

  override def divideAndRemainder(
      x: Number,
      y: Number,
      roundRemainderTowardsZero: Boolean,
  ): Array[Number] = {
    val signX = signum(x)
    val signY = signum(y)

    val sign = signX * signY
    // handle corner cases when x or y are zero
    if (sign == 0) {
      if (signY == 0) throw new ArithmeticException("division by zero")
      if (signX == 0) Array[Number](0, 0)
    }

    val div = x.doubleValue / y.intValue()
    val remainder = x.doubleValue - div

    Array(div, remainder)
  }

  override def power(number: Number, exponent: Int): Number = {
    Math.pow(number.doubleValue, exponent)
  }

  override def reciprocal(number: Number): Number =
    divide(1d, number)

  override def negate(number: Number): Number =
    number.doubleValue * (-1d)

  override def signum(number: Number): Int =
    Math.signum(number.doubleValue).intValue

  override def abs(number: Number): Number =
    Math.abs(number.doubleValue)

  override def exp(number: Number): Number =
    Math.exp(number.doubleValue)

  override def log(number: Number): Number =
    Math.log(number.doubleValue)

  override def narrow(number: Number): Number =
    // FIXME hack: don't narrow anymore, turn everything into doubles
    number

  override def compare(x: Number, y: Number): Int =
    java.lang.Double.compare(x.doubleValue, y.doubleValue)

  override def isZero(number: Number): Boolean =
    compare(number.doubleValue, 0d) == 0

  override def isOne(number: Number): Boolean =
    compare(number.doubleValue, 1d) == 0

  override def isLessThanOne(number: Number): Boolean =
    number.doubleValue < 1d
}
