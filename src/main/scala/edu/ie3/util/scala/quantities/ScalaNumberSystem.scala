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
    java2scala(super.add(scala2java(x), scala2java(y)))

  override def subtract(x: Number, y: Number): Number =
    java2scala(super.subtract(scala2java(x), scala2java(y)))

  override def multiply(x: Number, y: Number): Number =
    java2scala(super.multiply(scala2java(x), scala2java(y)))

  override def divide(x: Number, y: Number): Number =
    java2scala(super.divide(scala2java(x), scala2java(y)))

  override def divideAndRemainder(
      x: Number,
      y: Number,
      roundRemainderTowardsZero: Boolean
  ): Array[Number] =
    super
      .divideAndRemainder(
        scala2java(x),
        scala2java(y),
        roundRemainderTowardsZero
      )
      .map(java2scala)

  override def power(number: Number, exponent: Int): Number =
    java2scala(super.power(scala2java(number), exponent))

  override def reciprocal(number: Number): Number =
    java2scala(super.reciprocal(scala2java(number)))

  override def negate(number: Number): Number =
    java2scala(super.negate(scala2java(number)))

  override def signum(number: Number): Int = super.signum(scala2java(number))

  override def abs(number: Number): Number =
    java2scala(super.abs(scala2java(number)))

  override def exp(number: Number): Number =
    java2scala(super.exp(scala2java(number)))

  override def log(number: Number): Number =
    java2scala(super.log(scala2java(number)))

  override def narrow(number: Number): Number =
    number // FIXME hack: don't narrow anymore

  override def compare(x: Number, y: Number): Int =
    super.compare(scala2java(x), scala2java(y))

  override def isZero(number: Number): Boolean =
    super.isZero(scala2java(number))

  override def isOne(number: Number): Boolean = super.isOne(scala2java(number))

  override def isLessThanOne(number: Number): Boolean =
    super.isLessThanOne(scala2java(number))

  override def isInteger(number: Number): Boolean =
    super.isInteger(scala2java(number))

  /** If necessary, converts [[BigDecimal]] into [[java.math.BigDecimal]]
    *
    * @return
    *   the very same [[Number]] but [[BigDecimal]] brought to
    *   [[java.math.BigDecimal]] on necessity
    */
  def scala2java: Number => Number = {
    case bd: BigDecimal => bd.bigDecimal
    case x              => x
  }

  /** If necessary, converts [[java.math.BigDecimal]] into [[BigDecimal]]
    *
    * @return
    *   the very same [[Number]] but [[java.math.BigDecimal]] brought to
    *   [[BigDecimal]] on necessity
    */
  def java2scala: Number => Number = {
    case bd: java.math.BigDecimal => BigDecimal.javaBigDecimal2bigDecimal(bd)
    case x                        => x
  }
}
