/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala

/** Class that introduces functional ''mapping'' if there's an object that is
  * not wrapped in a monad, such as [[Either]] or [[Option]].
  *
  * This class is useful when code should be divided into multiple pieces that
  * each have their own scope, so that objects of the same class are not
  * confused. For example, in the following code snippet, ''a'', ''b'', ''c''
  * and ''d'' can be easily confused, which can lead to errors that are hard to
  * trace.
  *
  * {{{
  *   val a: Foo = create()
  *   val b: Foo = transform(a)
  *   val c: Foo = b.doStuff()
  *   val d: Foo = maybeBar.calculate(c)
  * }}}
  *
  * When using a [[Scope]], the variables cannot be confused anymore:
  *
  * {{{
  *   val d: Foo = Scope(create())
  *     .map(transform)
  *     .map(_.doStuff)
  *     .map(maybeBar.calculate)
  *     .get
  * }}}
  *
  * @param obj
  *   The object that is held
  * @tparam T
  *   The type of the object
  */
final case class Scope[T](private val obj: T) {
  def map[B](f: T => B): Scope[B] = Scope(f(obj))

  def get: T = obj
}
