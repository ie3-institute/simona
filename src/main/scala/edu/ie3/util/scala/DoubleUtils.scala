/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala

@deprecated("Use implementation in power system utils package")
object DoubleUtils {
  implicit class ImplicitDouble(d: Double) {
    def ~=(other: Double)(implicit precision: Double): Boolean =
      (d - other).abs <= precision
    def !~=(other: Double)(implicit precision: Double): Boolean =
      (d - other).abs > precision
  }
}
