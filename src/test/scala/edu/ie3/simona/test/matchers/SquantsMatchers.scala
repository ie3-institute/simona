/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.matchers

import edu.ie3.simona.exceptions.QuantityException
import squants.Quantity

/** Trait, to simplify test coding, that is reliant on squants */
trait SquantsMatchers {
  def equalWithTolerance[T <: Quantity[T]](actual: T, expected: T)(implicit
      tolerance: T
  ): Boolean = {
    val bool = actual =~ expected

    if (!bool) {
      throw new QuantityException(
        s"The actual quantity $actual and the expected quantity $expected differ more than $tolerance in value"
      )
    }
    bool
  }

}
