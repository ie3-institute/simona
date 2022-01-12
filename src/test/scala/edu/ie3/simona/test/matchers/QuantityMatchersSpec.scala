/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.matchers

import edu.ie3.simona.test.common.UnitSpec
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import javax.measure.quantity.Length

class QuantityMatchersSpec extends UnitSpec {
  "Testing quantities with custom quantity matchers" when {
    val a = Quantities.getQuantity(5d, Units.METRE)
    val b = Quantities.getQuantity(6d, Units.METRE)
    val toleranceQuantity = Quantities.getQuantity(1e-10, Units.METRE)

    "testing for equality" should {
      "pass if quantities are exactly the same" in {
        a should equalWithTolerance(a, 1e-10)
      }

      "pass if quantities are approximately the same" in {
        a should equalWithTolerance(
          a.add(toleranceQuantity.multiply(0.9)),
          1e-10
        )
      }

      "detect mismatch on tolerance exceeding" in {
        a should not(
          equalWithTolerance(a.add(toleranceQuantity.multiply(1.1)), 1e-10)
        )
      }
    }

    "testing for less than" should {
      "pass if the quantity is really less" in {
        a should beLessThanWithTolerance(b, 1e-10)
      }

      "pass if the quantity is less with tolerance" in {
        a.add(toleranceQuantity.multiply(0.9)) should beLessThanWithTolerance(
          a,
          1e-10
        )
      }

      "detect mismatch on tolerance exceeding" in {
        a.add(toleranceQuantity.multiply(1.1)) should not(
          beLessThanWithTolerance(a, 1e-10)
        )
      }
    }

    "testing for greater than" should {
      "pass if the quantity is really greater" in {
        b should beGreaterThanWithTolerance(a, 1e-10)
      }

      "pass if the quantity is greater with tolerance" in {
        a.subtract(
          toleranceQuantity.multiply(0.9)
        ) should beGreaterThanWithTolerance(a, 1e-10)
      }

      "detect mismatch on tolerance exceeding" in {
        a.subtract(toleranceQuantity.multiply(1.1)) should not(
          beGreaterThanWithTolerance(a, 1e-10)
        )
      }
    }
  }
}
