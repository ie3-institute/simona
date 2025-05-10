/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.matchers

import edu.ie3.simona.test.common.UnitSpec
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

class QuantityMatchersSpec extends UnitSpec {
  "Testing quantities with custom quantity matchers" when {
    val quant = Quantities.getQuantity(5d, Units.METRE)
    val toleranceQuantity = Quantities.getQuantity(1e-10, Units.METRE)

    given testTolerance: Double = 1e-10

    "testing for equality" should {
      "pass if quantities are exactly the same" in {
        quant should equalWithTolerance(quant)
      }

      "pass if quantities are approximately the same" in {
        quant should equalWithTolerance(
          quant.add(toleranceQuantity.multiply(0.9))
        )
      }

      "detect mismatch on tolerance exceeding" in {
        quant should not(
          equalWithTolerance(quant.add(toleranceQuantity.multiply(1.1)))
        )
      }
    }

  }
}
