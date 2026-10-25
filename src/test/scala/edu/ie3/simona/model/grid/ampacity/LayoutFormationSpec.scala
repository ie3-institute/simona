/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.test.common.UnitSpec

class LayoutFormationSpec extends UnitSpec {

  "LayoutFormation.fromString" should {

    "parse each known formation string" in {
      LayoutFormation.fromString("flat-distance") should be(
        LayoutFormation.FlatDistance
      )
      LayoutFormation.fromString("flat-touching") should be(
        LayoutFormation.FlatTouching
      )
      LayoutFormation.fromString("trefoil-touching") should be(
        LayoutFormation.TrefoilTouching
      )
      LayoutFormation.fromString("trefoil-not-touching") should be(
        LayoutFormation.TrefoilNotTouching
      )
    }

    "be case- and whitespace-insensitive" in {
      LayoutFormation.fromString("  Flat-Distance ") should be(
        LayoutFormation.FlatDistance
      )
      LayoutFormation.fromString("TREFOIL-TOUCHING") should be(
        LayoutFormation.TrefoilTouching
      )
    }

    "throw an exception for an unknown formation" in {
      an[IllegalArgumentException] should be thrownBy {
        LayoutFormation.fromString("unknown")
      }
      an[IllegalArgumentException] should be thrownBy {
        LayoutFormation.fromString("")
      }
    }
  }
}

