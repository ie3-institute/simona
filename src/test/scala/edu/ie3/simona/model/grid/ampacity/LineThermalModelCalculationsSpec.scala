/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.model.grid.ampacity.LineThermalModelCalculations.*
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.quantities.KelvinMetersPerWatt
import squants.{Dimensionless, Each, Meters, Percent}

class LineThermalModelCalculationsSpec extends UnitSpec {

  "A LineSegmentThermalModel" should {

    "return all correct thermal resistance for cable shells" in {

      val cases = Table(
        ("specificThermalResistivity", "innerRadius", "outerRadius", "thermalResistance"),
        (6.0, 0.04145/2, 0.04145/2 + 0.01283, 0.46012825568418825), //Anders Cable No 3

      )

      forAll(cases) { (specificThermalResistivity, innerRadius, outerRadius, thermalResistance) =>
        val specTherRes = KelvinMetersPerWatt(specificThermalResistivity)
        val inRadius = Meters(innerRadius)
        val outRadius= Meters(outerRadius)
        val expectedThermalResistance = KelvinMetersPerWatt(thermalResistance)

        val actual = calcThermalResistanceCableShells(specTherRes, inRadius, outRadius)

        actual shouldBe expectedThermalResistance
      }
    }
  }
}
