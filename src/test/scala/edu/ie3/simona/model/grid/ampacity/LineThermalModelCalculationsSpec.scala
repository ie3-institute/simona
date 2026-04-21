/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.model.grid.ampacity.LineThermalModelCalculations.*
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.quantities.{KelvinMetersPerWatt, ThermalResistivity}
import squants.Meters

class LineThermalModelCalculationsSpec extends UnitSpec {

  implicit val resistanceTolerance: ThermalResistivity = KelvinMetersPerWatt(1e-10)

  "A LineSegmentThermalModel" should {

    "return all correct thermal resistance for cable shells" in {

      val cases = Table(
        ("specificThermalResistivity", "innerRadius", "outerRadius", "thermalResistance"),
        (3.5, 0.0205/2, 0.0205/2 + 0.0034+0.0008+0.00065, 0.21580767835674133), //Anders Cable No 1 T1 (should be 0.214)
        (5.0, 0.0314/2, 0.0314/2 + 0.0022, 0.10435789657723259), //Anders Cable No 1 T3
        (6.0, 0.04145/2, 0.04145/2 + 0.01283, 0.46012825568418825), //Anders Cable No 3 T1
        (2.5, 0.0184/2, 0.0194/2, 0.02105715448), //CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 202 T1_SC
        (3.5, 0.0194/2, 0.0348/2, 0.3255045049), //CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 202 T1_I
        (2.5, 0.0348/2, 0.0358/2, 0.011272350425), //CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 202 T1_SI
        (6.0, 0.0358/2, 0.0368/2, 0.02630826604), //CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 202 T1_UWBT
        (6.0, 0.0386/2, 0.0392/2, 0.014729284181), //CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 203 T3_TOSW
        (3.5, 0.0392/2, 0.0436/2, 0.05925838476), //CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 203 T3_OC
        (2.5, 0.0436/2, 0.044/2, 0.003633699755), //CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 203 T3_OC_SC
      )

      forAll(cases) { (specificThermalResistivity, innerRadius, outerRadius, thermalResistance) =>
        val specTherRes = KelvinMetersPerWatt(specificThermalResistivity)
        val inRadius = Meters(innerRadius)
        val outRadius= Meters(outerRadius)
        val expectedThermalResistance = KelvinMetersPerWatt(thermalResistance)

        val actual = calcThermalResistanceCableShells(specTherRes, inRadius, outRadius)

        actual should approximate(expectedThermalResistance)
      }
    }
  }
  "return all correct thermal resistance for T4 / soil" in {

    val cases = Table(
      ("specificThermalResistivity", "depthCable", "cableDiameter", "thermalResistance"),
      (1.0, 1.0, 0.0729, 0.6373564504421266), //Anders 1997 Cable No 2 T4 p. 215
      //(1.0, 1.8, 0.1252, 1.276), //Anders Cable No 5 T4
      //(1.0, 1.0, 0.0358, 1.933), //Cable No 1 T4 Anders 2005 p. 311
      //(1.0, 1.0, 0.044, 1.8524966955), //CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 204 T4, Tree-foil touching
    )

    forAll(cases) { (specificThermalResistivity, depthCable, cableDiameter, thermalResistance) =>
      val specTherRes = KelvinMetersPerWatt(specificThermalResistivity)
      val depth = Meters(depthCable)
      val cableDia = Meters(cableDiameter)
      val expectedThermalResistance = KelvinMetersPerWatt(thermalResistance)

      val actual = calcThermalResistanceToSoil(specTherRes, depth, cableDia)

      actual should approximate(expectedThermalResistance)
    }
  }
}
