/*
 * © 2024-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.grid.TransformerTappingSupport.getTappingOptions
import edu.ie3.simona.model.grid.TransformerTapping
import edu.ie3.simona.test.common.model.grid.{
  DbfsTestGrid,
  GridComponentsMokka,
  SubGridGateMokka,
}
import edu.ie3.simona.test.common.result.ResultMokka
import edu.ie3.simona.test.common.{ConfigTestData, UnitSpec}
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import squants.{Dimensionless, Each, Percent}

class TransformerTappingSupportSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with GridComponentsMokka
    with ResultMokka
    with SubGridGateMokka
    with DbfsTestGrid
    with ConfigTestData {

  private given Dimensionless = Each(1e-3)

  protected val inferior1: TestProbe[GridAgent.Message] =
    TestProbe[GridAgent.Message]("inferior1")
  protected val inferior2: TestProbe[GridAgent.Message] =
    TestProbe[GridAgent.Message]("inferior2")

  "TransformerTappingSupportSpec" should {

    "get tapping options correctly" in {
      val tappingModel1: TransformerTapping = mockTransformerTapping(
        autoTap = true,
        currentTapPos = 3,
        tapMax = 4,
        deltaV = Percent(1),
      )

      val tappingModel2: TransformerTapping = mockTransformerTapping(
        autoTap = true,
        currentTapPos = 1,
        tapMax = 3,
        tapMin = -2,
        deltaV = Percent(1),
      )

      val cases = Table(
        ("tappings", "expectedPlus", "expectedMinus"),
        (Set(tappingModel1), 0.08, -0.01),
        (Set(tappingModel2), 0.03, -0.02),
        (Set(tappingModel1, tappingModel2), 0.03, -0.01),
      )

      forAll(cases) { (tappings, expectedPlus, expectedMinus) =>
        val (actualPlus, actualMinus) = getTappingOptions(tappings)

        actualPlus should approximate(Each(expectedPlus))
        actualMinus should approximate(Each(expectedMinus))
      }
    }
  }
}
