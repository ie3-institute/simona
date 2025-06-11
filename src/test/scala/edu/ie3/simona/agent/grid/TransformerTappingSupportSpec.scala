/*
 * © 2024. TU Dortmund University,
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
import edu.ie3.util.quantities.QuantityUtils.asPu
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}

class TransformerTappingSupportSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with GridComponentsMokka
    with ResultMokka
    with SubGridGateMokka
    with DbfsTestGrid
    with ConfigTestData {

  val voltageTolerance = 1e-3

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
        deltaV = 1.asPu,
      )

      val tappingModel2: TransformerTapping = mockTransformerTapping(
        autoTap = true,
        currentTapPos = 1,
        tapMax = 3,
        tapMin = -2,
        deltaV = 1.asPu,
      )

      val cases = Table(
        ("tappings", "expectedPlus", "expectedMinus"),
        (Set(tappingModel1), 0.08.asPu, -0.01.asPu),
        (Set(tappingModel2), 0.03.asPu, -0.02.asPu),
        (Set(tappingModel1, tappingModel2), 0.03.asPu, -0.01.asPu),
      )

      forAll(cases) { (tappings, expectedPlus, expectedMinus) =>
        val (actualPlus, actualMinus) = getTappingOptions(tappings)

        actualPlus shouldBe expectedPlus
        actualMinus shouldBe expectedMinus
      }
    }
  }
}
