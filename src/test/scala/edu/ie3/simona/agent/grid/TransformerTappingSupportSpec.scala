/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.result.NodeResult
import edu.ie3.datamodel.models.result.connector.{
  LineResult,
  Transformer2WResult,
}
import edu.ie3.simona.agent.grid.TransformerTappingSupport.getTappingOptions
import edu.ie3.simona.agent.grid.congestion.VoltageRange.calculatePossibleVoltageDeltaForLines
import edu.ie3.simona.agent.grid.congestion.{Congestions, VoltageRange}
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.model.grid.GridModel.GridComponents
import edu.ie3.simona.model.grid.{
  GridModel,
  RefSystem,
  TransformerTapping,
  VoltageLimits,
}
import edu.ie3.simona.test.common.model.grid.{
  DbfsTestGrid,
  GridComponentsMokka,
  SubGridGateMokka,
}
import edu.ie3.simona.test.common.result.ResultMokka
import edu.ie3.simona.test.common.{ConfigTestData, UnitSpec}
import edu.ie3.util.quantities.PowerSystemUnits.PU
import edu.ie3.util.quantities.QuantityUtils.asPu
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import squants.electro.Kilovolts
import squants.energy.Kilowatts
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime

class TransformerTappingSupportSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with GridComponentsMokka
    with ResultMokka
    with SubGridGateMokka
    with DbfsTestGrid
    with ConfigTestData {

  val voltageTolerance = 1e-3

  protected val inferior1: TestProbe[GridAgent.Request] =
    TestProbe[GridAgent.Request]("inferior1")
  protected val inferior2: TestProbe[GridAgent.Request] =
    TestProbe[GridAgent.Request]("inferior2")

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
