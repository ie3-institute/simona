/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import edu.ie3.datamodel.models.result.NodeResult
import edu.ie3.datamodel.models.result.connector.LineResult
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.congestion.VoltageRange
import edu.ie3.simona.agent.grid.congestion.VoltageRange.calculateVoltageDeltaFromLineCurrent
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.model.grid.GridModel.GridComponents
import edu.ie3.simona.model.grid.VoltageLimits
import edu.ie3.simona.test.common.model.grid.{
  DbfsTestGrid,
  GridComponentsMokka,
  SubGridGateMokka,
}
import edu.ie3.simona.test.common.result.ResultMokka
import edu.ie3.simona.test.common.{ConfigTestData, UnitSpec}
import edu.ie3.util.quantities.QuantityUtils.{asAmpere, asPu}
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroPU
import edu.ie3.util.scala.quantities.QuantityConversionUtils.toSquants
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import squants.{Dimensionless, Each, Percent}

class VoltageRangeSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with GridComponentsMokka
    with ResultMokka
    with SubGridGateMokka
    with DbfsTestGrid
    with ConfigTestData {

  private given Conversion[Double, squants.Dimensionless] = (d: Double) =>
    Each(d)

  private given squants.Dimensionless = Each(1e-3)

  protected val inferior1: TestProbe[GridAgent.Message] =
    TestProbe[GridAgent.Message]("inferior1")
  protected val inferior2: TestProbe[GridAgent.Message] =
    TestProbe[GridAgent.Message]("inferior2")

  "A VoltageRange" should {

    val node1 = nodeModel()
    val node2 = nodeModel()
    val node3 = nodeModel()
    val node4 = nodeModel()

    val nodeResults = Set(
      mockNodeResult(node1.uuid, 0.93.asPu),
      mockNodeResult(node2.uuid, 0.95.asPu),
      mockNodeResult(node3.uuid, 0.97.asPu),
      mockNodeResult(node4.uuid, 1.05.asPu),
    )

    "calculate the suggestion correctly" in {
      val cases = Table(
        ("deltaPlus", "deltaMinus", "expected"),
        // no voltage limit violation
        (0.05, -0.03, 0d),
        // upper voltage limit violation (both are negative), decreasing voltage
        (-0.01, -0.02, -0.015),
        // lower voltage limit violation (both are positive), increasing voltage
        (0.02, 0.01, 0.015),
        // violation of both lower limit, upper > 0, increase voltage to the maximal possible voltage increase
        (0.01, 0.02, 0.01),
        // violation of both upper limit, lower < 0, decrease voltage to the lower limit
        (-0.02, -0.01, -0.01),
        // violation of both voltage limits (upper negative, lower positive), do nothing
        (-0.01, 0.01, 0d),
      )

      forAll(cases) { (deltaPlus, deltaMinus, expected) =>
        val suggestion = VoltageRange(deltaPlus, deltaMinus).suggestion

        suggestion should approximate(Each(expected))
      }
    }

    "calculates the possible voltage delta for lines correctly" in {
      val line12 = lineModel(node1.uuid, node2.uuid)
      val line13 = lineModel(node1.uuid, node3.uuid)

      val gridComponents = GridComponents(
        Seq(node1, node2, node3),
        Set(line12, line13),
        Set.empty,
        Set.empty,
        Set.empty,
        Set.empty,
        Seq.empty,
      )

      val cases = Table(
        ("results", "deltaV"),
        (
          buildPowerFlowResultEvent(
            nodeResults,
            Set(
              mockLineResult(line12.uuid, 5.asAmpere, 5.asAmpere),
              mockLineResult(line13.uuid, 11.asAmpere, 10.9.asAmpere),
            ),
          ),
          0.093, // min voltage increase to resolve line congestion
        ),
        (
          buildPowerFlowResultEvent(
            nodeResults,
            Set(
              mockLineResult(line12.uuid, 9.3.asAmpere, 9.2.asAmpere),
              mockLineResult(line13.uuid, 8.asAmpere, 8.asAmpere),
            ),
          ),
          -0.0651, // max voltage decrease until line congestion occur
        ),
      )

      forAll(cases) { (results, deltaV) =>
        val nodeResults = results.nodeResults
          .map(res => res.getInputModel -> res.getvMag.toSquants)
          .toMap

        val actualDeltaV = calculateVoltageDeltaFromLineCurrent(
          nodeResults,
          results.lineResults,
          gridComponents,
        )

        actualDeltaV should approximate(Each(deltaV))
      }
    }

    "calculate the voltage range for a lowest grid correctly" in {
      val line12 = lineModel(node1.uuid, node2.uuid)
      val line13 = lineModel(node1.uuid, node3.uuid)
      val line34 = lineModel(node3.uuid, node4.uuid)

      val gridComponents = GridComponents(
        Seq(node1, node2, node3, node4),
        Set(line12, line13, line34),
        Set.empty,
        Set.empty,
        Set.empty,
        Set.empty,
        Seq.empty,
      )

      val powerFlowResult = buildPowerFlowResultEvent(
        nodeResults,
        Set(
          mockLineResult(line12.uuid, 5.asAmpere, 5.asAmpere),
          mockLineResult(line13.uuid, 8.asAmpere, 8.asAmpere),
          mockLineResult(line34.uuid, 7.asAmpere, 7.asAmpere),
        ),
      )

      val range = VoltageRange(
        powerFlowResult,
        VoltageLimits(0.9, 1.1),
        gridComponents,
        Map.empty,
        subnetNo = 1,
      )

      range.possibleIncrease should approximate(Each(0.05))
      range.possibleDecrease should approximate(Each(-0.03))
      range.suggestion should approximate(zeroPU)
    }

    "calculates the voltage range for a middle grid correctly" in {
      val line12 = lineModel(node1.uuid, node2.uuid)
      val line13 = lineModel(node1.uuid, node3.uuid)
      val line34 = lineModel(node3.uuid, node4.uuid)

      val gridComponents = GridComponents(
        Seq(node1, node2, node3, node4),
        Set(line12, line13, line34),
        Set.empty,
        Set.empty,
        Set.empty,
        Set.empty,
        Seq.empty,
      )

      val tappingModel = mockTransformerTapping(
        autoTap = true,
        tapMax = 3,
        tapMin = -3,
        deltaV = Percent(1),
      )

      val powerFlowResult = buildPowerFlowResultEvent(
        nodeResults,
        Set(
          mockLineResult(line12.uuid, 5.asAmpere, 5.asAmpere),
          mockLineResult(line13.uuid, 8.asAmpere, 8.asAmpere),
          mockLineResult(line34.uuid, 7.asAmpere, 7.asAmpere),
        ),
      )

      // the voltage range of the given grid is limited by the voltage range
      // of the inferior grids and the possible transformer tapping
      val range = VoltageRange(
        powerFlowResult,
        VoltageLimits(0.9, 1.1),
        gridComponents,
        Map(
          inferior1.ref -> (VoltageRange(0.1, 0.01), Set(
            tappingModel
          )),
          inferior2.ref -> (VoltageRange(0.01, -0.04), Set(
            tappingModel
          )),
        ),
        subnetNo = 1,
      )

      range.possibleIncrease should approximate(Each(0.04))
      range.possibleDecrease should approximate(Each(-0.02))
      range.suggestion should approximate(zeroPU)
    }

    "be updated with a line voltage delta correctly" in {
      val range1 = VoltageRange(0.05, -0.05)
      val cases1 = Table(
        ("deltaV", "plus", "minus"),
        (0.01, 0.05, 0.01),
        (0.06, 0.05, 0.05),
        (-0.01, 0.05, -0.01),
        (-0.04, 0.05, -0.04),
        (-0.06, 0.05, -0.05),
      )

      forAll(cases1) { (deltaV, plus, minus) =>
        val updated = range1.updateWithVoltageDelta(deltaV)
        updated.possibleIncrease should approximate(Each(plus))
        updated.possibleDecrease should approximate(Each(minus))
      }

      val range2 = VoltageRange(-0.01, -0.05)
      val cases2 = Table(
        ("deltaV", "plus", "minus"),
        (0.01, -0.01, -0.01),
        (0.06, -0.01, -0.01),
        (-0.01, -0.01, -0.01),
        (-0.04, -0.01, -0.04),
        (-0.06, -0.01, -0.05),
      )

      forAll(cases2) { (deltaV, plus, minus) =>
        val updated = range2.updateWithVoltageDelta(deltaV)
        updated.possibleIncrease should approximate(Each(plus))
        updated.possibleDecrease should approximate(Each(minus))
      }

      val range3 = VoltageRange(0.05, 0.01)
      val cases3 = Table(
        ("deltaV", "plus", "minus"),
        (0.01, 0.05, 0.01),
        (0.06, 0.05, 0.05),
        (-0.01, 0.05, 0.01),
        (-0.04, 0.05, 0.01),
        (-0.06, 0.05, 0.01),
      )

      forAll(cases3) { (deltaV, plus, minus) =>
        val updated = range3.updateWithVoltageDelta(deltaV)
        updated.possibleIncrease should approximate(Each(plus))
        updated.possibleDecrease should approximate(Each(minus))
      }

    }

    "be updated with inferior voltage ranges and without tapping correctly" in {
      val range = VoltageRange(0.05, -0.05)

      val tappingModel =
        mockTransformerTapping(
          tapMax = 10,
          tapMin = -10,
          deltaV = Percent(1),
        )

      val cases = Table(
        ("range1", "range2", "expected"),
        (
          VoltageRange(0.02, -0.06),
          VoltageRange(0.06, -0.03),
          VoltageRange(0.02, -0.03),
        ),
        (
          VoltageRange(0.06, -0.06),
          VoltageRange(0.06, -0.06),
          VoltageRange(0.05, -0.05),
        ),
        (
          VoltageRange(0d, -0.01),
          VoltageRange(0.02, -0.03),
          VoltageRange(0d, -0.01),
        ),
        (
          VoltageRange(0.02, 0.01),
          VoltageRange(0.04, -0.01),
          VoltageRange(0.02, 0.01),
        ),
      )

      forAll(cases) { (range1, range2, expected) =>
        val updatedRange = range.updateWithInferiorRanges(
          Map(
            inferior1.ref -> (range1, Set(tappingModel)),
            inferior2.ref -> (range2, Set(tappingModel)),
          )
        )

        updatedRange.possibleIncrease should approximate(
          expected.possibleIncrease
        )
        updatedRange.possibleDecrease should approximate(
          expected.possibleDecrease
        )
      }
    }

    "be updated with inferior voltage ranges and with tapping correctly" in {
      val range = VoltageRange(0.05, -0.05)

      val tappingModel = mockTransformerTapping(
        autoTap = true,
        currentTapPos = 7,
        tapMax = 10,
        tapMin = -10,
        deltaV = Percent(1),
      )

      val cases = Table(
        ("range1", "range2", "expected"),
        (
          VoltageRange(0.02, -0.06),
          VoltageRange(0.06, -0.03),
          VoltageRange(0.05, -0.05),
        ),
        (
          VoltageRange(0.06, -0.06),
          VoltageRange(0.06, -0.06),
          VoltageRange(0.05, -0.05),
        ),
        (
          VoltageRange(0d, -0.01),
          VoltageRange(0.02, -0.03),
          VoltageRange(0.03, -0.05),
        ),
        (
          VoltageRange(0.02, 0.01),
          VoltageRange(0.04, -0.01),
          VoltageRange(0.05, -0.05),
        ),
      )

      forAll(cases) { (range1, range2, expected) =>
        val updatedRange = range.updateWithInferiorRanges(
          Map(
            inferior1.ref -> (range1, Set(tappingModel)),
            inferior2.ref -> (range2, Set(tappingModel)),
          )
        )

        updatedRange.possibleIncrease should approximate(
          expected.possibleIncrease
        )
        updatedRange.possibleDecrease should approximate(
          expected.possibleDecrease
        )
      }
    }

    def buildPowerFlowResultEvent(
        nodeResults: Set[NodeResult],
        lineResults: Set[LineResult],
    ): PowerFlowResultEvent = {
      PowerFlowResultEvent(
        nodeResults,
        Set.empty,
        lineResults,
        Set.empty,
        Set.empty,
      )
    }
  }

}
