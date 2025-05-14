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
import edu.ie3.simona.agent.grid.congestion.VoltageRange.calculatePossibleVoltageDeltaForLines
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
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import tech.units.indriya.ComparableQuantity

import javax.measure.quantity.Dimensionless

class VoltageRangeSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with GridComponentsMokka
    with ResultMokka
    with SubGridGateMokka
    with DbfsTestGrid
    with ConfigTestData {

  given puTolerance: ComparableQuantity[Dimensionless] = 1e-6.asPu

  protected val inferior1: TestProbe[GridAgent.Request] =
    TestProbe[GridAgent.Request]("inferior1")
  protected val inferior2: TestProbe[GridAgent.Request] =
    TestProbe[GridAgent.Request]("inferior2")

  "A VoltageRange" should {

    "calculate the suggestion correctly" in {
      val cases = Table(
        ("deltaPlus", "deltaMinus", "expected"),
        (0.05.asPu, -0.03.asPu, 0.asPu), // no voltage limit violation
        (
          -0.01.asPu,
          -0.02.asPu,
          -0.015.asPu,
        ), // upper voltage limit violation (both are negative), decreasing voltage
        (
          0.02.asPu,
          0.01.asPu,
          0.015.asPu,
        ), // lower voltage limit violation (both are positive), increasing voltage
        (
          0.01.asPu,
          0.02.asPu,
          0.01.asPu,
        ), // violation of both lower limit, upper > 0, increase voltage to the upper limit
        (
          -0.02.asPu,
          -0.01.asPu,
          -0.01.asPu,
        ), // violation of both upper limit, lower < 0, decrease voltage to the lower limit
        (
          -0.01.asPu,
          0.01.asPu,
          0.asPu,
        ), // violation of both voltage limits (upper negative, lower positive), do nothing
      )

      forAll(cases) { (deltaPlus, deltaMinus, expected) =>
        val suggestion = VoltageRange(
          deltaPlus,
          deltaMinus,
        ).suggestion

        suggestion should equalWithTolerance(expected)
      }
    }

    "calculates the possible voltage delta for lines correctly" in {
      val node1 = nodeModel()
      val node2 = nodeModel()
      val node3 = nodeModel()

      val line12 = lineModel(node1.uuid, node2.uuid)
      val line13 = lineModel(node1.uuid, node3.uuid)

      val gridComponents = GridComponents(
        Seq(node1, node2, node3),
        Set(line12, line13),
        Set.empty,
        Set.empty,
        Set.empty,
      )

      val cases = Table(
        ("results", "deltaV"),
        (
          buildPowerFlowResultEvent(
            Set(
              mockNodeResult(node1.uuid, 0.93.asPu),
              mockNodeResult(node2.uuid, 0.95.asPu),
              mockNodeResult(node3.uuid, 0.95.asPu),
            ),
            Set(
              mockLineResult(line12.uuid, 5.asAmpere, 5.asAmpere),
              mockLineResult(line13.uuid, 11.asAmpere, 10.9.asAmpere),
            ),
          ),
          0.093.asPu, // min voltage increase to resolve line congestion
        ),
        (
          buildPowerFlowResultEvent(
            Set(
              mockNodeResult(node1.uuid, 0.93.asPu),
              mockNodeResult(node2.uuid, 0.95.asPu),
              mockNodeResult(node3.uuid, 0.95.asPu),
            ),
            Set(
              mockLineResult(line12.uuid, 9.3.asAmpere, 9.2.asAmpere),
              mockLineResult(line13.uuid, 8.asAmpere, 8.asAmpere),
            ),
          ),
          -0.0651.asPu, // max voltage decrease until line congestion occur
        ),
      )

      forAll(cases) { (results, deltaV) =>
        val nodeResults = results.nodeResults
          .map(res => res.getInputModel -> res.getvMag())
          .toMap

        val actualDeltaV = calculatePossibleVoltageDeltaForLines(
          nodeResults,
          results.lineResults,
          gridComponents,
        )

        actualDeltaV should equalWithTolerance(deltaV)
      }
    }

    "calculate the voltage range for a lowest grid correctly" in {
      val node1 = nodeModel()
      val node2 = nodeModel()
      val node3 = nodeModel()
      val node4 = nodeModel()

      val line12 = lineModel(node1.uuid, node2.uuid)
      val line13 = lineModel(node1.uuid, node3.uuid)
      val line34 = lineModel(node3.uuid, node4.uuid)

      val gridComponents = GridComponents(
        Seq(node1, node2, node3, node4),
        Set(line12, line13, line34),
        Set.empty,
        Set.empty,
        Set.empty,
      )

      val powerFlowResult = buildPowerFlowResultEvent(
        Set(
          mockNodeResult(node1.uuid, 0.93.asPu),
          mockNodeResult(node2.uuid, 0.95.asPu),
          mockNodeResult(node3.uuid, 1.05.asPu),
          mockNodeResult(node4.uuid, 0.97.asPu),
        ),
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

      range.deltaPlus should equalWithTolerance(0.05.asPu)
      range.deltaMinus should equalWithTolerance(-0.03.asPu)
      range.suggestion should equalWithTolerance(0.asPu)
    }

    "calculates the voltage range for a middle grid correctly" in {
      val node1 = nodeModel()
      val node2 = nodeModel()
      val node3 = nodeModel()
      val node4 = nodeModel()

      val line12 = lineModel(node1.uuid, node2.uuid)
      val line13 = lineModel(node1.uuid, node3.uuid)
      val line34 = lineModel(node3.uuid, node4.uuid)

      val gridComponents = GridComponents(
        Seq(node1, node2, node3, node4),
        Set(line12, line13, line34),
        Set.empty,
        Set.empty,
        Set.empty,
      )

      val tappingModel = mockTransformerTapping(
        autoTap = true,
        tapMax = 3,
        tapMin = -3,
        deltaV = 1.asPu,
      )

      val powerFlowResult = buildPowerFlowResultEvent(
        Set(
          mockNodeResult(node1.uuid, 0.93.asPu),
          mockNodeResult(node2.uuid, 0.95.asPu),
          mockNodeResult(node3.uuid, 1.05.asPu),
          mockNodeResult(node4.uuid, 0.97.asPu),
        ),
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
          inferior1.ref -> (VoltageRange(0.1.asPu, 0.01.asPu), Set(
            tappingModel
          )),
          inferior2.ref -> (VoltageRange(0.01.asPu, -0.04.asPu), Set(
            tappingModel
          )),
        ),
        subnetNo = 1,
      )

      range.deltaPlus should equalWithTolerance(0.04.asPu)
      range.deltaMinus should equalWithTolerance(-0.02.asPu)
      range.suggestion should equalWithTolerance(0.asPu)
    }

    "be updated with a line voltage delta correctly" in {
      val range1 = VoltageRange(0.05.asPu, -0.05.asPu)
      val cases1 = Table(
        ("deltaV", "plus", "minus"),
        (0.01.asPu, 0.05.asPu, 0.01.asPu),
        (0.06.asPu, 0.05.asPu, 0.05.asPu),
        (-0.01.asPu, 0.05.asPu, -0.01.asPu),
        (-0.04.asPu, 0.05.asPu, -0.04.asPu),
        (-0.06.asPu, 0.05.asPu, -0.05.asPu),
      )

      forAll(cases1) { (deltaV, plus, minus) =>
        val updated = range1.updateWithLineDelta(deltaV)
        updated.deltaPlus should equalWithTolerance(plus)
        updated.deltaMinus should equalWithTolerance(minus)
      }

      val range2 = VoltageRange(-0.01.asPu, -0.05.asPu)
      val cases2 = Table(
        ("deltaV", "plus", "minus"),
        (0.01.asPu, -0.01.asPu, -0.01.asPu),
        (0.06.asPu, -0.01.asPu, -0.01.asPu),
        (-0.01.asPu, -0.01.asPu, -0.01.asPu),
        (-0.04.asPu, -0.01.asPu, -0.04.asPu),
        (-0.06.asPu, -0.01.asPu, -0.05.asPu),
      )

      forAll(cases2) { (deltaV, plus, minus) =>
        val updated = range2.updateWithLineDelta(deltaV)
        updated.deltaPlus should equalWithTolerance(plus)
        updated.deltaMinus should equalWithTolerance(minus)
      }

      val range3 = VoltageRange(0.05.asPu, 0.01.asPu)
      val cases3 = Table(
        ("deltaV", "plus", "minus"),
        (0.01.asPu, 0.05.asPu, 0.01.asPu),
        (0.06.asPu, 0.05.asPu, 0.05.asPu),
        (-0.01.asPu, 0.05.asPu, 0.01.asPu),
        (-0.04.asPu, 0.05.asPu, 0.01.asPu),
        (-0.06.asPu, 0.05.asPu, 0.01.asPu),
      )

      forAll(cases3) { (deltaV, plus, minus) =>
        val updated = range3.updateWithLineDelta(deltaV)
        updated.deltaPlus should equalWithTolerance(plus)
        updated.deltaMinus should equalWithTolerance(minus)
      }

    }

    "be updated with inferior voltage ranges and without tapping correctly" in {
      val range = VoltageRange(0.05.asPu, -0.05.asPu)

      val tappingModel =
        mockTransformerTapping(
          tapMax = 10,
          tapMin = -10,
          deltaV = 1.asPu,
        )

      val cases = Table(
        ("range1", "range2", "expected"),
        (
          VoltageRange(0.02.asPu, -0.06.asPu),
          VoltageRange(0.06.asPu, -0.03.asPu),
          VoltageRange(0.02.asPu, -0.03.asPu),
        ),
        (
          VoltageRange(0.06.asPu, -0.06.asPu),
          VoltageRange(0.06.asPu, -0.06.asPu),
          VoltageRange(0.05.asPu, -0.05.asPu),
        ),
        (
          VoltageRange(0.asPu, -0.01.asPu),
          VoltageRange(0.02.asPu, -0.03.asPu),
          VoltageRange(0.asPu, -0.01.asPu),
        ),
        (
          VoltageRange(0.02.asPu, 0.01.asPu),
          VoltageRange(0.04.asPu, -0.01.asPu),
          VoltageRange(0.02.asPu, 0.01.asPu),
        ),
      )

      forAll(cases) { (range1, range2, expected) =>
        val updatedRange = range.updateWithInferiorRanges(
          Map(
            inferior1.ref -> (range1, Set(tappingModel)),
            inferior2.ref -> (range2, Set(tappingModel)),
          )
        )

        updatedRange.deltaPlus should equalWithTolerance(expected.deltaPlus)
        updatedRange.deltaMinus should equalWithTolerance(expected.deltaMinus)
      }
    }

    "be updated with inferior voltage ranges and with tapping correctly" in {
      val range = VoltageRange(0.05.asPu, -0.05.asPu)

      val tappingModel = mockTransformerTapping(
        autoTap = true,
        currentTapPos = 7,
        tapMax = 10,
        tapMin = -10,
        deltaV = 1.asPu,
      )

      val cases = Table(
        ("range1", "range2", "expected"),
        (
          VoltageRange(0.02.asPu, -0.06.asPu),
          VoltageRange(0.06.asPu, -0.03.asPu),
          VoltageRange(0.05.asPu, -0.05.asPu),
        ),
        (
          VoltageRange(0.06.asPu, -0.06.asPu),
          VoltageRange(0.06.asPu, -0.06.asPu),
          VoltageRange(0.05.asPu, -0.05.asPu),
        ),
        (
          VoltageRange(0.asPu, -0.01.asPu),
          VoltageRange(0.02.asPu, -0.03.asPu),
          VoltageRange(0.03.asPu, -0.05.asPu),
        ),
        (
          VoltageRange(0.02.asPu, 0.01.asPu),
          VoltageRange(0.04.asPu, -0.01.asPu),
          VoltageRange(0.05.asPu, -0.05.asPu),
        ),
      )

      forAll(cases) { (range1, range2, expected) =>
        val updatedRange = range.updateWithInferiorRanges(
          Map(
            inferior1.ref -> (range1, Set(tappingModel)),
            inferior2.ref -> (range2, Set(tappingModel)),
          )
        )

        updatedRange.deltaPlus should equalWithTolerance(expected.deltaPlus)
        updatedRange.deltaMinus should equalWithTolerance(expected.deltaMinus)
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
