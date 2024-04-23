/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.datamodel.models.input.connector.ConnectorPort
import edu.ie3.datamodel.models.result.NodeResult
import edu.ie3.datamodel.models.result.connector.LineResult
import edu.ie3.simona.agent.grid.CongestionManagementSupport.VoltageRange
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.model.grid.GridModel.GridComponents
import edu.ie3.simona.model.grid.{TransformerTappingModel, VoltageLimits}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.model.grid.{
  GridComponentsMokka,
  SubGridGateMokka,
}
import edu.ie3.simona.test.common.result.ResultMokka
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef

import java.util.UUID

class CongestionManagementSupportSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with GridComponentsMokka
    with ResultMokka
    with SubGridGateMokka
    with CongestionManagementSupport {

  val voltageTolerance = 1e-3

  val inferior1: TestProbe[GridAgent.Request] =
    TestProbe[GridAgent.Request]("inferior1")
  val inferior2: TestProbe[GridAgent.Request] =
    TestProbe[GridAgent.Request]("inferior2")

  "CongestionManagementSupport" should {

    val inferiorGrids: Seq[ActorRef[GridAgent.Request]] =
      Seq(inferior1.ref, inferior2.ref)

    "map all transformers to inferior grids" in {
      val nodeHv = mockNode(UUID.randomUUID(), 100)
      val node30kV = mockNode(UUID.randomUUID(), 30)
      val node20kV = mockNode(UUID.randomUUID(), 20)
      val node10kV = mockNode(UUID.randomUUID(), 10)

      val transformer2w = mockTransformer2w(UUID.randomUUID(), nodeHv, node30kV)
      val transformer3w =
        mockTransformer3w(UUID.randomUUID(), nodeHv, 100, node20kV, node10kV)

      val transformer1 = mockTransformerModel(transformer2w.getUuid)
      val transformer2 = mockTransformer3wModel(transformer3w.getUuid)

      val gridGates = Map(
        SubGridGate.fromTransformer2W(transformer2w) -> inferior1.ref,
        SubGridGate.fromTransformer3W(
          transformer3w,
          ConnectorPort.B,
        ) -> inferior2.ref,
      )

      val gridComponents = GridComponents(
        Seq.empty,
        Set.empty,
        Set(transformer1, mockTransformerModel(UUID.randomUUID())),
        Set(transformer2),
        Set.empty,
      )

      val map = getTransformer(
        inferiorGrids,
        gridGates,
        gridComponents,
      )

      map shouldBe Map(
        inferior1.ref -> transformer1,
        inferior2.ref -> transformer2,
      )
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
          0.093.asPu,
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
          (-0.0651).asPu,
        ),
      )

      forAll(cases) { (results, deltaV) =>
        val nodeResults = results.nodeResults
          .map(res => res.getInputModel -> res.getvMag())
          .toMap

        calculatePossibleVoltageDeltaForLines(
          nodeResults,
          results.lineResults,
          gridComponents,
        ) should equalWithTolerance(deltaV, 1e-3)
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

      val range = calculateVoltageOptions(
        powerFlowResult,
        VoltageLimits(0.9, 1.1),
        gridComponents,
        Map.empty,
        Map.empty,
      )

      range.deltaPlus should equalWithTolerance(0.05.asPu)
      range.deltaMinus should equalWithTolerance((-0.03).asPu)
      range.suggestion should equalWithTolerance(0.02.asPu)
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

      val tappingModel =
        TransformerTappingModel(0.01.asPu, 0, 3, -3, 0, autoTap = true)
      val tappingMap =
        Map(inferior1.ref -> tappingModel, inferior2.ref -> tappingModel)

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

      val range = calculateVoltageOptions(
        powerFlowResult,
        VoltageLimits(0.9, 1.1),
        gridComponents,
        tappingMap,
        Map(
          inferior1.ref -> VoltageRange(0.1.asPu, 0.01.asPu),
          inferior2.ref -> VoltageRange(0.01.asPu, (-0.04).asPu),
        ),
      )

      range.deltaPlus should equalWithTolerance(0.04.asPu)
      range.deltaMinus should equalWithTolerance((-0.02).asPu)
      range.suggestion should equalWithTolerance(0.02.asPu)
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

  "A VoltageRange" should {

    "calculate the suggestion correctly" in {
      val cases = Table(
        ("deltaPlus", "deltaMinus", "expected"),
        (0.05.asPu, (-0.05).asPu, 0.asPu), // no voltage limit violation
        (
          (-0.01).asPu,
          (-0.02).asPu,
          (-0.01).asPu,
        ), // upper voltage limit violation, decreasing voltage
        (
          0.02.asPu,
          0.01.asPu,
          0.01.asPu,
        ), // lower voltage limit violation, increasing voltage
        (
          (-0.01).asPu,
          0.01.asPu,
          (-0.01).asPu,
        ), // violation of both voltage limits, decreasing voltage
      )

      forAll(cases) { (deltaPlus, deltaMinus, expected) =>
        VoltageRange(
          deltaPlus,
          deltaMinus,
        ).suggestion should equalWithTolerance(expected)
      }

    }

    "be updated with a line voltage delta correctly" in {
      val range1 = VoltageRange(0.05.asPu, (-0.05).asPu)
      val cases1 = Table(
        ("deltaV", "plus", "minus"),
        (0.01.asPu, 0.05.asPu, 0.01.asPu),
        (0.06.asPu, 0.05.asPu, 0.05.asPu),
        ((-0.01).asPu, 0.05.asPu, (-0.01).asPu),
        ((-0.04).asPu, 0.05.asPu, (-0.04).asPu),
        ((-0.06).asPu, 0.05.asPu, (-0.05).asPu),
      )

      forAll(cases1) { (deltaV, plus, minus) =>
        val updated = range1.updateWithLineDelta(deltaV)
        updated.deltaPlus should equalWithTolerance(plus)
        updated.deltaMinus should equalWithTolerance(minus)
      }

      val range2 = VoltageRange((-0.01).asPu, (-0.05).asPu)
      val cases2 = Table(
        ("deltaV", "plus", "minus"),
        (0.01.asPu, (-0.01).asPu, (-0.01).asPu),
        (0.06.asPu, (-0.01).asPu, (-0.01).asPu),
        ((-0.01).asPu, (-0.01).asPu, (-0.01).asPu),
        ((-0.04).asPu, (-0.01).asPu, (-0.04).asPu),
        ((-0.06).asPu, (-0.01).asPu, (-0.05).asPu),
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
        ((-0.01).asPu, 0.05.asPu, 0.01.asPu),
        ((-0.04).asPu, 0.05.asPu, 0.01.asPu),
        ((-0.06).asPu, 0.05.asPu, 0.01.asPu),
      )

      forAll(cases3) { (deltaV, plus, minus) =>
        val updated = range3.updateWithLineDelta(deltaV)
        updated.deltaPlus should equalWithTolerance(plus)
        updated.deltaMinus should equalWithTolerance(minus)
      }

    }

    "be updated with inferior voltage ranges and without tapping correctly" in {
      val range = VoltageRange(0.05.asPu, (-0.05).asPu)

      val tappingModel =
        TransformerTappingModel(0.15.asPu, 0, 10, -10, 0, autoTap = false)
      val tappingMap =
        Map(inferior1.ref -> tappingModel, inferior2.ref -> tappingModel)

      val cases = Table(
        ("range1", "range2", "expected"),
        (
          VoltageRange(0.02.asPu, (-0.06).asPu),
          VoltageRange(0.06.asPu, (-0.03).asPu),
          VoltageRange(0.02.asPu, (-0.03).asPu),
        ),
        (
          VoltageRange(0.06.asPu, (-0.06).asPu),
          VoltageRange(0.06.asPu, (-0.06).asPu),
          VoltageRange(0.05.asPu, (-0.05).asPu),
        ),
        (
          VoltageRange(0.asPu, (-0.01).asPu),
          VoltageRange(0.02.asPu, (-0.03).asPu),
          VoltageRange(0.asPu, (-0.01).asPu),
        ),
        (
          VoltageRange(0.02.asPu, 0.01.asPu),
          VoltageRange(0.04.asPu, (-0.01).asPu),
          VoltageRange(0.02.asPu, 0.01.asPu),
        ),
      )

      forAll(cases) { (range1, range2, expected) =>
        val updatedRange = range.updateWithInferiorRanges(
          tappingMap,
          Map(inferior1.ref -> range1, inferior2.ref -> range2),
        )

        updatedRange.deltaPlus should equalWithTolerance(expected.deltaPlus)
        updatedRange.deltaMinus should equalWithTolerance(expected.deltaMinus)
      }
    }

    "be updated with inferior voltage ranges and with tapping correctly" in {
      val range = VoltageRange(0.05.asPu, (-0.05).asPu)

      val tappingModel =
        TransformerTappingModel(0.01.asPu, 7, 10, -10, 0, autoTap = true)
      val tappingMap =
        Map(inferior1.ref -> tappingModel, inferior2.ref -> tappingModel)

      val cases = Table(
        ("range1", "range2", "expected"),
        (
          VoltageRange(0.02.asPu, (-0.06).asPu),
          VoltageRange(0.06.asPu, (-0.03).asPu),
          VoltageRange(0.05.asPu, (-0.05).asPu),
        ),
        (
          VoltageRange(0.06.asPu, (-0.06).asPu),
          VoltageRange(0.06.asPu, (-0.06).asPu),
          VoltageRange(0.05.asPu, (-0.05).asPu),
        ),
        (
          VoltageRange(0.asPu, (-0.01).asPu),
          VoltageRange(0.02.asPu, (-0.03).asPu),
          VoltageRange(0.05.asPu, (-0.04).asPu),
        ),
        (
          VoltageRange(0.02.asPu, 0.01.asPu),
          VoltageRange(0.04.asPu, (-0.01).asPu),
          VoltageRange(0.05.asPu, (-0.02).asPu),
        ),
      )

      forAll(cases) { (range1, range2, expected) =>
        val updatedRange = range.updateWithInferiorRanges(
          tappingMap,
          Map(inferior1.ref -> range1, inferior2.ref -> range2),
        )

        updatedRange.deltaPlus should equalWithTolerance(expected.deltaPlus)
        updatedRange.deltaMinus should equalWithTolerance(expected.deltaMinus)
      }
    }
  }
}
