/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.models.result.NodeResult
import edu.ie3.datamodel.models.result.connector.LineResult
import edu.ie3.simona.agent.grid.CongestionManagementSupport.VoltageRange
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.model.grid.GridModel.GridComponents
import edu.ie3.simona.model.grid.{
  TransformerModel,
  TransformerTappingModel,
  VoltageLimits,
}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.model.grid.{
  GridComponentsMokka,
  SubGridGateMokka,
}
import edu.ie3.simona.test.common.result.ResultMokka
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.OperationInterval
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import squants.{Amperes, Each}

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

    "group transformers correctly" in {
      // grid 1 is connected via a transformer2w and one port of a transformer3w
      // grid 2 is connected via one port of a transformer3w
      // grid 3 is connected via a transformer2w
      // grid 4 is connected via two transformer2ws

      val (transformer3wA, transformer3wB, transformer3wC) =
        mockTransformer3wModel()
      val transformer1 = mockTransformerModel()
      val ref1 = TestProbe[GridAgent.Request]("ref1").ref
      val ref2 = TestProbe[GridAgent.Request]("ref2").ref

      val ref3 = TestProbe[GridAgent.Request]("ref3").ref
      val transformer3 = mockTransformerModel()

      val ref4 = TestProbe[GridAgent.Request]("ref4").ref
      val transformer4_1 = mockTransformerModel()
      val transformer4_2 = mockTransformerModel()

      val receivedData = Map(
        ref1 -> Seq(
          transformer1,
          transformer3wB,
        ), // connected with both transformer2w and transformer3w
        ref2 -> Seq(transformer3wC), // connected with a transformer3w
        ref3 -> Seq(transformer3), // connected with just one transformer model
        ref4 -> Seq(
          transformer4_1,
          transformer4_2,
        ), // connected with two transformer2w
      )

      val grouped = groupTappingModels(
        receivedData,
        Set(transformer3wA),
      )

      grouped shouldBe Map(
        Set(transformer1, transformer3wA) -> Set(ref1, ref2),
        Set(transformer3) -> Set(ref3),
        Set(transformer4_1, transformer4_2) -> Set(ref4),
      )

    }

    "calculate the tap and voltage change for one transformer" in {
      val tappingModel = dummyTappingModel()
      val tapping = dummyTransformerModel(tappingModel)

      val cases = Table(
        ("suggestion", "expectedTap", "expectedDelta"),
        (0.02.asPu, 1, 0.015.asPu),
        ((-0.02).asPu, -1, (-0.015).asPu),
        (0.031.asPu, 2, 0.03.asPu),
        (0.05.asPu, 3, 0.045.asPu),
        ((-0.06).asPu, -4, (-0.06).asPu),
        ((-0.1).asPu, -6, (-0.09).asPu), // max decrease
        (0.1.asPu, 4, 0.06.asPu), // max increase
      )

      forAll(cases) { (suggestion, expectedTap, expectedDelta) =>
        val (actualTap, actualDelta) =
          calculateTapAndVoltage(suggestion, Seq(tapping))

        actualTap shouldBe Map(tapping -> expectedTap)
        actualDelta should equalWithTolerance(expectedDelta)
      }
    }

    "calculate the tap and voltage change for multiple transformers" in {
      val tappingModel1 = dummyTappingModel()
      val tappingModel2 = dummyTappingModel(
        deltaV = 1.2.asPercent,
        tapMin = -5,
        tapMax = 3,
        currentTapPos = 0,
      )
      val tappingModel3 = dummyTappingModel(deltaV = 1.49.asPercent)

      val transformer11 = dummyTransformerModel(tappingModel1)
      val transformer12 = dummyTransformerModel(tappingModel1)

      val transformer21 = dummyTransformerModel(tappingModel2)
      val transformer22 = dummyTransformer3wModel(tappingModel2)

      val transformer31 = dummyTransformerModel(tappingModel1)
      val transformer32 = dummyTransformer3wModel(tappingModel2)

      val transformer41 = dummyTransformerModel(tappingModel1)
      val transformer42 = dummyTransformer3wModel(tappingModel3)

      val modelCase1 = Seq(transformer11, transformer12)
      val modelCase2 = Seq(transformer21, transformer22)
      val modelCase3 = Seq(transformer31, transformer32)
      val modelCase4 = Seq(transformer41, transformer42)

      val cases = Table(
        ("suggestion", "models", "expectedTaps", "expectedDelta"),
        (
          0.02.asPu,
          modelCase1,
          Map(transformer11 -> 1, transformer12 -> 1),
          0.015.asPu,
        ),
        (
          0.038.asPu,
          modelCase1,
          Map(transformer11 -> 2, transformer12 -> 2),
          0.03.asPu,
        ),
        (
          (-0.06).asPu,
          modelCase1,
          Map(transformer11 -> -4, transformer12 -> -4),
          (-0.06).asPu,
        ),
        (
          0.02.asPu,
          modelCase2,
          Map(transformer21 -> 1, transformer22 -> 1),
          0.012.asPu,
        ),
        (
          0.038.asPu,
          modelCase2,
          Map(transformer21 -> 3, transformer22 -> 3),
          0.036.asPu,
        ),
        (
          (-0.06).asPu,
          modelCase2,
          Map(transformer21 -> -5, transformer22 -> -5),
          (-0.06).asPu,
        ),
        (
          0.02.asPu,
          modelCase3,
          Map(transformer31 -> 0, transformer32 -> 0),
          0.asPu,
        ),
        (
          0.038.asPu,
          modelCase3,
          Map(transformer31 -> 0, transformer32 -> 0),
          0.asPu,
        ),
        (
          (-0.06).asPu,
          modelCase3,
          Map(transformer31 -> -4, transformer32 -> -5),
          (-0.06).asPu,
        ),
        (
          0.02.asPu,
          modelCase4,
          Map(transformer41 -> 1, transformer42 -> 1),
          0.0149.asPu,
        ),
        (
          0.038.asPu,
          modelCase4,
          Map(transformer41 -> 2, transformer42 -> 2),
          0.0298.asPu,
        ),
        (
          (-0.06).asPu,
          modelCase4,
          Map(transformer41 -> -4, transformer42 -> -4),
          (-0.0596).asPu,
        ),
      )

      forAll(cases) { (suggestion, models, expectedTaps, expectedDelta) =>
        val (tapChanges, delta) = calculateTapAndVoltage(suggestion, models)

        tapChanges shouldBe expectedTaps
        delta should equalWithTolerance(expectedDelta)
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
      )

      range.deltaPlus should equalWithTolerance(0.05.asPu)
      range.deltaMinus should equalWithTolerance((-0.03).asPu)
      range.suggestion should equalWithTolerance(0.041.asPu)
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

      val tappingModel = mockTransformerTappingModel(
        autoTap = true,
        currentTapPos = 0,
        tapMax = 3,
        tapMin = -3,
        deltaV = 0.01.asPu,
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
        Map(
          inferior1.ref -> (VoltageRange(0.1.asPu, 0.01.asPu), Seq(
            tappingModel
          )),
          inferior2.ref -> (VoltageRange(0.01.asPu, (-0.04).asPu), Seq(
            tappingModel
          )),
        ),
      )

      range.deltaPlus should equalWithTolerance(0.04.asPu)
      range.deltaMinus should equalWithTolerance((-0.02).asPu)
      range.suggestion should equalWithTolerance(0.041.asPu)
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
        (0.05.asPu, (-0.03).asPu, 0.04.asPu), // no voltage limit violation
        (
          (-0.01).asPu,
          (-0.02).asPu,
          (-0.015).asPu,
        ), // upper voltage limit violation, decreasing voltage
        (
          0.02.asPu,
          0.01.asPu,
          0.015.asPu,
        ), // lower voltage limit violation, increasing voltage
        (
          (-0.01).asPu,
          0.01.asPu,
          (-0.01).asPu,
        ), // violation of both voltage limits, decreasing voltage
      )

      forAll(cases) { (deltaPlus, deltaMinus, expected) =>
        val suggestion = VoltageRange(
          deltaPlus,
          deltaMinus,
        ).suggestion

        suggestion should equalWithTolerance(expected)
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
        mockTransformerTappingModel(
          autoTap = false,
          currentTapPos = 0,
          tapMax = 10,
          tapMin = -10,
          deltaV = 0.01.asPu,
        )

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
          Map(
            inferior1.ref -> (range1, Seq(tappingModel)),
            inferior2.ref -> (range2, Seq(tappingModel)),
          )
        )

        updatedRange.deltaPlus should equalWithTolerance(expected.deltaPlus)
        updatedRange.deltaMinus should equalWithTolerance(expected.deltaMinus)
      }
    }

    "be updated with inferior voltage ranges and with tapping correctly" in {
      val range = VoltageRange(0.05.asPu, (-0.05).asPu)

      val tappingModel = mockTransformerTappingModel(
        autoTap = true,
        currentTapPos = 7,
        tapMax = 10,
        tapMin = -10,
        deltaV = 0.01.asPu,
      )

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
          Map(
            inferior1.ref -> (range1, Seq(tappingModel)),
            inferior2.ref -> (range2, Seq(tappingModel)),
          )
        )

        updatedRange.deltaPlus should equalWithTolerance(expected.deltaPlus)
        updatedRange.deltaMinus should equalWithTolerance(expected.deltaMinus)
      }
    }
  }
}
