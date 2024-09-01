/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.models.result.NodeResult
import edu.ie3.datamodel.models.result.connector.LineResult
import edu.ie3.simona.agent.grid.CongestionManagementSupport.{
  TappingGroup,
  VoltageRange,
}
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.model.grid.GridModel.GridComponents
import edu.ie3.simona.model.grid.{TransformerTapping, VoltageLimits}
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
      val (transformer3wA, transformer3wB, transformer3wC) =
        mockTransformer3wModel()
      val transformer1 = mockTransformerModel()
      val ref1 = TestProbe[GridAgent.Request]("ref1").ref
      val ref2 = TestProbe[GridAgent.Request]("ref2").ref

      val ref3 = TestProbe[GridAgent.Request]("ref3").ref
      val transformer3 = mockTransformerModel()

      val ref4 = TestProbe[GridAgent.Request]("ref4").ref
      val transformer4a = mockTransformerModel()
      val transformer4b = mockTransformerModel()

      // grid 1 is connected via a transformer2w and one port of a transformer3w
      // grid 2 is connected via one port of a transformer3w
      // grid 3 is connected via a transformer2w
      // grid 4 is connected via two transformer2ws
      val receivedData
          : Map[ActorRef[GridAgent.Request], Set[TransformerTapping]] = Map(
        ref1 -> Set(
          transformer1,
          transformer3wB,
        ), // connected with both transformer2w and transformer3w
        ref2 -> Set(transformer3wC), // connected with a transformer3w
        ref3 -> Set(transformer3), // connected with just one transformer model
        ref4 -> Set(
          transformer4a,
          transformer4b,
        ), // connected with two transformer2w
      )

      val groups = groupTappingModels(
        receivedData,
        Set(transformer3wA),
      )

      // explanation for the expected groups:
      // since both grid 1 and grid 2 are connected by the same transformer3w they must be tapped by the same voltage delta
      // since grid 1 is also connected by transformer 1, both transformer are building a group together
      // the group contain the refs for both grids
      //
      // since grid 3 is only connected by a transformer2w, the group contains only this transformer and one ref
      //
      // since grid 4 is connected by two transformer2w, the group contains both transformers and the ref of grid 4
      groups shouldBe Set(
        TappingGroup(Set(ref1, ref2), Set(transformer1, transformer3wA)),
        TappingGroup(Set(ref3), Set(transformer3)),
        TappingGroup(Set(ref4), Set(transformer4a, transformer4b)),
      )
    }

    "calculate the tap and voltage change for one transformer" in {
      val tappingModel = dummyTappingModel()
      val tapping = dummyTransformerModel(tappingModel)

      val cases = Table(
        ("range", "expectedTap", "expectedDelta"),
        (VoltageRange(0.025.asPu, 0.015.asPu, 0.02.asPu), -1, 0.015.asPu),
        (
          VoltageRange((-0.015).asPu, (-0.025).asPu, (-0.02).asPu),
          1,
          (-0.015).asPu,
        ),
        (VoltageRange(0.041.asPu, 0.021.asPu, 0.031.asPu), -2, 0.03.asPu),
        (VoltageRange(0.05.asPu, 0.03.asPu, 0.05.asPu), -3, 0.045.asPu),
        (
          VoltageRange(0.asPu, (-0.2).asPu, (-0.1).asPu),
          4,
          (-0.06).asPu,
        ), // max tap increase
        (
          VoltageRange(0.2.asPu, 0.asPu, 0.1.asPu),
          -6,
          0.09.asPu,
        ), // max tap decrease
        (
          VoltageRange(0.015.asPu, 0.03.asPu, 0.15.asPu),
          -1,
          0.015.asPu,
        ),
        (
          VoltageRange((-0.04).asPu, (-0.03).asPu, (-0.03).asPu),
          2,
          (-0.03).asPu,
        ),
      )

      forAll(cases) { (range, expectedTap, expectedDelta) =>
        val (actualTap, actualDelta) =
          calculateTapAndVoltage(range, Seq(tapping))

        actualTap shouldBe Map(tapping -> expectedTap)
        actualDelta should equalWithTolerance(expectedDelta)
      }
    }

    "calculate the tap and voltage change for multiple transformers" in {
      val tappingModel1 = dummyTappingModel()
      val tappingModel2 = dummyTappingModel(
        deltaV = 1.2.asPercent,
        tapMin = -3,
        currentTapPos = 0,
      )

      val transformer11 = dummyTransformerModel(tappingModel1)
      val transformer12 = dummyTransformerModel(tappingModel1)

      val transformer21 = dummyTransformerModel(tappingModel2)
      val transformer22 = dummyTransformer3wModel(tappingModel2)

      val transformer31 = dummyTransformerModel(tappingModel1)
      val transformer32 = dummyTransformer3wModel(tappingModel2)

      val modelCase1 = Seq(transformer11, transformer12)
      val modelCase2 = Seq(transformer21, transformer22)
      val modelCase3 = Seq(transformer31, transformer32)

      val cases = Table(
        ("suggestion", "models", "expectedTaps", "expectedDelta"),
        (
          VoltageRange(0.1.asPu, (-0.1).asPu, 0.02.asPu),
          modelCase1,
          Map(transformer11 -> -1, transformer12 -> -1),
          0.015.asPu,
        ),
        (
          VoltageRange(0.1.asPu, (-0.1).asPu, 0.038.asPu),
          modelCase1,
          Map(transformer11 -> -3, transformer12 -> -3),
          0.045.asPu,
        ),
        (
          VoltageRange(0.1.asPu, (-0.1).asPu, (-0.06).asPu),
          modelCase1,
          Map(transformer11 -> 4, transformer12 -> 4),
          (-0.06).asPu,
        ),
        (
          VoltageRange(0.1.asPu, (-0.1).asPu, 0.02.asPu),
          modelCase2,
          Map(transformer21 -> -2, transformer22 -> -2),
          0.024.asPu,
        ),
        (
          VoltageRange(0.1.asPu, (-0.1).asPu, 0.038.asPu),
          modelCase2,
          Map(transformer21 -> -3, transformer22 -> -3),
          0.036.asPu,
        ),
        (
          VoltageRange(0.1.asPu, (-0.1).asPu, (-0.06).asPu),
          modelCase2,
          Map(transformer21 -> 5, transformer22 -> 5),
          (-0.06).asPu,
        ),
        (
          VoltageRange(0.1.asPu, (-0.1).asPu, 0.02.asPu),
          modelCase3,
          Map(transformer31 -> 0, transformer32 -> 0),
          0.asPu,
        ),
        (
          VoltageRange(0.1.asPu, (-0.1).asPu, 0.038.asPu),
          modelCase3,
          Map(transformer31 -> 0, transformer32 -> 0),
          0.asPu,
        ),
        (
          VoltageRange(0.1.asPu, (-0.1).asPu, (-0.06).asPu),
          modelCase3,
          Map(transformer31 -> 4, transformer32 -> 5),
          (-0.06).asPu,
        ),
        (
          VoltageRange(0.015.asPu, 0.05.asPu, 0.015.asPu),
          modelCase1,
          Map(transformer11 -> -1, transformer12 -> -1),
          0.015.asPu,
        ),
        (
          VoltageRange((-0.05).asPu, (-0.03).asPu, (-0.03).asPu),
          modelCase1,
          Map(transformer11 -> 2, transformer12 -> 2),
          (-0.03).asPu,
        ),
      )

      forAll(cases) { (range, models, expectedTaps, expectedDelta) =>
        val (tapChanges, delta) = calculateTapAndVoltage(range, models)

        tapChanges shouldBe expectedTaps
        delta should equalWithTolerance(expectedDelta)
      }

    }

    "calculate the common delta correctly" in {

      val cases = Table(
        ("suggestion", "possibleDeltas", "expected"),
        (0.015.asPu, Seq(List(0.03.asPu, 0.015.asPu, 0.asPu)), 0.015.asPu),
        (
          0.012.asPu,
          Seq(List(0.03.asPu, 0.02.asPu, 0.01.asPu, 0.asPu)),
          0.01.asPu,
        ),
        (0.006.asPu, Seq(List(0.03.asPu, 0.015.asPu, 0.asPu)), 0.asPu),
        (
          0.03.asPu,
          Seq(
            List(0.06.asPu, 0.03.asPu, 0.asPu),
            List(0.045.asPu, 0.03.asPu, 0.015.asPu, 0.asPu),
          ),
          0.03.asPu,
        ),
        (
          0.03.asPu,
          Seq(List(0.06.asPu, 0.03.asPu), List(0.03.asPu, 0.015.asPu)),
          0.03.asPu,
        ),
        (
          0.035.asPu,
          Seq(
            List(0.06.asPu, 0.03.asPu, 0.asPu),
            List(0.045.asPu, 0.03.asPu, 0.015.asPu, 0.asPu),
          ),
          0.03.asPu,
        ),
        (
          0.02.asPu,
          Seq(List(0.06.asPu, 0.03.asPu), List(0.03.asPu, 0.015.asPu)),
          0.03.asPu,
        ),
        (
          0.06.asPu,
          Seq(List(0.06.asPu, 0.03.asPu), List(0.03.asPu, 0.015.asPu)),
          0.asPu,
        ),
        (
          (-0.02).asPu,
          Seq(List(0.06.asPu, 0.03.asPu), List(0.03.asPu, 0.015.asPu)),
          0.asPu,
        ),
      )

      forAll(cases) { (suggestion, possibleDeltas, expected) =>
        val delta = findCommonDelta(suggestion, possibleDeltas)

        delta should equalWithTolerance(expected)
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
          (-0.0651).asPu, // max voltage decrease until line congestion occur
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

      val range = calculatePossibleVoltageRange(
        powerFlowResult,
        VoltageLimits(0.9, 1.1),
        gridComponents,
        Map.empty,
        subnetNo = 1,
      )

      range.deltaPlus should equalWithTolerance(0.05.asPu)
      range.deltaMinus should equalWithTolerance((-0.03).asPu)
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

      val tappingModel = mockTransformerTappingModel(
        autoTap = true,
        currentTapPos = 0,
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
      val range = calculatePossibleVoltageRange(
        powerFlowResult,
        VoltageLimits(0.9, 1.1),
        gridComponents,
        Map(
          inferior1.ref -> (VoltageRange(0.1.asPu, 0.01.asPu), Set(
            tappingModel
          )),
          inferior2.ref -> (VoltageRange(0.01.asPu, (-0.04).asPu), Set(
            tappingModel
          )),
        ),
        subnetNo = 1,
      )

      range.deltaPlus should equalWithTolerance(0.04.asPu)
      range.deltaMinus should equalWithTolerance((-0.02).asPu)
      range.suggestion should equalWithTolerance(0.asPu)
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
        (0.05.asPu, (-0.03).asPu, 0.asPu), // no voltage limit violation
        (
          (-0.01).asPu,
          (-0.02).asPu,
          (-0.015).asPu,
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
          (-0.02).asPu,
          (-0.01).asPu,
          (-0.01).asPu,
        ), // violation of both upper limit, lower < 0, decrease voltage to the lower limit
        (
          (-0.01).asPu,
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
          deltaV = 1.asPu,
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
            inferior1.ref -> (range1, Set(tappingModel)),
            inferior2.ref -> (range2, Set(tappingModel)),
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
        deltaV = 1.asPu,
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
          VoltageRange(0.03.asPu, (-0.05).asPu),
        ),
        (
          VoltageRange(0.02.asPu, 0.01.asPu),
          VoltageRange(0.04.asPu, (-0.01).asPu),
          VoltageRange(0.05.asPu, (-0.05).asPu),
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
  }
}
