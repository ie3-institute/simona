/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.control

import edu.ie3.simona.agent.grid.congestion.VoltageRange
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.model.grid.TransformerTapping
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.model.grid.GridComponentsMokka
import edu.ie3.util.quantities.QuantityUtils.{asPercent, asPu}
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef

class TappingGroupModelSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with GridComponentsMokka {

  val voltageTolerance = 1e-3

  val inferior1: TestProbe[GridAgent.Request] =
    TestProbe[GridAgent.Request]("inferior1")
  val inferior2: TestProbe[GridAgent.Request] =
    TestProbe[GridAgent.Request]("inferior2")

  "TappingGroupModel" should {

    "group transformers correctly" in {
      val (transformer3wA, transformer3wB, transformer3wC) =
        mockTransformer3wModel()
      val transformer1 = mockTransformerModel()
      val ref1 = TestProbe[GridAgent.Request]("ref1").ref
      val ref2 = TestProbe[GridAgent.Request]("ref2").ref

      val ref3 = TestProbe[GridAgent.Request]("ref3").ref
      val transformer3 = mockTransformerModel(hasAutoTap = true)

      val ref4 = TestProbe[GridAgent.Request]("ref4").ref
      val transformer4a = mockTransformerModel(hasAutoTap = true)
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

      val groups = TappingGroupModel.buildModels(
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
        TappingGroupModel(
          Set(transformer1, transformer3wA),
          Set(ref1, ref2),
          hasAutoTap = false,
        ),
        TappingGroupModel(Set(transformer3), Set(ref3), hasAutoTap = true),
        TappingGroupModel(
          Set(transformer4a, transformer4b),
          Set(ref4),
          hasAutoTap = false,
        ),
      )
    }

    "calculate the tap and voltage change for one transformer" in {
      val tappingModel = dummyTappingModel()
      val tapping = dummyTransformerModel(tappingModel)

      val group = TappingGroupModel(
        Set(tapping),
        Set.empty,
      )

      val cases = Table(
        ("range", "expectedTap", "expectedDelta"),
        (VoltageRange(0.025.asPu, 0.015.asPu, 0.02.asPu), -1, 0.015.asPu),
        (
          VoltageRange(-0.015.asPu, -0.025.asPu, -0.02.asPu),
          1,
          -0.015.asPu,
        ),
        (VoltageRange(0.041.asPu, 0.021.asPu, 0.031.asPu), -2, 0.03.asPu),
        (VoltageRange(0.05.asPu, 0.03.asPu, 0.05.asPu), -3, 0.045.asPu),
        (
          VoltageRange(0.asPu, -0.2.asPu, -0.1.asPu),
          4,
          -0.06.asPu,
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
          VoltageRange(-0.04.asPu, -0.03.asPu, -0.03.asPu),
          2,
          -0.03.asPu,
        ),
      )

      forAll(cases) { (range, expectedTap, expectedDelta) =>
        val (actualTap, actualDelta) =
          group.calculateTapAndVoltage(range)

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

      val transformer11: TransformerTapping =
        dummyTransformerModel(tappingModel1)
      val transformer12: TransformerTapping =
        dummyTransformerModel(tappingModel1)

      val transformer21: TransformerTapping =
        dummyTransformerModel(tappingModel2)
      val transformer22: TransformerTapping =
        dummyTransformer3wModel(tappingModel2)

      val transformer31: TransformerTapping =
        dummyTransformerModel(tappingModel1)
      val transformer32: TransformerTapping =
        dummyTransformer3wModel(tappingModel2)

      val modelCase1 = Set(transformer11, transformer12)
      val modelCase2 = Set(transformer21, transformer22)
      val modelCase3 = Set(transformer31, transformer32)

      val cases = Table(
        ("suggestion", "models", "expectedTaps", "expectedDelta"),
        (
          VoltageRange(0.1.asPu, -0.1.asPu, 0.02.asPu),
          modelCase1,
          Map(transformer11 -> -1, transformer12 -> -1),
          0.015.asPu,
        ),
        (
          VoltageRange(0.1.asPu, -0.1.asPu, 0.038.asPu),
          modelCase1,
          Map(transformer11 -> -3, transformer12 -> -3),
          0.045.asPu,
        ),
        (
          VoltageRange(0.1.asPu, -0.1.asPu, -0.06.asPu),
          modelCase1,
          Map(transformer11 -> 4, transformer12 -> 4),
          -0.06.asPu,
        ),
        (
          VoltageRange(0.1.asPu, -0.1.asPu, 0.02.asPu),
          modelCase2,
          Map(transformer21 -> -2, transformer22 -> -2),
          0.024.asPu,
        ),
        (
          VoltageRange(0.1.asPu, -0.1.asPu, 0.038.asPu),
          modelCase2,
          Map(transformer21 -> -3, transformer22 -> -3),
          0.036.asPu,
        ),
        (
          VoltageRange(0.1.asPu, -0.1.asPu, -0.06.asPu),
          modelCase2,
          Map(transformer21 -> 5, transformer22 -> 5),
          -0.06.asPu,
        ),
        (
          VoltageRange(0.1.asPu, -0.1.asPu, 0.02.asPu),
          modelCase3,
          Map(transformer31 -> 0, transformer32 -> 0),
          0.asPu,
        ),
        (
          VoltageRange(0.1.asPu, -0.1.asPu, 0.038.asPu),
          modelCase3,
          Map(transformer31 -> 0, transformer32 -> 0),
          0.asPu,
        ),
        (
          VoltageRange(0.1.asPu, -0.1.asPu, -0.06.asPu),
          modelCase3,
          Map(transformer31 -> 4, transformer32 -> 5),
          -0.06.asPu,
        ),
        (
          VoltageRange(0.015.asPu, 0.05.asPu, 0.015.asPu),
          modelCase1,
          Map(transformer11 -> -1, transformer12 -> -1),
          0.015.asPu,
        ),
        (
          VoltageRange(-0.05.asPu, -0.03.asPu, -0.03.asPu),
          modelCase1,
          Map(transformer11 -> 2, transformer12 -> 2),
          -0.03.asPu,
        ),
      )

      forAll(cases) { (range, models, expectedTaps, expectedDelta) =>
        val group = TappingGroupModel(
          models,
          Set.empty,
        )

        val (tapChanges, delta) = group.calculateTapAndVoltage(range)

        tapChanges shouldBe expectedTaps
        delta should equalWithTolerance(expectedDelta)
      }

    }

    "calculate the common delta correctly" in {
      val dummyGroup =
        TappingGroupModel(Set.empty, Set.empty, hasAutoTap = false)

      val cases = Table(
        ("suggestion", "possibleDeltas", "expected"),
        (0.015.asPu, Set(List(0.03.asPu, 0.015.asPu, 0.asPu)), 0.015.asPu),
        (
          0.012.asPu,
          Set(List(0.03.asPu, 0.02.asPu, 0.01.asPu, 0.asPu)),
          0.01.asPu,
        ),
        (0.006.asPu, Set(List(0.03.asPu, 0.015.asPu, 0.asPu)), 0.asPu),
        (
          0.03.asPu,
          Set(
            List(0.06.asPu, 0.03.asPu, 0.asPu),
            List(0.045.asPu, 0.03.asPu, 0.015.asPu, 0.asPu),
          ),
          0.03.asPu,
        ),
        (
          0.03.asPu,
          Set(List(0.06.asPu, 0.03.asPu), List(0.03.asPu, 0.015.asPu)),
          0.03.asPu,
        ),
        (
          0.035.asPu,
          Set(
            List(0.06.asPu, 0.03.asPu, 0.asPu),
            List(0.045.asPu, 0.03.asPu, 0.015.asPu, 0.asPu),
          ),
          0.03.asPu,
        ),
        (
          0.02.asPu,
          Set(List(0.06.asPu, 0.03.asPu), List(0.03.asPu, 0.015.asPu)),
          0.03.asPu,
        ),
        (
          0.06.asPu,
          Set(List(0.06.asPu, 0.03.asPu), List(0.03.asPu, 0.015.asPu)),
          0.asPu,
        ),
        (
          -0.02.asPu,
          Set(List(0.06.asPu, 0.03.asPu), List(0.03.asPu, 0.015.asPu)),
          0.asPu,
        ),
      )

      forAll(cases) { (suggestion, possibleDeltas, expected) =>
        val delta = dummyGroup.findCommonDelta(suggestion, possibleDeltas)

        delta should equalWithTolerance(expected)
      }
    }

  }

}
