/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion.mitigation

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.congestion.data.{
  AwaitingData,
  CongestionManagementData,
}
import edu.ie3.simona.agent.grid.congestion.mitigations.MitigationProgress
import edu.ie3.simona.agent.grid.congestion.mitigations.TappingMessages.{
  ReceivedVoltageRange,
  RequestVoltageOptions,
  VoltageRangeResponse,
}
import edu.ie3.simona.agent.grid.congestion.{
  CongestedComponents,
  CongestionTestBaseData,
  Congestions,
  VoltageRange,
}
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.model.grid.{GridModel, TransformerTapping}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.model.grid.GridComponentsMokka
import edu.ie3.simona.test.common.result.CongestedComponentsTestData
import edu.ie3.simona.test.matchers.QuantityMatchers
import edu.ie3.util.quantities.QuantityUtils.{asPercent, asPu}
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import tech.units.indriya.ComparableQuantity

import javax.measure.quantity.Dimensionless

class TransformerTagChangeSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with CongestionTestBaseData
    with CongestedComponentsTestData
    with QuantityMatchers
    with GridComponentsMokka {

  protected given puTolerance: ComparableQuantity[Dimensionless] = 1e-3.asPu

  private type AwaitedData = (VoltageRange, Set[TransformerTapping])

  protected val superiorAgent: TestProbe[GridAgent.Request] = TestProbe(
    "superiorAgent"
  )
  protected val inferiorAgent: TestProbe[GridAgent.Request] = TestProbe(
    "inferiorAgent"
  )

  "The congestion mitigation by transformer tap change" should {
    val gridModel = GridModel(
      hvGridContainer,
      refSystem,
      voltageLimits,
      startTime,
      endTime,
      config,
    )

    val transformers: Set[TransformerTapping] =
      gridModel.gridComponents.transformers.map {
        (transformerTapping: TransformerTapping) => transformerTapping
      }

    def spawnCenterAgent(
        stateData: CongestionManagementData,
        awaitingData: AwaitingData[(VoltageRange, Set[TransformerTapping])],
        capacity: Int = 10,
    ): ActorRef[GridAgent.Request] = testKit.spawn(
      Behaviors.withStash[GridAgent.Request](capacity) { buffer =>
        GridAgent.updateTransformerTapping(
          stateData,
          awaitingData,
        )(using constantData, buffer)
      }
    )

    "answer a request for voltage options" in {
      val stateData = CongestionManagementData(
        gridAgentBaseData(gridModel = Some(gridModel)),
        3600,
        100,
        PowerFlowResultEvent(
          Iterable(
            nodeResult1,
            nodeResult2,
            nodeResult3,
            nodeResult4,
            nodeResultA,
            nodeResultB,
          ),
          Iterable.empty,
          Iterable(lineResult12),
          Iterable.empty,
          Iterable.empty,
        ),
        Congestions(
          voltageCongestions = true,
          lineCongestions = false,
          transformerCongestions = false,
        ),
        CongestedComponents.empty,
        MitigationProgress(),
      )

      // the map is empty, since the inferior grid itself has no inferior grids
      val awaitingData = AwaitingData(
        Map.empty[ActorRef[GridAgent.Request], Option[AwaitedData]]
      )

      val centerAgent = spawnCenterAgent(stateData, awaitingData)

      centerAgent ! RequestVoltageOptions(superiorAgent.ref, 1)

      val (voltageRange, actualTransformers) =
        superiorAgent.expectMessageType[VoltageRangeResponse].value

      voltageRange.deltaPlus should equalWithTolerance(-0.01.asPu)
      voltageRange.deltaMinus should equalWithTolerance(-0.01.asPu)
      voltageRange.suggestion should equalWithTolerance(-0.011.asPu)

      actualTransformers shouldBe transformers
    }

    "wait to answer a request for voltage options, if inferior data is still missing" in {
      val stateData = CongestionManagementData(
        gridAgentBaseData(gridModel = Some(gridModel)),
        3600,
        100,
        PowerFlowResultEvent(
          Iterable(
            nodeResult3,
            nodeResult4,
            nodeResultA,
            nodeResultB,
          ),
          Iterable.empty,
          Iterable(lineResult34),
          Iterable.empty,
          Iterable.empty,
        ),
        Congestions(
          voltageCongestions = true,
          lineCongestions = false,
          transformerCongestions = false,
        ),
        CongestedComponents.empty,
        MitigationProgress(),
      )

      // the grid, that receives the request, is a center grid, which has to wait for its inferior data
      val awaitingData: AwaitingData[AwaitedData] =
        AwaitingData(Set(inferiorAgent.ref))

      val centerAgent = spawnCenterAgent(stateData, awaitingData)

      centerAgent ! RequestVoltageOptions(superiorAgent.ref, 1)

      val mockedMvLvTappingModel = mockTransformerTapping()

      // the request will be stashed and answered after inferior data was received
      centerAgent ! ReceivedVoltageRange(
        Seq(
          (
            inferiorAgent.ref,
            (
              VoltageRange(0.04.asPu, -0.05.asPu),
              Set(mockedMvLvTappingModel),
            ),
          )
        )
      )

      val (voltageRange, actualTransformers) =
        superiorAgent.expectMessageType[VoltageRangeResponse].value

      voltageRange.deltaPlus should equalWithTolerance(0.04.asPu)
      voltageRange.deltaMinus should equalWithTolerance(0.01.asPu)
      voltageRange.suggestion should equalWithTolerance(0.026.asPu)

      actualTransformers shouldBe transformers

    }

  }

}
