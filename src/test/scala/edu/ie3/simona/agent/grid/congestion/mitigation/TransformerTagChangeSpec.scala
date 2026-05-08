/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion.mitigation

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.congestion.mitigations.TappingMessages.{
  RequestVoltageOptions,
  VoltageRangeResponse,
}
import edu.ie3.simona.agent.grid.congestion.{
  CongestedComponents,
  CongestionTestBaseData,
  Congestions,
  VoltageRange,
}
import edu.ie3.simona.agent.grid.data.CongestionManagementData
import edu.ie3.simona.agent.grid.data.GridAgentData.AwaitingData
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.model.grid.TransformerTapping
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.model.grid.GridComponentsMokka
import edu.ie3.simona.test.common.result.CongestedComponentsTestData
import edu.ie3.simona.test.matchers.QuantityMatchers
import edu.ie3.simona.util.ReceiveDataMap
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import squants.{Dimensionless, Each}

class TransformerTagChangeSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with CongestionTestBaseData
    with CongestedComponentsTestData
    with QuantityMatchers
    with GridComponentsMokka {

  protected given puTolerance: Dimensionless = Each(1e-3)

  private type AwaitedData = (VoltageRange, Set[TransformerTapping])

  protected val superiorAgent: TestProbe[GridAgent.Message] = TestProbe(
    "superiorAgent"
  )
  protected val inferiorAgent: TestProbe[GridAgent.Message] = TestProbe(
    "inferiorAgent"
  )

  "The congestion mitigation by transformer tap change" should {
    val gridComponents = defaultGridModel.gridComponents

    val transformers: Set[TransformerTapping] =
      (gridComponents.transformers ++ gridComponents.transformers3w).map {
        (transformerTapping: TransformerTapping) => transformerTapping
      }

    def spawnCenterAgent(
        stateData: CongestionManagementData,
        awaitingData: AwaitingData[(VoltageRange, Set[TransformerTapping])],
        capacity: Int = 10,
    ): ActorRef[GridAgent.Message] = testKit.spawn(
      Behaviors.withStash[GridAgent.Message](capacity) { buffer =>
        GridAgent.updateTransformerTapping(
          stateData,
          awaitingData,
        )(using constantData, buffer)
      }
    )

    "answer a request for voltage options" in {
      val stateData = CongestionManagementData(
        gridAgentBaseData(gridModel = Some(defaultGridModel)),
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
      )

      // the map is empty, since the inferior grid itself has no inferior grids
      val awaitingData: AwaitingData[AwaitedData] = ReceiveDataMap.empty

      val centerAgent = spawnCenterAgent(stateData, awaitingData)

      centerAgent ! RequestVoltageOptions(superiorAgent.ref, 1000)

      val (voltageRange, actualTransformers) =
        superiorAgent.expectMessageType[VoltageRangeResponse].value

      voltageRange.possibleIncrease should approximate(Each(-0.01))
      voltageRange.possibleDecrease should approximate(Each(-0.01))
      voltageRange.suggestion should approximate(Each(-0.011))

      actualTransformers shouldBe transformers
    }

    "wait to answer a request for voltage options, if inferior data is still missing" in {
      val stateData = CongestionManagementData(
        gridAgentBaseData(gridModel = Some(defaultGridModel)),
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
      )

      // the grid, that receives the request, is a center grid, which has to wait for its inferior data
      val awaitingData: AwaitingData[AwaitedData] =
        ReceiveDataMap(Set(inferiorAgent.ref))

      val centerAgent = spawnCenterAgent(stateData, awaitingData)

      centerAgent ! RequestVoltageOptions(superiorAgent.ref, 1000)

      val mockedMvLvTappingModel = mockTransformerTapping()

      // the request will be stashed and answered after inferior data was received
      centerAgent ! VoltageRangeResponse(
        inferiorAgent.ref,
        (VoltageRange(Each(0.04), Each(-0.05)), Set(mockedMvLvTappingModel)),
      )

      val (voltageRange, actualTransformers) =
        superiorAgent.expectMessageType[VoltageRangeResponse].value

      voltageRange.possibleIncrease should approximate(Each(0.04))
      voltageRange.possibleDecrease should approximate(Each(-0.01))
      voltageRange.suggestion should approximate(Each(0.0))

      actualTransformers shouldBe transformers

    }

  }

}
