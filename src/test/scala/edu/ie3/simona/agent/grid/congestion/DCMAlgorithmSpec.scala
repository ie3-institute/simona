/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.congestion.data.CongestionManagementData
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.test.common.UnitSpec
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  BehaviorTestKit,
  ScalaTestWithActorTestKit,
}

class DCMAlgorithmSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with CongestionTestBaseData {

  "The DCMAlgorithm" should {

    "finish the congestion management correctly" in {
      val stateData = CongestionManagementData(
        gridAgentBaseData(),
        3600,
        100,
        PowerFlowResultEvent(
          Iterable.empty,
          Iterable.empty,
          Iterable.empty,
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

      // this will return the idle behavior of the grid agent
      BehaviorTestKit(
        behaviorWithContextAndBuffer { (ctx, buffer) =>
          GridAgent.finishCongestionManagement(
            stateData,
            ctx,
          )(using constantData, buffer)
        }
      )

      // we should receive an empty result event
      resultListener.expectMessageType[PowerFlowResultEvent]

      // we should receive a next tick of 7200
      val completionMsg = scheduler.expectMessageType[Completion]
      completionMsg.actor shouldBe gridAgentActivation.ref
      completionMsg.newTick shouldBe Some(7200)
    }

  }
}
