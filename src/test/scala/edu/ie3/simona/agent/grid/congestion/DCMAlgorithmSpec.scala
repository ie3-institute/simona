/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.congestion.data.CongestionManagementData
import edu.ie3.simona.agent.grid.congestion.detection.DetectionMessages.CongestionCheckRequest
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.test.common.UnitSpec
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  BehaviorTestKit,
  ScalaTestWithActorTestKit,
  TestProbe,
}

import scala.concurrent.duration.DurationInt

class DCMAlgorithmSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with CongestionTestBaseData {

  "The DCMAlgorithm" should {

    "start the congestion management correctly" in {
      val inferiorGA = TestProbe[GridAgent.Request]("inferiorGridAgent")

      val baseData = gridAgentBaseData(inferiorRefs = Set(inferiorGA.ref))

      // behavior, that will start the congestion management by creating the state data and checking the result for congestions
      // we need to spawn the behavior here, since we send a start message internally
      testKit.spawn(
        behaviorWithContextAndBuffer { (ctx, buffer) =>
          GridAgent.startCongestionManagement(
            baseData,
            3600L,
            Some(
              PowerFlowResultEvent(
                Iterable(nodeResult1),
                Iterable.empty,
                Iterable(lineResult23),
                Iterable.empty,
                Iterable.empty,
              )
            ),
            ctx,
          )(using constantData, buffer)
        }
      )

      // we should receive a request to check for congestions
      inferiorGA.expectMessageType[CongestionCheckRequest](10.seconds)
    }

    "finish the congestion management correctly" in {
      val stateData = CongestionManagementData(
        gridAgentBaseData(),
        3600,
        100,
        PowerFlowResultEvent(
          Iterable(nodeResult1),
          Iterable.empty,
          Iterable(lineResult23),
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
      resultListener.expectMessageType[PowerFlowResultEvent] match {
        case PowerFlowResultEvent(
              nodeResults,
              _,
              lineResults,
              _,
              _,
              congestionResults,
            ) =>
          nodeResults shouldBe Iterable(nodeResult1)
          lineResults shouldBe Iterable(lineResult23)
      }

      // we should receive a next tick of 7200
      val completionMsg = scheduler.expectMessageType[Completion]
      completionMsg.actor shouldBe gridAgentActivation.ref
      completionMsg.newTick shouldBe Some(7200)
    }

  }
}
