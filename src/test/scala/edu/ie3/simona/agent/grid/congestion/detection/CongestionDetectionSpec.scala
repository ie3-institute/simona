/*
 * © 2025-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion.detection

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.congestion.CongestionManagementMessages.{
  GotoIdle,
  StartStep,
}
import edu.ie3.simona.agent.grid.congestion.detection.DetectionMessages.{
  CongestionCheckRequest,
  CongestionResponse,
}
import edu.ie3.simona.agent.grid.congestion.{
  CongestedComponents,
  CongestionTestBaseData,
  Congestions,
}
import edu.ie3.simona.agent.grid.data.CongestionManagementData
import edu.ie3.simona.agent.grid.data.GridAgentData.AwaitingData
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.util.ReceiveDataMap
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef

import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions

class CongestionDetectionSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with CongestionTestBaseData {

  val superiorAgent: TestProbe[GridAgent.Message] = TestProbe("superiorAgent")
  val inferiorAgent: TestProbe[GridAgent.Message] = TestProbe("inferiorAgent")

  "The congestion detection" should {

    "answer a request for congestions correctly" in {
      val stateData = CongestionManagementData(
        gridAgentBaseData(inferiorRefs = Set(inferiorAgent.ref)),
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

      val emptyAwaitingData: AwaitingData[Congestions] =
        ReceiveDataMap(Set(inferiorAgent.ref))

      val cases = Table(
        ("awaitingData", "expectedCongestions"),
        (
          ReceiveDataMap.empty,
          Congestions(
            voltageCongestions = true,
            lineCongestions = false,
            transformerCongestions = false,
          ),
        ),
        (
          emptyAwaitingData.addData(
            inferiorAgent.ref,
            Congestions(
              voltageCongestions = true,
              lineCongestions = false,
              transformerCongestions = false,
            ),
          ),
          Congestions(
            voltageCongestions = true,
            lineCongestions = false,
            transformerCongestions = false,
          ),
        ),
        (
          emptyAwaitingData.addData(
            inferiorAgent.ref,
            Congestions(
              voltageCongestions = false,
              lineCongestions = true,
              transformerCongestions = false,
            ),
          ),
          Congestions(
            voltageCongestions = true,
            lineCongestions = true,
            transformerCongestions = false,
          ),
        ),
      )

      forAll(cases) { (awaitingData, expectedCongestions) =>
        val behavior = spawnWithBuffer(
          GridAgent.checkForCongestion(
            stateData,
            awaitingData,
          )(using constantData, _)
        )

        behavior ! CongestionCheckRequest(superiorAgent.ref)

        val congestions =
          superiorAgent.expectMessageType[CongestionResponse].value
        congestions shouldBe expectedCongestions
      }
    }

    "wait to answer a request for congestions if inferior data is still missing" in {
      val stateData = CongestionManagementData(
        gridAgentBaseData(inferiorRefs = Set(inferiorAgent.ref)),
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

      val awaitingData: AwaitingData[Congestions] =
        ReceiveDataMap(Set(inferiorAgent.ref))

      val behavior = spawnWithBuffer(
        GridAgent.checkForCongestion(
          stateData,
          awaitingData,
        )(using constantData, _)
      )

      behavior ! CongestionCheckRequest(superiorAgent.ref)

      // the request will be stashed and answered after inferior data was received
      behavior ! CongestionResponse(
        inferiorAgent.ref,
        Congestions(
          voltageCongestions = false,
          lineCongestions = true,
          transformerCongestions = false,
        ),
      )

      val congestions =
        superiorAgent.expectMessageType[CongestionResponse](30.seconds).value
      congestions shouldBe Congestions(
        voltageCongestions = true,
        lineCongestions = true,
        transformerCongestions = false,
      )
    }

    "work as expected in center position" in {
      val stateData = CongestionManagementData(
        gridAgentBaseData(Set(inferiorAgent.ref)),
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

      val awaitingData: AwaitingData[Congestions] =
        ReceiveDataMap(Set(inferiorAgent.ref))

      // init behavior
      val centerGridAgent = spawnWithBuffer(
        GridAgent.checkForCongestion(
          stateData,
          awaitingData,
        )(using constantData, _)
      )

      // we will send the center grid agent a StartStep message to start the detection
      centerGridAgent ! StartStep

      // normally, the superior grid agent would send a CongestionCheckRequest
      // we mock this behavior by sending it manually
      centerGridAgent ! CongestionCheckRequest(superiorAgent.ref)

      // the center grid agent will request congestions from inferior grids
      val sender = inferiorAgent
        .expectMessageType[CongestionCheckRequest](30.seconds)
        .sender

      sender ! CongestionResponse(
        inferiorAgent.ref,
        Congestions(
          voltageCongestions = false,
          lineCongestions = true,
          transformerCongestions = false,
        ),
      )

      // after the center grid receives the responses,
      // all congestions will be combined and send to the superior grid
      val allCongestions =
        superiorAgent.expectMessageType[CongestionResponse]
      allCongestions.value shouldBe Congestions(
        voltageCongestions = true,
        lineCongestions = true,
        transformerCongestions = false,
      )

      // normally the superior agent would receive a GotoIdle message from the coordinator
      // and this message is then forwarded to all other agents
      // here we send the message manually to the center grid agent, since the superior agent is just a mock
      centerGridAgent ! GotoIdle

      inferiorAgent.expectMessageType[GotoIdle.type]
    }

    "work as expected in superior position" in {
      val stateData = CongestionManagementData(
        gridAgentBaseData(Set(inferiorAgent.ref), isSuperior = true),
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

      val awaitingData: AwaitingData[Congestions] =
        ReceiveDataMap(Set(inferiorAgent.ref))

      // init behavior
      val superiorGridAgent = spawnWithBuffer(
        GridAgent.checkForCongestion(
          stateData,
          awaitingData,
        )(using constantData, _)
      )

      // we will send the center grid agent a StartStep message to start the detection
      superiorGridAgent ! StartStep

      // the center grid agent will request congestions from inferior grids
      val sender = inferiorAgent
        .expectMessageType[CongestionCheckRequest](30.seconds)
        .sender

      // we answer the request
      sender ! CongestionResponse(
        inferiorAgent.ref,
        Congestions(
          voltageCongestions = false,
          lineCongestions = true,
          transformerCongestions = false,
        ),
      )

      // the coordinator will send a GotoIdle message
      superiorGridAgent ! GotoIdle

      // we expect a GotoIdle message
      inferiorAgent.expectMessageType[GotoIdle.type]
    }
  }
}
