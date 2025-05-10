/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion.detection

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.congestion.CongestionManagementMessages.{
  FinishStep,
  StartStep,
}
import edu.ie3.simona.agent.grid.congestion.data.{
  AwaitingData,
  CongestionManagementData,
}
import edu.ie3.simona.agent.grid.congestion.detection.DetectionMessages.{
  CongestionCheckRequest,
  CongestionResponse,
  ReceivedCongestions,
}
import edu.ie3.simona.agent.grid.congestion.{
  CongestedComponents,
  CongestionTestBaseData,
  Congestions,
}
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.test.common.UnitSpec
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

  val superiorAgent: TestProbe[GridAgent.Request] = TestProbe("superiorAgent")
  val inferiorAgent: TestProbe[GridAgent.Request] = TestProbe("inferiorAgent")

  "The congestion detection" should {

    "answer a request for congestions correctly" in {
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

      val cases = Table(
        ("inferiorData", "expectedCongestions"),
        (
          Map.empty[ActorRef[GridAgent.Request], Option[Congestions]],
          Congestions(
            voltageCongestions = true,
            lineCongestions = false,
            transformerCongestions = false,
          ),
        ),
        (
          Map(
            inferiorAgent.ref -> Some(
              Congestions(
                voltageCongestions = true,
                lineCongestions = false,
                transformerCongestions = false,
              )
            )
          ),
          Congestions(
            voltageCongestions = true,
            lineCongestions = false,
            transformerCongestions = false,
          ),
        ),
        (
          Map(
            inferiorAgent.ref -> Some(
              Congestions(
                voltageCongestions = false,
                lineCongestions = true,
                transformerCongestions = false,
              )
            )
          ),
          Congestions(
            voltageCongestions = true,
            lineCongestions = true,
            transformerCongestions = false,
          ),
        ),
      )

      forAll(cases) { (inferiorData, expectedCongestions) =>
        val awaitingData = AwaitingData(inferiorData)

        val behavior = spawnWithBuffer(
          GridAgent.checkForCongestion(
            stateData,
            awaitingData,
          )(constantData, _)
        )

        behavior ! CongestionCheckRequest(superiorAgent.ref)

        val congestions =
          superiorAgent.expectMessageType[CongestionResponse].value
        congestions shouldBe expectedCongestions
      }
    }

    "wait to answer a request for congestions if inferior data is still missing" in {
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

      val awaitingData: AwaitingData[Congestions] =
        AwaitingData(Set(inferiorAgent.ref))

      val behavior = spawnWithBuffer(
        GridAgent.checkForCongestion(
          stateData,
          awaitingData,
        )(constantData, _)
      )

      behavior ! CongestionCheckRequest(superiorAgent.ref)

      // the request will be stashed and answered after inferior data was received
      behavior ! ReceivedCongestions(
        Vector(
          (
            inferiorAgent.ref,
            Congestions(
              voltageCongestions = false,
              lineCongestions = true,
              transformerCongestions = false,
            ),
          )
        )
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
        AwaitingData(Set(inferiorAgent.ref))

      // init behavior
      val centerGridAgent = spawnWithBuffer(
        GridAgent.checkForCongestion(
          stateData,
          awaitingData,
        )(constantData, _)
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

      // we send the center grid agent a FinishStep message to finish the detection
      centerGridAgent ! FinishStep

      inferiorAgent.expectMessageType[FinishStep.type]
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
        AwaitingData(Set.empty[ActorRef[GridAgent.Request]])

      // init behavior
      val superiorGridAgent = spawnWithBuffer(
        GridAgent.checkForCongestion(
          stateData,
          awaitingData,
        )(constantData, _)
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

      // we expect a FinishStep message
      inferiorAgent.expectMessageType[FinishStep.type]
    }
  }
}
