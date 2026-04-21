/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.results

import edu.ie3.simona.event.ResultEvent.{
  ParticipantResultEvent,
  PowerFlowResultEvent,
}
import edu.ie3.simona.ontology.messages.ResultMessage.{
  RequestResult,
  ResultResponse,
}
import edu.ie3.simona.service.results.ResultServiceProxy.ExpectResult
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.result.PowerFlowResultData
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}

class ResultServiceProxySpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with PowerFlowResultData
    with ThreeWindingResultTestData {

  "The ResultServiceProxy" should {

    val startTime = dummyTime.minusHours(1)

    val allExpected = Seq(
      dummyNodeResultModel,
      dummySwitchResultModel,
      dummyLineResultModel,
      dummyTrafo2WResultModel,
      inputModel,
    )

    "answer request for results correctly without waiting for results" in {
      val resultProvider = TestProbe[ResultResponse]("listener")

      val resultProxy = spawn(ResultServiceProxy(Seq.empty, startTime, 10))

      resultProxy ! RequestResult(
        Seq(dummyInputModel, inputModel),
        3600L,
        resultProvider.ref,
        None,
      )

      // no results, since the result proxy received not waiting for result information
      resultProvider
        .expectMessageType[ResultResponse]
        .results shouldBe Map.empty
    }

    "answer request for results correctly with waiting for some results" in {
      val resultProvider = TestProbe[ResultResponse]("listener")

      val resultProxy = spawn(ResultServiceProxy(Seq.empty, startTime, 10))

      // tells the proxy to wait for the results of dummyInputModel for tick 3600L
      resultProxy ! ExpectResult(Seq(dummyNodeResultModel), 3600L)

      resultProxy ! RequestResult(allExpected, 3600L, resultProvider.ref, None)

      // still waiting for results
      resultProvider.expectNoMessage()

      resultProxy ! PowerFlowResultEvent(
        Seq(dummyNodeResult),
        Seq(dummySwitchResult),
        Seq(dummyLineResult),
        Seq(dummyTrafo2wResult),
        Seq(resultA),
        Seq(dummyNodeCongestionResult),
      )

      // no results for three winding transformers, because the proxy is not told to wait and the results was not received beforehand
      resultProvider.expectMessageType[ResultResponse].results shouldBe Map(
        dummyNodeResultModel -> Iterable(
          dummyNodeResult,
          dummyNodeCongestionResult,
        ),
        dummySwitchResultModel -> Iterable(dummySwitchResult),
        dummyLineResultModel -> Iterable(dummyLineResult),
        dummyTrafo2WResultModel -> Iterable(dummyTrafo2wResult),
      )
    }

    "answer request for results correctly with waiting for some results with different receive order" in {
      val resultProvider = TestProbe[ResultResponse]("listener")

      val resultProxy = spawn(ResultServiceProxy(Seq.empty, startTime, 10))

      // tells the proxy to wait for the results with dumyInputModel for tick 3600L
      resultProxy ! ExpectResult(Seq(dummyNodeResultModel), 3600L)

      resultProxy ! RequestResult(allExpected, 3600L, resultProvider.ref, None)

      // receiving three winding results for port B and C beforehand
      resultProxy ! PowerFlowResultEvent(
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq(resultB, resultC),
      )

      // still waiting for results
      resultProvider.expectNoMessage()

      resultProxy ! PowerFlowResultEvent(
        Seq(dummyNodeResult),
        Seq(dummySwitchResult),
        Seq(dummyLineResult),
        Seq(dummyTrafo2wResult),
        Seq(resultA),
        Seq(dummyNodeCongestionResult),
      )

      // receives three winding result, because all partial results are present
      resultProvider.expectMessageType[ResultResponse].results shouldBe Map(
        dummyNodeResultModel -> Seq(dummyNodeResult, dummyNodeCongestionResult),
        dummySwitchResultModel -> Seq(dummySwitchResult),
        dummyLineResultModel -> Seq(dummyLineResult),
        dummyTrafo2WResultModel -> Seq(dummyTrafo2wResult),
        inputModel -> Seq(expected),
      )
    }

    "answer request for results correctly with waiting for all results" in {
      val resultProvider = TestProbe[ResultResponse]("listener")

      val resultProxy = spawn(ResultServiceProxy(Seq.empty, startTime, 10))

      // tells the proxy to wait for the results of dumyInputModel for tick 3600L
      resultProxy ! ExpectResult(Seq(dummyNodeResultModel), 3600L)

      // tells the proxy to also wait for the results of inputModel for tick 3600L
      resultProxy ! ExpectResult(Seq(inputModel), 3600L)

      resultProxy ! RequestResult(
        Seq(
          dummyNodeResultModel,
          dummySwitchResultModel,
          dummyLineResultModel,
          dummyTrafo2WResultModel,
          inputModel,
        ),
        3600L,
        resultProvider.ref,
        None,
      )

      // still waiting for results
      resultProvider.expectNoMessage()

      resultProxy ! PowerFlowResultEvent(
        Seq(dummyNodeResult),
        Seq(dummySwitchResult),
        Seq(dummyLineResult),
        Seq(dummyTrafo2wResult),
        Seq(resultA),
        Seq(dummyNodeCongestionResult),
      )

      // still waiting for results
      resultProvider.expectNoMessage()

      // receiving three winding results for port B and C
      resultProxy ! PowerFlowResultEvent(
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq(resultB, resultC),
      )

      // no results for three winding transformers, because the proxy is not told to wait and the results was not received beforehand
      resultProvider.expectMessageType[ResultResponse].results shouldBe Map(
        dummyNodeResultModel -> List(
          dummyNodeResult,
          dummyNodeCongestionResult,
        ),
        dummySwitchResultModel -> List(dummySwitchResult),
        dummyLineResultModel -> List(dummyLineResult),
        dummyTrafo2WResultModel -> List(dummyTrafo2wResult),
        inputModel -> List(expected),
      )
    }

    "answer request for results correctly with threshold" in {
      val resultProvider = TestProbe[ResultResponse]("listener")

      val resultProxy = spawn(ResultServiceProxy(Seq.empty, startTime, 10))

      // tells the proxy to wait for the results of dumyInputModel for tick 3600L
      resultProxy ! ExpectResult(Seq(dummyNodeResultModel), 3600L)

      resultProxy ! PowerFlowResultEvent(
        Seq(dummyNodeResult),
        Seq(dummySwitchResult),
        Seq(dummyLineResult),
        Seq(dummyTrafo2wResult),
        Seq.empty,
        Seq(dummyNodeCongestionResult),
      )

      // tells the proxy to also wait for the results of inputModel for tick 7200L
      resultProxy ! ExpectResult(Seq(dummyNodeResultModel), 7200L)

      resultProxy ! PowerFlowResultEvent(
        Seq(dummyNodeResult2PlusHour),
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq.empty,
      )

      resultProxy ! RequestResult(
        Seq(
          dummyNodeResultModel,
          dummySwitchResultModel,
          dummyLineResultModel,
          dummyTrafo2WResultModel,
        ),
        7200L,
        resultProvider.ref,
        Some(3600),
      )

      resultProvider.expectMessageType[ResultResponse].results shouldBe Map(
        dummyNodeResultModel -> List(dummyNodeResult2PlusHour)
      )

      resultProxy ! RequestResult(
        Seq(
          dummyNodeResultModel,
          dummySwitchResultModel,
          dummyLineResultModel,
          dummyTrafo2WResultModel,
        ),
        7200L,
        resultProvider.ref,
        Some(3599),
      )

      resultProvider.expectMessageType[ResultResponse].results shouldBe Map(
        dummyNodeResultModel -> List(dummyNodeResult2PlusHour),
        dummySwitchResultModel -> List(dummySwitchResult),
        dummyLineResultModel -> List(dummyLineResult),
        dummyTrafo2WResultModel -> List(dummyTrafo2wResult),
      )
    }

    "correctly handle grid result events" in {
      val listener = TestProbe[ResultResponse]("listener")

      val resultProxy =
        spawn(ResultServiceProxy(Seq(listener.ref), startTime, 10))

      resultProxy ! PowerFlowResultEvent(
        Seq(dummyNodeResult),
        Seq(dummySwitchResult),
        Seq(dummyLineResult),
        Seq(dummyTrafo2wResult),
        Seq.empty,
      )

      // all results are mapped to their uuid
      listener.expectMessageType[ResultResponse].results shouldBe Map(
        dummyNodeResultModel -> List(dummyNodeResult),
        dummySwitchResultModel -> List(dummySwitchResult),
        dummyLineResultModel -> List(dummyLineResult),
        dummyTrafo2WResultModel -> List(dummyTrafo2wResult),
      )
    }

    "correctly handle unchanged grid result" in {
      val listener = TestProbe[ResultResponse]("listener")
      val resultProvider = TestProbe[ResultResponse]("listener")

      val resultProxy =
        spawn(ResultServiceProxy(Seq(listener.ref), startTime, 10))

      resultProxy ! PowerFlowResultEvent(
        Seq(dummyNodeResult),
        Seq(dummySwitchResult),
        Seq(dummyLineResult),
        Seq(dummyTrafo2wResult),
        Seq.empty,
        Seq(dummyNodeCongestionResult),
      )

      // all results are mapped to their uuid
      listener.expectMessageType[ResultResponse].results shouldBe Map(
        dummyNodeResultModel -> List(
          dummyNodeResult,
          dummyNodeCongestionResult,
        ),
        dummySwitchResultModel -> List(dummySwitchResult),
        dummyLineResultModel -> List(dummyLineResult),
        dummyTrafo2WResultModel -> List(dummyTrafo2wResult),
      )

      resultProxy ! PowerFlowResultEvent(
        Seq(dummyNodeResultPlusHour),
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq(dummyNodeCongestionResultPlusHour),
      )

      // no unchanged or new results received
      listener.expectNoMessage()

      // request only updated results
      resultProxy ! RequestResult(
        Seq(dummyNodeResultModel),
        7200L,
        resultProvider.ref,
        Some(3600),
      )

      resultProvider
        .expectMessageType[ResultResponse]
        .results shouldBe Map.empty

      // also request unchanged results
      resultProxy ! RequestResult(
        Seq(dummyNodeResultModel),
        7200L,
        resultProvider.ref,
        Some(3599),
      )

      resultProvider.expectMessageType[ResultResponse].results shouldBe Map(
        dummyNodeResultModel -> List(
          dummyNodeResult,
          dummyNodeCongestionResult,
        ),
        dummySwitchResultModel -> List(dummySwitchResult),
        dummyLineResultModel -> List(dummyLineResult),
        dummyTrafo2WResultModel -> List(dummyTrafo2wResult),
      )
    }

    "correctly handle three winding transformer result events" in {
      val listener = TestProbe[ResultResponse]("listener")

      val resultProxy =
        spawn(ResultServiceProxy(Seq(listener.ref), startTime, 10))

      // sending result for port A
      resultProxy ! PowerFlowResultEvent(
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq(resultA),
      )

      // no message, because the three winding result is not complete
      listener.expectNoMessage()

      // sending result for port C
      resultProxy ! PowerFlowResultEvent(
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq(resultC),
      )

      // no message, because the three winding result is not complete
      listener.expectNoMessage()

      // sending result for port B
      resultProxy ! PowerFlowResultEvent(
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq(resultB),
      )

      listener.expectMessageType[ResultResponse].results shouldBe Map(
        inputModel -> List(expected)
      )
    }

    "correctly handle participant result events" in {
      val listener = TestProbe[ResultResponse]("listener")

      val resultProxy =
        spawn(ResultServiceProxy(Seq(listener.ref), startTime, 10))

      resultProxy ! ParticipantResultEvent(dummyPvResult)

      listener.expectMessageType[ResultResponse].results shouldBe Map(
        dummyPvResult.getInputModel -> List(dummyPvResult)
      )
    }

  }

}
