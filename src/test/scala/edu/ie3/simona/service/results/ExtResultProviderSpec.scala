/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.results

import edu.ie3.simona.api.data.connection.ExtResultDataConnection
import edu.ie3.simona.api.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.ontology.results.{
  ProvideResultEntities,
  RequestResultEntities,
}
import edu.ie3.simona.api.ontology.simulation.ControlResponseMessageFromExt
import edu.ie3.simona.ontology.messages.ResultMessage.{
  RequestResult,
  ResultResponse,
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.result.PowerFlowResultData
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}

import scala.jdk.CollectionConverters.{
  ListHasAsScala,
  SeqHasAsJava,
  SetHasAsScala,
}

class ExtResultProviderSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with PowerFlowResultData {

  private val scheduler = TestProbe[SchedulerMessage]("scheduler")
  private val resultProxy = TestProbe[RequestResult]("resultProxy")

  "The ExtResultProvider" should {

    "handle result responses correctly" in {
      val connection = new ExtResultDataConnection(List(dummyInputModel).asJava)
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")
      val provider =
        spawn(ExtResultProvider(connection, scheduler.ref, resultProxy.ref))
      connection.setActorRefs(provider.ref, extSimAdapter.ref)

      provider ! ResultResponse(
        Map(dummyInputModel -> List(dummyNodeResult, dummyPvResult))
      )

      val results =
        connection.receiveWithType(classOf[ProvideResultEntities]).results
      results.keySet.asScala shouldBe Set(dummyInputModel)
      results.get(dummyInputModel).asScala shouldBe List(
        dummyNodeResult,
        dummyPvResult,
      )

      scheduler.expectMessage(Completion(provider.ref))
    }

    "handle result data message from external" in {
      val connection = new ExtResultDataConnection(List(dummyInputModel).asJava)
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")
      val provider =
        spawn(ExtResultProvider(connection, scheduler.ref, resultProxy.ref))
      connection.setActorRefs(provider.ref, extSimAdapter.ref)

      // requesting results from the result provider
      connection.sendExtMsg(
        new RequestResultEntities(3600L, List(dummyInputModel).asJava, false)
      )

      extSimAdapter.expectMessage(new ScheduleDataServiceMessage(provider.ref))
      provider ! Activation(3600L)

      resultProxy.expectMessage(
        RequestResult(Seq(dummyInputModel), 3600L, provider.ref, Some(-1))
      )

      provider ! ResultResponse(Map(dummyInputModel -> List(dummyPvResult)))

      scheduler.expectMessage(Completion(provider.ref))
    }

  }

}
