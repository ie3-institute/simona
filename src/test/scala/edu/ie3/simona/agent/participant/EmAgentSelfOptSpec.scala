/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import com.typesafe.config.ConfigFactory
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.em.EmAgent
import edu.ie3.simona.agent.participant.em.EmAgent.EmAgentInitializeStateData
import edu.ie3.simona.agent.participant.statedata.InitializeStateData
import edu.ie3.simona.ontology.messages.FlexibilityMessage.{
  IssueNoCtrl,
  IssuePowerCtrl,
  ProvideMinMaxFlexOptions,
  RequestFlexOptions
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeParticipantAgentTrigger
}
import edu.ie3.simona.test.ParticipantAgentSpec
import edu.ie3.simona.test.common.input.EmInputTestData
import edu.ie3.util.quantities.PowerSystemUnits
import org.scalatestplus.mockito.MockitoSugar.mock
import tech.units.indriya.quantity.Quantities

class EmAgentSelfOptSpec
    extends ParticipantAgentSpec(
      ActorSystem(
        "EmAgentSelfOptSpec",
        ConfigFactory
          .parseString("""
        |akka.loggers =["akka.event.slf4j.Slf4jLogger"]
        |akka.loglevel="DEBUG"
  """.stripMargin)
      )
    )
    with EmInputTestData {

  private val resolution =
    simonaConfig.simona.powerflow.resolution.getSeconds // TODO does this make sense?

  "An em agent with model calculation" should {
    "be initialized correctly" in {
      val emAgent = TestActorRef(
        new EmAgent(
          scheduler = scheduler.ref,
          listener = systemListener
        )
      )

      val initTriggerId = 0

      val participant1: TestProbe = TestProbe("participant1_pv")
      val participant2: TestProbe = TestProbe("participant2_evcs")

      val participant1Init =
        InitializeParticipantAgentTrigger[ApparentPower, InitializeStateData[
          ApparentPower
        ]](mock[InitializeStateData[ApparentPower]])
      val participant2Init =
        InitializeParticipantAgentTrigger[ApparentPower, InitializeStateData[
          ApparentPower
        ]](mock[InitializeStateData[ApparentPower]])

      val connectedAgents = Seq(
        (
          participant1.ref,
          participant1Init,
          participant1Model
        ),
        (
          participant2.ref,
          participant2Init,
          participant2Model
        )
      )

      scheduler.send(
        emAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            EmAgentInitializeStateData
          ](
            EmAgentInitializeStateData(
              inputModel = emInputModel,
              modelConfig = modelConfig,
              secondaryDataServices = None,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = defaultOutputConfig,
              primaryServiceProxy = primaryServiceProxy.ref,
              connectedAgents = connectedAgents
            )
          ),
          initTriggerId,
          emAgent
        )
      )

      val receivedInit1 = participant1
        .expectMsgType[TriggerWithIdMessage]
      receivedInit1.trigger shouldBe participant1Init

      val receivedInit2 = participant2
        .expectMsgType[TriggerWithIdMessage]
      receivedInit2.trigger shouldBe participant2Init

      participant1.send(
        emAgent,
        CompletionMessage(
          receivedInit1.triggerId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(0L),
                participant1.ref
              )
            )
          )
        )
      )

      scheduler.expectNoMessage()

      participant2.send(
        emAgent,
        CompletionMessage(
          receivedInit2.triggerId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(0L),
                participant2.ref
              )
            )
          )
        )
      )

      scheduler.expectMsg(
        CompletionMessage(
          initTriggerId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(0L),
                emAgent
              )
            )
          )
        )
      )

      // init done, start EmAgent
      val activationTriggerId = 1L

      scheduler.send(
        emAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          activationTriggerId,
          emAgent
        )
      )

      // expect activations and flex requests
      val activationTrigger1 = participant1.expectMsgType[TriggerWithIdMessage]
      activationTrigger1.trigger shouldBe ActivityStartTrigger(0L)
      activationTrigger1.receiverActor shouldBe participant1.ref

      participant1.expectMsg(RequestFlexOptions)

      val activationTrigger2 = participant2.expectMsgType[TriggerWithIdMessage]
      activationTrigger2.trigger shouldBe ActivityStartTrigger(0L)
      activationTrigger2.receiverActor shouldBe participant2.ref

      participant2.expectMsg(RequestFlexOptions)

      // send flex options
      participant1.send(
        emAgent,
        ProvideMinMaxFlexOptions(
          participant1Model.getUuid,
          Quantities.getQuantity(-5d, PowerSystemUnits.KILOWATT),
          Quantities.getQuantity(-5d, PowerSystemUnits.KILOWATT),
          Quantities.getQuantity(0d, PowerSystemUnits.KILOWATT)
        )
      )

      participant1.expectNoMessage()
      participant2.expectNoMessage()

      participant2.send(
        emAgent,
        ProvideMinMaxFlexOptions(
          participant2Model.getUuid,
          Quantities.getQuantity(2d, PowerSystemUnits.KILOWATT),
          Quantities.getQuantity(-11d, PowerSystemUnits.KILOWATT),
          Quantities.getQuantity(11d, PowerSystemUnits.KILOWATT)
        )
      )

      participant1.expectMsg(IssueNoCtrl)
      val issuePower = participant2.expectMsgType[IssuePowerCtrl]

      // TODO compare to expected result
    }
  }

}
