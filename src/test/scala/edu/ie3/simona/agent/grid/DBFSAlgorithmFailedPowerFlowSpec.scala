/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.testkit.{ImplicitSender, TestProbe}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import edu.ie3.powerflow.model.PowerFlowResult.FailedPowerFlowResult
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.DBFSAlgorithmCenGridSpec.{
  InferiorGA,
  SuperiorGA
}
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.agent.state.GridAgentState.SimulateGrid
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.model.grid.RefSystem
import edu.ie3.simona.ontology.messages.PowerMessage.ProvideGridPowerMessage.ExchangePower
import edu.ie3.simona.ontology.messages.PowerMessage.{
  FailedPowerFlow,
  ProvideGridPowerMessage,
  RequestGridPowerMessage
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.messages.VoltageMessage.ProvideSlackVoltageMessage.ExchangeVoltage
import edu.ie3.simona.ontology.messages.VoltageMessage.{
  ProvideSlackVoltageMessage,
  RequestSlackVoltageMessage
}
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  FinishGridSimulationTrigger,
  InitializeGridAgentTrigger,
  StartGridSimulationTrigger
}
import edu.ie3.simona.test.common.model.grid.DbfsTestGrid
import edu.ie3.simona.test.common.{
  ConfigTestData,
  TestKitWithShutdown,
  UnitSpec
}
import edu.ie3.util.quantities.PowerSystemUnits._
import tech.units.indriya.quantity.Quantities

import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.concurrent.Await

class DBFSAlgorithmFailedPowerFlowSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "DBFSAlgorithmSpec",
        ConfigFactory
          .parseString("""
                     |akka.loggers =["akka.event.slf4j.Slf4jLogger"]
                     |akka.loglevel="OFF"
        """.stripMargin)
      )
    )
    with UnitSpec
    with ConfigTestData
    with ImplicitSender
    with DbfsTestGrid {

  private val scheduler = TestProbe("scheduler")
  private val primaryService = TestProbe("primaryService")
  private val weatherService = TestProbe("weatherService")

  private val superiorGridAgent = SuperiorGA(
    TestProbe("superiorGridAgent_1000"),
    Seq(supNodeA.getUuid)
  )

  private val inferiorGridAgent =
    InferiorGA(TestProbe("inferiorGridAgent"), Seq(node1.getUuid))

  private val environmentRefs = EnvironmentRefs(
    scheduler = scheduler.ref,
    primaryServiceProxy = primaryService.ref,
    weather = weatherService.ref,
    evDataService = None
  )

  val resultListener: TestProbe = TestProbe("resultListener")

  "A GridAgent actor in center position with async test" should {

    val centerGridAgent =
      system.actorOf(
        GridAgent.props(
          environmentRefs,
          simonaConfig,
          listener = Iterable(resultListener.ref)
        )
      )

    s"initialize itself when it receives a $InitializeGridAgentTrigger with corresponding data" in {
      val triggerId = 0

      // this subnet has 1 superior grid (HöS) and 3 inferior grids (MS). Map the gates to test probes accordingly
      val subGridGateToActorRef = hvSubGridGatesPF.map {
        case gate if gate.getInferiorSubGrid == hvGridContainerPF.getSubnet =>
          gate -> superiorGridAgent.ref
        case gate =>
          val actor = gate.getInferiorSubGrid match {
            case 11 => inferiorGridAgent
          }
          gate -> actor.ref
      }.toMap

      val gridAgentInitData =
        GridAgentInitData(
          hvGridContainerPF,
          subGridGateToActorRef,
          RefSystem("2000 MVA", "110 kV")
        )

      // send init data to agent and expect a CompletionMessage
      implicit val timeout: Timeout = 3 seconds
      val actualInitReply =
        Await.result(
          centerGridAgent ? TriggerWithIdMessage(
            InitializeGridAgentTrigger(gridAgentInitData),
            triggerId,
            centerGridAgent
          ),
          timeout.duration
        )

      actualInitReply shouldBe CompletionMessage(
        0,
        Some(
          Vector(
            ScheduleTriggerMessage(
              ActivityStartTrigger(3600),
              centerGridAgent
            )
          )
        )
      )

    }

    s"go to $SimulateGrid when it receives an activity start trigger" in {

      val activityStartTriggerId = 1

      // send init data to agent
      scheduler.send(
        centerGridAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(3600),
          activityStartTriggerId,
          centerGridAgent
        )
      )

      // we expect a completion message
      scheduler.expectMsgPF() {
        case CompletionMessage(
              triggerId,
              Some(Vector(ScheduleTriggerMessage(triggerToBeScheduled, _)))
            ) =>
          triggerId shouldBe 1
          triggerToBeScheduled shouldBe StartGridSimulationTrigger(3600)
        case x =>
          fail(
            s"Invalid message received when expecting a completion message after activity start trigger. Message was $x"
          )
      }
    }

    s"start the simulation when a $StartGridSimulationTrigger is sent, handle failed power flow if it occurs" in {
      val sweepNo = 0

      val startGridSimulationTriggerId = 2

      // send the start grid simulation trigger
      scheduler.send(
        centerGridAgent,
        TriggerWithIdMessage(
          StartGridSimulationTrigger(3600),
          startGridSimulationTriggerId,
          centerGridAgent
        )
      )

      // we expect a request for grid power values here for sweepNo $sweepNo
      val powerRequestSender = inferiorGridAgent.expectGridPowerRequest()

      // we expect a request for voltage values of slack node
      val slackVoltageRequestSender =
        superiorGridAgent.expectSlackVoltageRequest(sweepNo)

      // normally the inferior grid agent ask for the slack voltage as well to do their power flow calculation
      // we simulate this behaviour now by doing the same for our inferior grid agent
      inferiorGridAgent.requestSlackVoltage(centerGridAgent, sweepNo)

      // as we are in the first sweep, provided slack voltage should be equal
      // to 1 p.u. (in physical value, here: 110kV) from the superior grid agent perspective
      // (here: centerGridAgent perspective)
      inferiorGridAgent.expectSlackVoltageProvision(
        sweepNo,
        Seq(
          ExchangeVoltage(
            node1.getUuid,
            Quantities.getQuantity(110, KILOVOLT),
            Quantities.getQuantity(0, KILOVOLT)
          )
        )
      )

      // we now answer the request of our centerGridAgent
      // with a fake grid power message and one fake slack voltage message
      inferiorGridAgent.gaProbe.send(
        powerRequestSender,
        ProvideGridPowerMessage(
          inferiorGridAgent.nodeUuids.map(nodeUuid =>
            ExchangePower(
              nodeUuid,
              Quantities.getQuantity(1000, MEGAWATT),
              Quantities.getQuantity(0, MEGAVAR)
            )
          )
        )
      )

      superiorGridAgent.gaProbe.send(
        slackVoltageRequestSender,
        ProvideSlackVoltageMessage(
          sweepNo,
          Seq(
            ExchangeVoltage(
              supNodeA.getUuid,
              Quantities.getQuantity(380, KILOVOLT),
              Quantities.getQuantity(0, KILOVOLT)
            )
          )
        )
      )

      // power flow calculation should run now. After it's done,
      // our test agent should now be ready to provide the grid power values,
      // hence we ask for them and expect a corresponding response
      superiorGridAgent.requestGridPower(centerGridAgent, sweepNo)

      // the requested power is to high for the grid to handle, therefor the superior grid agent
      // receives a failed power flow message
      superiorGridAgent.gaProbe.expectMsg(FailedPowerFlow)

      // normally the slack node would send a FinishGridSimulationTrigger to itself and to all
      // connected inferior grids, because the slack node is just a mock, we imitate this behavior
      superiorGridAgent.gaProbe.send(
        centerGridAgent,
        FinishGridSimulationTrigger(3600)
      )

      // after a FinishGridSimulationTrigger is send the inferior grids themself will send the Trigger
      // to their connected inferior grids, therefor the inferior grid agent should receive a
      // FinishGridSimulationTrigger
      inferiorGridAgent.gaProbe.expectMsg(FinishGridSimulationTrigger(3600))

      // after all grids have received a FinishGridSimulationTrigger, the scheduler should receive a CompletionMessage
      scheduler.expectMsgPF(30.seconds) {
        case CompletionMessage(
              _,
              Some(
                Vector(
                  ScheduleTriggerMessage(ActivityStartTrigger(7200), _)
                )
              )
            ) =>
          // after doing cleanup stuff, our agent should go back to idle again
          // because of the failed power flow the result listener should receive no message
          resultListener.expectNoMessage()

        case x =>
          fail(
            s"Invalid message received when expecting a completion message for simulate grid! Message was $x"
          )
      }
    }

  }
}
