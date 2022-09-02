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

class DBFSAlgorithmPowerFlowSpec
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

      scheduler.send(
        centerGridAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(3600),
          activityStartTriggerId,
          centerGridAgent
        )
      )

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

      // configuration of the test
      val maxNumberOfTestSweeps = 4

      // go on with testing the sweep behaviour
      for (sweepNo <- 0 to maxNumberOfTestSweeps) {

        val startGridSimulationTriggerId = sweepNo + 4
        val requestedConnectionNodeUuids =
          Vector(UUID.fromString("9fe5fa33-6d3b-4153-a829-a16f4347bc4e"))

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

        val slackVoltageRequestSender =
          superiorGridAgent.expectSlackVoltageRequest(sweepNo)

        inferiorGridAgent.requestSlackVoltage(centerGridAgent, sweepNo)

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

        superiorGridAgent.requestGridPower(centerGridAgent, sweepNo)

        superiorGridAgent.gaProbe.expectMsgPF(30.seconds) {
          case FailedPowerFlow =>
            fail(s"failed power flow")
        }

        // Simulate Grid
        scheduler.expectMsgPF(30.seconds) {
          case CompletionMessage(
                _,
                Some(
                  Vector(
                    ScheduleTriggerMessage(
                      StartGridSimulationTrigger(3600),
                      _
                    )
                  )
                )
              ) =>
          // when we received a FinishGridSimulationTrigger (as inferior grid agent)
          // we expect another completion message then as well (scheduler view)
          case CompletionMessage(
                _,
                Some(
                  Vector(
                    ScheduleTriggerMessage(ActivityStartTrigger(7200), _)
                  )
                )
              ) =>
            // after doing cleanup stuff, our agent should go back to idle again
            resultListener.expectMsgPF(30.seconds) {
              case powerFlowResultEvent: PowerFlowResultEvent =>
                powerFlowResultEvent.nodeResults.headOption match {
                  case Some(value) =>
                    value.getvMag().getValue shouldBe 1
                    value.getvAng().getValue shouldBe 0
                }

                powerFlowResultEvent.lineResults shouldBe empty
                powerFlowResultEvent.switchResults shouldBe empty
                powerFlowResultEvent.transformer2wResults shouldBe empty
                powerFlowResultEvent.transformer3wResults shouldBe empty
            }

          case x =>
            fail(
              s"Invalid message received when expecting a completion message for simulate grid! Message was $x"
            )
        }
      }
    }

  }
}
