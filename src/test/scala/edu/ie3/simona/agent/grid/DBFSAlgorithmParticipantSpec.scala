/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestProbe}
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.agent.participant.data.Data.PrimaryData
import edu.ie3.simona.agent.participant.statedata.InitializeStateData
import edu.ie3.simona.agent.state.GridAgentState.SimulateGrid
import edu.ie3.simona.model.grid.RefSystem
import edu.ie3.simona.ontology.messages.PowerMessage.ProvideGridPowerMessage.ExchangePower
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.messages.VoltageMessage.ProvideSlackVoltageMessage
import edu.ie3.simona.ontology.messages.VoltageMessage.ProvideSlackVoltageMessage.ExchangeVoltage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationFailedMessage
import edu.ie3.simona.ontology.trigger.Trigger._
import edu.ie3.simona.test.common.model.grid.DbfsTestGridWithParticipants
import edu.ie3.simona.test.common.{ConfigTestData, TestKitWithShutdown}
import edu.ie3.util.quantities.PowerSystemUnits.{KILOVOLT, MEGAVAR, MEGAWATT}
import tech.units.indriya.quantity.Quantities

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

class DBFSAlgorithmParticipantSpec
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
    with DBFSMockGridAgents
    with ConfigTestData
    with ImplicitSender
    with DbfsTestGridWithParticipants {

  private val scheduler = TestProbe("scheduler")
  private val primaryService = TestProbe("primaryService")
  private val weatherService = TestProbe("weatherService")

  private val environmentRefs = EnvironmentRefs(
    scheduler = scheduler.ref,
    primaryServiceProxy = primaryService.ref,
    weather = weatherService.ref,
    evDataService = None
  )

  protected val resultListener: TestProbe = TestProbe("resultListener")

  private val superiorGridAgent = SuperiorGA(
    TestProbe("superiorGridAgent_1000"),
    Seq(supNodeA.getUuid)
  )

  "Test participant" should {

    val gridAgentWithParticipants = system.actorOf(
      GridAgent.props(
        environmentRefs,
        simonaConfig,
        Iterable(resultListener.ref)
      )
    )

    s"initialize itself when it receives a $InitializeGridAgentTrigger with corresponding data" in {
      val triggerId = 0
      val loadAgentTriggerId = 1

      // this subnet has 1 superior grid (HöS) and 3 inferior grids (MS). Map the gates to test probes accordingly
      val subGridGateToActorRef: Map[SubGridGate, ActorRef] =
        hvSubGridGates.map {
          case gate if gate.getInferiorSubGrid == hvGridContainer.getSubnet =>
            gate -> superiorGridAgent.ref
        }.toMap

      val gridAgentInitData =
        GridAgentInitData(
          hvGridContainer,
          subGridGateToActorRef,
          RefSystem("2000 MVA", "110 kV")
        )

      // send init data to agent and expect a CompletionMessage
      scheduler.send(
        gridAgentWithParticipants,
        TriggerWithIdMessage(
          InitializeGridAgentTrigger(gridAgentInitData),
          triggerId,
          gridAgentWithParticipants
        )
      )

      val (loadAgent, initializeTrigger): (
          ActorRef,
          InitializeParticipantAgentTrigger[PrimaryData, InitializeStateData[
            PrimaryData
          ]]
      ) =
        scheduler.expectMsgPF() {
          case ScheduleTriggerMessage(
                initializeTrigger: InitializeParticipantAgentTrigger[
                  PrimaryData,
                  InitializeStateData[PrimaryData]
                ],
                loadAgent
              ) =>
            (loadAgent, initializeTrigger)
        }

      scheduler.expectMsgPF() {
        case CompletionMessage(
              triggerId,
              Some(
                Vector(
                  ScheduleTriggerMessage(triggerToBeScheduled, gridAgentActor)
                )
              )
            ) =>
          triggerId shouldBe 0
          triggerToBeScheduled shouldBe ActivityStartTrigger(3600L)
          gridAgentActor shouldBe gridAgentWithParticipants
        case x =>
          fail(
            s"Invalid message received when expecting a completion message after an init trigger. Message was $x"
          )
      }

      scheduler.send(
        loadAgent,
        TriggerWithIdMessage(
          initializeTrigger,
          loadAgentTriggerId,
          loadAgent
        )
      )

      primaryService.expectMsg(
        PrimaryServiceRegistrationMessage(load1.getUuid)
      )

      primaryService.send(loadAgent, RegistrationFailedMessage)

      scheduler.expectMsgPF(10.seconds) {
        case CompletionMessage(
              loadAgentTriggerId,
              Some(triggerMessage :: Nil)
            ) =>
          loadAgentTriggerId shouldBe 1
          triggerMessage.trigger shouldBe ActivityStartTrigger(0L)

          // triggering the loadAgent's calculation
          scheduler.send(
            loadAgent,
            TriggerWithIdMessage(
              triggerMessage.trigger,
              2,
              loadAgent
            )
          )

        case x =>
          fail(
            s"Invalid message received when expecting a completion message after an init trigger. Message was $x"
          )
      }

      // the load agent should send a CompletionMessage
      scheduler.expectMsg(CompletionMessage(2, None))

    }

    s"go to $SimulateGrid when it receives an activity start trigger" in {

      val activityStartTriggerId = 3

      // send init data to agent
      scheduler.send(
        gridAgentWithParticipants,
        TriggerWithIdMessage(
          ActivityStartTrigger(3600),
          activityStartTriggerId,
          gridAgentWithParticipants
        )
      )

      // we expect a completion message
      scheduler.expectMsgPF() {
        case CompletionMessage(
              triggerId,
              Some(Vector(ScheduleTriggerMessage(triggerToBeScheduled, _)))
            ) =>
          triggerId shouldBe 3
          triggerToBeScheduled shouldBe StartGridSimulationTrigger(3600)
        case x =>
          fail(
            s"Invalid message received when expecting a completion message after activity start trigger. Message was $x"
          )
      }

    }

    s"check the request asset power message indirectly" in {

      val startGridSimulationTriggerId = 4
      val firstSweepNo = 0

      val voltageEhv =
        Array(
          (
            Quantities.getQuantity(380, KILOVOLT),
            Quantities.getQuantity(0, KILOVOLT)
          ),
          (
            Quantities.getQuantity(380, KILOVOLT),
            Quantities.getQuantity(0, KILOVOLT)
          )
        )

      val powerEhv =
        Array(
          (
            Quantities.getQuantity(135.90837346741768, MEGAWATT),
            Quantities.getQuantity(60.98643348675892, MEGAVAR)
          ),
          (
            Quantities.getQuantity(135.90837346741768, MEGAWATT),
            Quantities.getQuantity(60.98643348675892, MEGAVAR)
          )
        )

      // send the start grid simulation trigger
      // the gird agent should send a RequestAssetPowerMessage to the load agent
      scheduler.send(
        gridAgentWithParticipants,
        TriggerWithIdMessage(
          StartGridSimulationTrigger(3600),
          startGridSimulationTriggerId,
          gridAgentWithParticipants
        )
      )

      // we expect a request for voltage values of our slack node
      // (voltages are requested by our agent under test from the superior grid)
      val firstSlackVoltageRequestSender =
        superiorGridAgent.expectSlackVoltageRequest(firstSweepNo)

      // we now answer the request of our gridAgentsWithParticipants
      // with a fake slack voltage message
      superiorGridAgent.gaProbe.send(
        firstSlackVoltageRequestSender,
        ProvideSlackVoltageMessage(
          firstSweepNo,
          Seq(
            ExchangeVoltage(
              supNodeA.getUuid,
              voltageEhv(firstSweepNo)._1,
              voltageEhv(firstSweepNo)._2
            )
          )
        )
      )

      // power flow calculation should run now. After it's done,
      // our test agent should now be ready to provide the grid power values,
      // hence we ask for them and expect a corresponding response
      superiorGridAgent.requestGridPower(
        gridAgentWithParticipants,
        firstSweepNo
      )

      // the gridAgentWithParticipants have received an AssetPowerChangedMessage
      // before requesting power from the superiorGrid
      superiorGridAgent.expectGridPowerProvision(
        Seq(
          ExchangePower(
            supNodeA.getUuid,
            powerEhv(firstSweepNo)._1,
            powerEhv(firstSweepNo)._2
          )
        )
      )

      // before the second sweep the gridAgentWithParticipants will receive an AssetPowerUnchangedMessage
      // we start a second sweep by asking for next sweep values which should trigger the whole procedure again
      val secondSweepNo = firstSweepNo + 1

      superiorGridAgent.requestGridPower(
        gridAgentWithParticipants,
        secondSweepNo
      )

      // the agent now should ask for updated slack voltages from the superior grid
      val secondSlackAskSender =
        superiorGridAgent.expectSlackVoltageRequest(secondSweepNo)

      // the superior grid would answer with updated slack voltage values
      superiorGridAgent.gaProbe.send(
        secondSlackAskSender,
        ProvideSlackVoltageMessage(
          secondSweepNo,
          Seq(
            ExchangeVoltage( // this one should currently be ignored anyways
              supNodeA.getUuid,
              voltageEhv(secondSweepNo)._1,
              voltageEhv(secondSweepNo)._2
            )
          )
        )
      )

      // here the gridAgentWithParticipants have received a second AssetPowerUnchangedMessage
      // we expect that the GridAgent unstashes the messages and return a value for our power request
      superiorGridAgent.expectGridPowerProvision(
        Seq(
          ExchangePower(
            supNodeA.getUuid,
            powerEhv(secondSweepNo)._1,
            powerEhv(secondSweepNo)._2
          )
        )
      )

      // normally the superior grid agent would check weather the power flow calculation converges and would
      // send a CompletionMessage to the scheduler and a FinishGridSimulationTrigger to the inferior grid agent
      // after the convergence
      // (here we do it by hand)
      superiorGridAgent.gaProbe.send(
        scheduler.ref,
        CompletionMessage(
          5,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(7200),
                superiorGridAgent.gaProbe.ref
              )
            )
          )
        )
      )

      scheduler.expectMsgPF() {
        case CompletionMessage(
              triggerId,
              Some(Seq(message: ScheduleTriggerMessage))
            ) =>
          triggerId shouldBe 5
          message shouldBe ScheduleTriggerMessage(
            ActivityStartTrigger(7200),
            superiorGridAgent.gaProbe.ref
          )

        case x =>
          fail(
            s"Invalid message received when expecting a completion message for simulate grid after cleanup! Message was $x"
          )
      }
      superiorGridAgent.gaProbe.send(
        gridAgentWithParticipants,
        FinishGridSimulationTrigger(3600)
      )

      scheduler.expectMsgPF() {
        case CompletionMessage(
              triggerId,
              Some(Seq(message: ScheduleTriggerMessage))
            ) =>
          triggerId shouldBe 4
          message shouldBe ScheduleTriggerMessage(
            ActivityStartTrigger(7200),
            gridAgentWithParticipants
          )

        case x =>
          fail(
            s"Invalid message received when expecting a completion message for simulate grid after cleanup! Message was $x"
          )
      }

    }
  }
}
