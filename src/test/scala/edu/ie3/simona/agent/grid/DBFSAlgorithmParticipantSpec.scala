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
import edu.ie3.datamodel.models.input.container.ThermalGrid
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
import edu.ie3.util.scala.quantities.Megavars
import squants.energy.Megawatts
import tech.units.indriya.quantity.Quantities

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

      // this subnet has 1 superior grid (ehv) and 3 inferior grids (mv). Map the gates to test probes accordingly
      val subGridGateToActorRef: Map[SubGridGate, ActorRef] =
        hvSubGridGates.map { gate =>
          gate -> superiorGridAgent.ref
        }.toMap

      val gridAgentInitData =
        GridAgentInitData(
          hvGridContainer,
          Seq.empty[ThermalGrid],
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

      val (loadAgent, initializeTrigger) =
        scheduler.expectMsgPF() {
          case ScheduleTriggerMessage(
                initializeTrigger: InitializeParticipantAgentTrigger[
                  PrimaryData,
                  InitializeStateData[PrimaryData]
                ],
                loadAgent,
                _,
                _
              ) =>
            (loadAgent, initializeTrigger)
        }

      scheduler.expectMsg(
        CompletionMessage(
          triggerId,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(3600L),
                gridAgentWithParticipants
              )
            )
          )
        )
      )

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

      scheduler.expectMsg(
        CompletionMessage(
          loadAgentTriggerId,
          Some(
            Seq(ScheduleTriggerMessage(ActivityStartTrigger(0L), loadAgent))
          )
        )
      )

      // triggering the loadAgent's calculation
      scheduler.send(
        loadAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          2,
          loadAgent
        )
      )
      // the load agent should send a CompletionMessage
      scheduler.expectMsg(CompletionMessage(2, None))

    }

    s"go to $SimulateGrid when it receives an activity start trigger" in {

      val activityStartTriggerId = 3

      // send init data to agent
      scheduler.send(
        gridAgentWithParticipants,
        TriggerWithIdMessage(
          ActivityStartTrigger(3600L),
          activityStartTriggerId,
          gridAgentWithParticipants
        )
      )

      // we expect a completion message
      scheduler.expectMsg(
        CompletionMessage(
          3,
          Some(
            Seq(
              ScheduleTriggerMessage(
                StartGridSimulationTrigger(3600L),
                gridAgentWithParticipants
              )
            )
          )
        )
      )

    }

    s"check the request asset power message indirectly" in {

      val startGridSimulationTriggerId = 4
      val firstSweepNo = 0

      // send the start grid simulation trigger
      // the gird agent should send a RequestAssetPowerMessage to the load agent
      scheduler.send(
        gridAgentWithParticipants,
        TriggerWithIdMessage(
          StartGridSimulationTrigger(3600L),
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
              Quantities.getQuantity(380, KILOVOLT),
              Quantities.getQuantity(0, KILOVOLT)
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

      // the gridAgentWithParticipants has received an AssetPowerChangedMessage
      // before requesting power from the superiorGrid
      superiorGridAgent.expectGridPowerProvision(
        Seq(
          ExchangePower(
            supNodeA.getUuid,
            Megawatts(135.90837346741768),
            Megavars(60.98643348675892)
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
            ExchangeVoltage(
              supNodeA.getUuid,
              Quantities.getQuantity(374.2269461446, KILOVOLT),
              Quantities.getQuantity(65.9863075134, KILOVOLT)
            )
          )
        )
      )

      // here the gridAgentWithParticipants has received a second AssetPowerUnchangedMessage
      // we expect that the GridAgent unstashes the messages and return a value for our power request
      superiorGridAgent.expectGridPowerProvision(
        Seq(
          ExchangePower(
            supNodeA.getUuid,
            Megawatts(135.90837346741768),
            Megavars(60.98643348675892)
          )
        )
      )

      // normally the superior grid agent would send a FinishGridSimulationTrigger to the inferior grid agent after the convergence
      // (here we do it by hand)
      superiorGridAgent.gaProbe.send(
        gridAgentWithParticipants,
        FinishGridSimulationTrigger(3600L)
      )

      scheduler.expectMsg(
        CompletionMessage(
          4,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(7200L),
                gridAgentWithParticipants
              )
            )
          )
        )
      )
    }
  }
}
