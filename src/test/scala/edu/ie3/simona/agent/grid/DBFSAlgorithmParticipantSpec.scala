/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.agent.grid.GridAgentMessages.Responses.{
  ExchangePower,
  ExchangeVoltage,
}
import edu.ie3.simona.agent.grid.GridAgentMessages._
import edu.ie3.simona.agent.participant2.ParticipantAgent.RegistrationFailedMessage
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.model.grid.{RefSystem, VoltageLimits}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.test.common.model.grid.DbfsTestGridWithParticipants
import edu.ie3.simona.test.common.{ConfigTestData, TestSpawnerTyped}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.scala.quantities.Megavars
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import squants.electro.Kilovolts
import squants.energy.Megawatts

import scala.language.postfixOps

class DBFSAlgorithmParticipantSpec
    extends ScalaTestWithActorTestKit
    with DBFSMockGridAgents
    with ConfigTestData
    with DbfsTestGridWithParticipants
    with TestSpawnerTyped {

  private val scheduler: TestProbe[SchedulerMessage] = TestProbe("scheduler")
  private val runtimeEvents: TestProbe[RuntimeEvent] =
    TestProbe("runtimeEvents")
  private val primaryService: TestProbe[ServiceMessage] =
    TestProbe("primaryService")
  private val weatherService = TestProbe("weatherService")

  private val environmentRefs = EnvironmentRefs(
    scheduler = scheduler.ref,
    runtimeEventListener = runtimeEvents.ref,
    primaryServiceProxy = primaryService.ref.toClassic,
    weather = weatherService.ref.toClassic,
    emDataService = None,
    evDataService = None,
  )

  protected val resultListener: TestProbe[ResultEvent] =
    TestProbe("resultListener")

  private val superiorGridAgent = SuperiorGA(
    TestProbe("superiorGridAgent_1000"),
    Seq(supNodeA.getUuid),
  )

  "Test participant" should {
    val gridAgentWithParticipants = testKit.spawn(
      GridAgent(
        environmentRefs,
        simonaConfig,
        Iterable(resultListener.ref),
      )
    )

    s"initialize itself when it receives an init activation" in {

      // this subnet has 1 superior grid (ehv) and 3 inferior grids (mv). Map the gates to test probes accordingly
      val subGridGateToActorRef: Map[SubGridGate, ActorRef[GridAgent.Request]] =
        hvSubGridGates.map { gate =>
          gate -> superiorGridAgent.ref
        }.toMap

      val gridAgentInitData = GridAgentInitData(
        hvGridContainer,
        Seq.empty,
        subGridGateToActorRef,
        RefSystem("2000 MVA", "110 kV"),
        VoltageLimits(0.9, 1.1),
      )

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      scheduler
        .expectMessageType[ScheduleActivation] // lock activation scheduled

      gridAgentWithParticipants ! CreateGridAgent(gridAgentInitData, key)

      val scheduleActivationMsg =
        scheduler.expectMessageType[ScheduleActivation]
      scheduleActivationMsg.tick shouldBe INIT_SIM_TICK
      scheduleActivationMsg.unlockKey shouldBe Some(key)
      val gridAgentActivation = scheduleActivationMsg.actor

      // send init data to agent and expect a Completion
      gridAgentWithParticipants ! WrappedActivation(Activation(INIT_SIM_TICK))

      val scheduleLoadAgentMsg = scheduler.expectMessageType[ScheduleActivation]
      scheduleLoadAgentMsg.tick shouldBe INIT_SIM_TICK
      val loadAgent = scheduleLoadAgentMsg.actor

      scheduler.expectMessage(Completion(gridAgentActivation, Some(3600)))

      loadAgent ! Activation(INIT_SIM_TICK)

      val serviceRegistrationMsg = primaryService
        .expectMessageType[PrimaryServiceRegistrationMessage]
      serviceRegistrationMsg.inputModelUuid shouldBe load1.getUuid

      serviceRegistrationMsg.requestingActor ! RegistrationFailedMessage(
        primaryService.ref.toClassic
      )

      scheduler.expectMessage(Completion(loadAgent, Some(0)))

      // triggering the loadAgent's calculation
      loadAgent ! Activation(0)

      // the load agent should send a Completion
      scheduler.expectMessage(Completion(loadAgent, Some(3600)))

    }

    s"go to SimulateGrid when it receives an activity start trigger" in {

      // send init data to agent
      gridAgentWithParticipants ! WrappedActivation(Activation(3600))

      // we expect a completion message
      scheduler.expectMessageType[Completion].newTick shouldBe Some(3600)
    }

    s"check the request asset power message indirectly" in {

      val firstSweepNo = 0

      // send the start grid simulation trigger
      // the gird agent should send a RequestAssetPowerMessage to the load agent
      gridAgentWithParticipants ! WrappedActivation(Activation(3600))

      // we expect a request for voltage values of our slack node
      // (voltages are requested by our agent under test from the superior grid)
      val firstSlackVoltageRequestSender =
        superiorGridAgent.expectSlackVoltageRequest(firstSweepNo)

      // we now answer the request of our gridAgentsWithParticipants
      // with a fake slack voltage message
      firstSlackVoltageRequestSender ! SlackVoltageResponse(
        firstSweepNo,
        Seq(
          ExchangeVoltage(
            supNodeA.getUuid,
            Kilovolts(380d),
            Kilovolts(0d),
          )
        ),
      )

      // power flow calculation should run now. After it's done,
      // our test agent should now be ready to provide the grid power values,
      // hence we ask for them and expect a corresponding response
      superiorGridAgent.requestGridPower(
        gridAgentWithParticipants,
        firstSweepNo,
      )

      // the gridAgentWithParticipants has received an AssetPowerChangedMessage
      // before requesting power from the superiorGrid
      superiorGridAgent.expectGridPowerProvision(
        Seq(
          ExchangePower(
            supNodeA.getUuid,
            Megawatts(135.90837346741768),
            Megavars(60.98643348675892),
          )
        )
      )

      // before the second sweep the gridAgentWithParticipants will receive an AssetPowerUnchangedMessage
      // we start a second sweep by asking for next sweep values which should trigger the whole procedure again
      val secondSweepNo = firstSweepNo + 1

      superiorGridAgent.requestGridPower(
        gridAgentWithParticipants,
        secondSweepNo,
      )

      // the agent now should ask for updated slack voltages from the superior grid
      val secondSlackAskSender =
        superiorGridAgent.expectSlackVoltageRequest(secondSweepNo)

      // the superior grid would answer with updated slack voltage values
      secondSlackAskSender ! SlackVoltageResponse(
        secondSweepNo,
        Seq(
          ExchangeVoltage(
            supNodeA.getUuid,
            Kilovolts(374.2269461446d),
            Kilovolts(65.9863075134d),
          )
        ),
      )

      // here the gridAgentWithParticipants has received a second AssetPowerUnchangedMessage
      // we expect that the GridAgent unstashes the messages and return a value for our power request
      superiorGridAgent.expectGridPowerProvision(
        Seq(
          ExchangePower(
            supNodeA.getUuid,
            Megawatts(135.90837346741768),
            Megavars(60.98643348675892),
          )
        )
      )

      // normally the superior grid agent would send a FinishGridSimulationTrigger to the inferior grid agent after the convergence
      // (here we do it by hand)
      gridAgentWithParticipants ! FinishGridSimulationTrigger(3600L)

      scheduler.expectMessageType[Completion].newTick shouldBe Some(7200)
    }
  }
}
