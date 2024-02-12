/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.agent.grid.GridAgentMessage.{
  ActivationAdapter,
  VMAdapter,
  CreateGridAgent,
  FinishGridSimulationTrigger
}
import edu.ie3.simona.event.listener.ResultEventListener.ResultMessage
import edu.ie3.simona.model.grid.RefSystem
import edu.ie3.simona.ontology.messages.PowerMessage.ProvideGridPowerMessage.ExchangePower
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation
}
import edu.ie3.simona.ontology.messages.VoltageMessage.ProvideSlackVoltageMessage
import edu.ie3.simona.ontology.messages.VoltageMessage.ProvideSlackVoltageMessage.ExchangeVoltage
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationFailedMessage
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.test.common.model.grid.DbfsTestGridWithParticipants
import edu.ie3.simona.test.common.{ConfigTestData, TestSpawnerTyped}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.scala.quantities.Megavars
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe
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

  private val scheduler: TestProbe[SchedulerMessage] =
    TestProbe[SchedulerMessage]("scheduler")
  private val runtimeEvents = TestProbe("runtimeEvents")
  private val primaryService: TestProbe[ServiceMessage] =
    TestProbe[ServiceMessage]("primaryService")
  private val weatherService = TestProbe("weatherService")

  private val environmentRefs = EnvironmentRefs(
    scheduler = scheduler.ref.toClassic,
    runtimeEventListener = runtimeEvents.ref.toClassic,
    primaryServiceProxy = primaryService.ref.toClassic,
    weather = weatherService.ref.toClassic,
    evDataService = None
  )

  protected val resultListener: TestProbe[ResultMessage] =
    TestProbe[ResultMessage]("resultListener")

  private val superiorGridAgent = SuperiorGA(
    TestProbe("superiorGridAgent_1000"),
    Seq(supNodeA.getUuid)
  )

  "Test participant" should {
    val gridAgentWithParticipants = testKit.spawn(
      GridAgent(
        environmentRefs,
        simonaConfig,
        Iterable(resultListener.ref.toClassic)
      )
    )

    s"initialize itself when it receives an init activation" in {

      // this subnet has 1 superior grid (ehv) and 3 inferior grids (mv). Map the gates to test probes accordingly
      val subGridGateToActorRef: Map[SubGridGate, ActorRef[GridAgentMessage]] =
        hvSubGridGates.map { gate =>
          gate -> superiorGridAgent.ref
        }.toMap

      val gridAgentInitData = GridAgentInitData(
        hvGridContainer,
        Seq.empty,
        subGridGateToActorRef,
        RefSystem("2000 MVA", "110 kV")
      )

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      scheduler
        .expectMessageType[ScheduleActivation] // lock activation scheduled

      gridAgentWithParticipants ! CreateGridAgent(gridAgentInitData, key)

      val msg = scheduler.expectMessageType[ScheduleActivation]
      msg shouldBe ScheduleActivation(msg.actor, INIT_SIM_TICK, Some(key))

      // send init data to agent and expect a CompletionMessage
      gridAgentWithParticipants ! ActivationAdapter(Activation(INIT_SIM_TICK))

      val message = scheduler.expectMessageType[ScheduleActivation]

      val loadAgent: ActorRef[Activation] = message match {
        case ScheduleActivation(
              loadAgent,
              INIT_SIM_TICK,
              _
            ) =>
          loadAgent
      }

      val completionMessage = scheduler.expectMessageType[Completion]
      completionMessage shouldBe Completion(completionMessage.actor, Some(3600))

      loadAgent ! Activation(INIT_SIM_TICK)

      primaryService.expectMessage(
        PrimaryServiceRegistrationMessage(load1.getUuid)
      )

      loadAgent.toClassic ! RegistrationFailedMessage(
        primaryService.ref.toClassic
      )

      scheduler.expectMessage(Completion(loadAgent, Some(0)))

      // triggering the loadAgent's calculation
      loadAgent ! Activation(0)

      // the load agent should send a CompletionMessage
      scheduler.expectMessage(Completion(loadAgent, None))

    }

    s"go to SimulateGrid when it receives an activity start trigger" in {

      // send init data to agent
      gridAgentWithParticipants ! ActivationAdapter(Activation(3600))

      // we expect a completion message
      val message = scheduler.expectMessageType[Completion]
      message shouldBe Completion(message.actor, Some(3600))
    }

    s"check the request asset power message indirectly" in {

      val firstSweepNo = 0

      // send the start grid simulation trigger
      // the gird agent should send a RequestAssetPowerMessage to the load agent
      gridAgentWithParticipants ! ActivationAdapter(Activation(3600))

      // we expect a request for voltage values of our slack node
      // (voltages are requested by our agent under test from the superior grid)
      val firstSlackVoltageRequestSender =
        superiorGridAgent.expectSlackVoltageRequest(firstSweepNo)

      // we now answer the request of our gridAgentsWithParticipants
      // with a fake slack voltage message
      firstSlackVoltageRequestSender ! VMAdapter(
        ProvideSlackVoltageMessage(
          firstSweepNo,
          Seq(
            ExchangeVoltage(
              supNodeA.getUuid,
              Kilovolts(380d),
              Kilovolts(0d)
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
      secondSlackAskSender ! VMAdapter(
        ProvideSlackVoltageMessage(
          secondSweepNo,
          Seq(
            ExchangeVoltage(
              supNodeA.getUuid,
              Kilovolts(374.2269461446d),
              Kilovolts(65.9863075134d)
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
      gridAgentWithParticipants ! FinishGridSimulationTrigger(3600L)

      val message = scheduler.expectMessageType[Completion]
      message shouldBe Completion(message.actor, Some(7200))
    }
  }
}
