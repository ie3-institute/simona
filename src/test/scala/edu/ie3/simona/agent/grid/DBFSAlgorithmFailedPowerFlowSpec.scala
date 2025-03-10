/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.models.input.container.ThermalGrid
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.agent.grid.GridAgentMessages.Responses.{
  ExchangePower,
  ExchangeVoltage,
}
import edu.ie3.simona.agent.grid.GridAgentMessages._
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.model.grid.{RefSystem, VoltageLimits}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.test.common.model.grid.DbfsTestGrid
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

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

class DBFSAlgorithmFailedPowerFlowSpec
    extends ScalaTestWithActorTestKit
    with DBFSMockGridAgents
    with ConfigTestData
    with DbfsTestGrid
    with TestSpawnerTyped {

  private val scheduler: TestProbe[SchedulerMessage] = TestProbe("scheduler")
  private val runtimeEvents: TestProbe[RuntimeEvent] =
    TestProbe("runtimeEvents")
  private val primaryService = TestProbe("primaryService")
  private val weatherService = TestProbe("weatherService")

  private val superiorGridAgent = SuperiorGA(
    TestProbe("superiorGridAgent_1000"),
    Seq(supNodeA.getUuid),
  )

  private val inferiorGridAgent =
    InferiorGA(TestProbe("inferiorGridAgent"), Seq(node1.getUuid))

  private val environmentRefs = EnvironmentRefs(
    scheduler = scheduler.ref,
    runtimeEventListener = runtimeEvents.ref,
    primaryServiceProxy = primaryService.ref.toClassic,
    weather = weatherService.ref.toClassic,
    emDataService = None,
    evDataService = None,
  )

  val resultListener: TestProbe[ResultEvent] = TestProbe("resultListener")

  "A GridAgent actor in center position with async test" should {

    // since the grid agent is stopped after a failed power flow
    // we need to initialize the agent for each test
    def initAndGoToSimulateGrid: ActorRef[GridAgent.Request] = {
      val centerGridAgent =
        testKit.spawn(
          GridAgent(
            environmentRefs,
            simonaConfig,
            listener = Iterable(resultListener.ref),
          )
        )

      // this subnet has 1 superior grid (ehv) and 3 inferior grids (mv). Map the gates to test probes accordingly
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
          Seq.empty[ThermalGrid],
          subGridGateToActorRef,
          RefSystem("2000 MVA", "110 kV"),
          VoltageLimits(0.9, 1.1),
        )

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      centerGridAgent ! CreateGridAgent(
        gridAgentInitData,
        key,
      )

      val scheduleActivationMsg =
        scheduler.expectMessageType[ScheduleActivation]
      scheduleActivationMsg.tick shouldBe INIT_SIM_TICK
      scheduleActivationMsg.unlockKey shouldBe Some(key)
      val gridAgentActivation = scheduleActivationMsg.actor

      centerGridAgent ! WrappedActivation(Activation(INIT_SIM_TICK))
      scheduler.expectMessage(Completion(gridAgentActivation, Some(3600)))

      // send init data to agent
      centerGridAgent ! WrappedActivation(Activation(3600))

      // we expect a completion message
      scheduler.expectMessageType[Completion].newTick shouldBe Some(3600)

      centerGridAgent
    }

    s"start the simulation when an activation is sent is sent, handle failed power flow if it occurs" in {
      val centerGridAgent = initAndGoToSimulateGrid

      val sweepNo = 0

      // send the start grid simulation trigger
      centerGridAgent ! WrappedActivation(Activation(3600))

      // we expect a request for grid power values here for sweepNo $sweepNo
      val powerRequestSender = inferiorGridAgent.expectGridPowerRequest()

      // we expect a request for voltage values of slack node
      val slackVoltageRequestSender =
        superiorGridAgent.expectSlackVoltageRequest(sweepNo)

      // normally the inferior grid agents ask for the slack voltage as well to run their power flow calculation
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
            Kilovolts(110d),
            Kilovolts(0d),
          )
        ),
      )

      // we now answer the request of our centerGridAgent
      // with a fake grid power message and one fake slack voltage message
      powerRequestSender ! GridPowerResponse(
        inferiorGridAgent.nodeUuids.map(nodeUuid =>
          ExchangePower(
            nodeUuid,
            Megawatts(1000.0),
            Megavars(0.0),
          )
        )
      )

      slackVoltageRequestSender ! SlackVoltageResponse(
        sweepNo,
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
      superiorGridAgent.requestGridPower(centerGridAgent, sweepNo)

      // the requested power is too high for the grid to handle, therefore the superior grid agent
      // receives a FailedPowerFlow message
      // wait 30 seconds max for power flow to finish
      superiorGridAgent.gaProbe.expectMessage(30 seconds, FailedPowerFlow)

      // normally the slack node would send a FinishGridSimulationTrigger to all
      // connected inferior grids, because the slack node is just a mock, we imitate this behavior
      centerGridAgent ! FinishGridSimulationTrigger(3600)

      // after a FinishGridSimulationTrigger is sent to the inferior grids, they themselves will
      // forward the trigger to their connected inferior grids. Therefore, the inferior grid agent
      // should receive a FinishGridSimulationTrigger
      inferiorGridAgent.gaProbe.expectMessage(FinishGridSimulationTrigger(3600))

      // after all grids have received a FinishGridSimulationTrigger, the scheduler should receive a Completion
      scheduler.expectMessageType[Completion].newTick shouldBe Some(7200)

      resultListener.expectNoMessage()

      // PowerFlowFailed events are only sent by the slack subgrid
      runtimeEvents.expectNoMessage()
    }

    s"inform its superior GridAgent if a failed power flow occurred" in {
      val centerGridAgent = initAndGoToSimulateGrid

      val sweepNo = 0

      // send the start grid simulation trigger
      centerGridAgent ! WrappedActivation(Activation(3600))

      // we expect a request for grid power values here for sweepNo 0
      val powerRequestSender = inferiorGridAgent.expectGridPowerRequest()

      // we expect a request for voltage values of slack node
      val slackVoltageRequestSender =
        superiorGridAgent.expectSlackVoltageRequest(sweepNo)

      // normally the inferior grid agents ask for the slack voltage as well to run their power flow calculation
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
            Kilovolts(110d),
            Kilovolts(0d),
          )
        ),
      )

      // we have a failed power flow in the inferior grid
      // and send this info to the center grid
      powerRequestSender ! FailedPowerFlow

      slackVoltageRequestSender ! SlackVoltageResponse(
        sweepNo,
        Seq(
          ExchangeVoltage(
            supNodeA.getUuid,
            Kilovolts(380d),
            Kilovolts(0d),
          )
        ),
      )

      // power flow calculation is skipped now and power flow failure is forwarded
      superiorGridAgent.requestGridPower(centerGridAgent, sweepNo)

      // the center grid should forward the failed power flow message to the superior grid
      superiorGridAgent.gaProbe.expectMessage(30 seconds, FailedPowerFlow)

      // normally the slack node would send a FinishGridSimulationTrigger to all
      // connected inferior grids, because the slack node is just a mock, we imitate this behavior
      centerGridAgent ! FinishGridSimulationTrigger(3600)

      // after a FinishGridSimulationTrigger is sent to the inferior grids, they themselves will
      // forward the trigger to their connected inferior grids. Therefore, the inferior grid agent
      // should receive a FinishGridSimulationTrigger
      inferiorGridAgent.gaProbe.expectMessage(FinishGridSimulationTrigger(3600))

      // after all grids have received a FinishGridSimulationTrigger, the scheduler should receive a Completion
      scheduler.expectMessageType[Completion].newTick shouldBe Some(7200)

      resultListener.expectNoMessage()

      // PowerFlowFailed events are only sent by the slack subgrid
      runtimeEvents.expectNoMessage()
    }
  }

  "A GridAgent actor in slack position with async test" should {

    "stop simulation if some inferior power flow has failed" in {
      val hvGridAgent =
        InferiorGA(TestProbe("HvGridAgent"), Seq(supNodeA.getUuid))

      val slackGridAgent: ActorRef[GridAgent.Request] = testKit.spawn(
        GridAgent(
          environmentRefs,
          simonaConfig, // stopOnFailure is enabled
          listener = Iterable(resultListener.ref),
        )
      )

      val sweepNo = 0

      val subnetGatesToActorRef =
        ehvSubGridGates.map(_ -> hvGridAgent.ref).toMap

      val gridAgentInitData =
        GridAgentInitData(
          ehvGridContainer,
          Seq.empty[ThermalGrid],
          subnetGatesToActorRef,
          RefSystem("5000 MVA", "380 kV"),
          VoltageLimits(0.9, 1.1),
        )

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      slackGridAgent ! CreateGridAgent(gridAgentInitData, key)

      val scheduleActivationMsg =
        scheduler.expectMessageType[ScheduleActivation]
      scheduleActivationMsg.tick shouldBe INIT_SIM_TICK
      scheduleActivationMsg.unlockKey shouldBe Some(key)
      val gridAgentActivation = scheduleActivationMsg.actor

      slackGridAgent ! WrappedActivation(Activation(INIT_SIM_TICK))
      scheduler.expectMessage(Completion(gridAgentActivation, Some(3600)))

      // send init data to agent
      slackGridAgent ! WrappedActivation(Activation(3600))

      // we expect a completion message
      scheduler.expectMessageType[Completion].newTick shouldBe Some(3600)

      // send the start grid simulation trigger
      slackGridAgent ! WrappedActivation(Activation(3600))

      val powerRequestSender = hvGridAgent.expectGridPowerRequest()

      // normally the inferior grid agents ask for the slack voltage as well to run their power flow calculation
      // we simulate this behaviour now by doing the same for our inferior grid agent
      hvGridAgent.requestSlackVoltage(slackGridAgent, sweepNo)

      // as we are in the first sweep, provided slack voltage should be equal
      // to 1 p.u. (in physical value, here: 380kV) from the superior grid agent perspective
      // (here: slackGridAgent perspective)
      hvGridAgent.expectSlackVoltageProvision(
        sweepNo,
        Seq(
          ExchangeVoltage(
            supNodeA.getUuid,
            Kilovolts(380d),
            Kilovolts(0d),
          )
        ),
      )

      // we have a failed power flow in the inferior grid
      // and send this info to the center grid
      powerRequestSender ! FailedPowerFlow

      // runtime event is sent by slack agent
      runtimeEvents.expectMessage(RuntimeEvent.PowerFlowFailed)

      // slack agent should have died now
      val deathWatch = createTestProbe("deathWatch")
      deathWatch.expectTerminated(slackGridAgent.ref)

      // superior GA has died immediately, sends no more messages
      hvGridAgent.gaProbe.expectNoMessage()
      scheduler.expectNoMessage()

      resultListener.expectNoMessage()
    }
  }

}
