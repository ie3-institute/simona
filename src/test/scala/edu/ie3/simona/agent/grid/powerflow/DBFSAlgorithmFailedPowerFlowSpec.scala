/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.powerflow

import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentCoordinator.{
  FinishedInitialization,
  PowerFlowResults,
}
import edu.ie3.simona.agent.grid.GridAgentMessages.*
import edu.ie3.simona.agent.grid.GridAgentMessages.Responses.{
  ExchangePower,
  ExchangeVoltage,
}
import edu.ie3.simona.agent.grid.data.GridAgentData.{
  GridAgentConstantData,
  GridAgentInitData,
}
import edu.ie3.simona.agent.grid.{GridAgent, GridAgentCoordinator}
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.model.grid.ampacity.AmpacityCalculationParams
import edu.ie3.simona.model.grid.{GridModel, RefSystem, VoltageLimits}
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.service.load.LoadProfileService
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.service.results.ResultServiceProxy
import edu.ie3.simona.service.results.ResultServiceProxy.ExpectResult
import edu.ie3.simona.service.weather.WeatherService
import edu.ie3.simona.test.common.model.grid.DbfsTestGrid
import edu.ie3.simona.test.common.{ConfigTestData, TestSpawnerTyped}
import edu.ie3.util.scala.quantities.Megavars
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import squants.electro.Kilovolts
import squants.energy.Megawatts

import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

class DBFSAlgorithmFailedPowerFlowSpec
    extends ScalaTestWithActorTestKit
    with DBFSMockGridAgents
    with ConfigTestData
    with DbfsTestGrid
    with TestSpawnerTyped {

  private val scheduler: TestProbe[SchedulerMessage] = TestProbe("scheduler")
  private val runtimeEvents: TestProbe[RuntimeEvent] = TestProbe(
    "runtimeEvents"
  )
  private val primaryService =
    TestProbe[PrimaryServiceProxy.Message]("primaryService")
  private val resultProxy = TestProbe[ResultServiceProxy.Message]("resultProxy")
  private val weatherService =
    TestProbe[WeatherService.Message]("weatherService")
  private val loadProfileService =
    TestProbe[LoadProfileService.Message]("loadProfileService")

  private val gridAgentCoordinator: TestProbe[GridAgentCoordinator.Message] =
    TestProbe("gridAgentCoordinator")
  private val superiorGridAgent = SuperiorGA(
    TestProbe("superiorGridAgent_1000"),
    Seq(supNodeA.getUuid),
  )

  private val inferiorGridAgent =
    InferiorGA(TestProbe("inferiorGridAgent"), Seq(node1.getUuid))

  private val environmentRefs = EnvironmentRefs(
    scheduler = scheduler.ref,
    runtimeEventListener = runtimeEvents.ref,
    primaryServiceProxy = primaryService.ref,
    resultProxy = resultProxy.ref,
    weather = weatherService.ref,
    price = None,
    loadProfiles = loadProfileService.ref,
    emDataService = None,
    evDataService = None,
  )

  given GridAgentConstantData = GridAgentConstantData(
    gridAgentCoordinator.ref,
    environmentRefs,
    simonaConfig,
    3600,
    startTime,
    endTime,
  )

  "A GridAgent actor in center position with async test" should {

    // since the grid agent is stopped after a failed power flow
    // we need to initialize the agent for each test
    def initAndGoToSimulateGrid: ActorRef[GridAgent.Message] = {
      val gridModel = GridModel(
        hvGridContainerPF,
        RefSystem("2000 MVA", "110 kV"),
        VoltageLimits(0.9, 1.1),
        startTime,
        endTime,
        simonaConfig,
      )

      val gridAgentInitData = GridAgentInitData(
        gridModel,
        startTime,
        AmpacityCalculationParams(simonaConfig.ampacityCalculation),
        PowerFlowParams(simonaConfig.powerflow.value),
      )

      val centerGridAgent = testKit.spawn(GridAgent(gridAgentInitData))

      centerGridAgent ! RegisterInferiorGrid(
        inferiorGridAgent.ref,
        inferiorGridAgent.nodeUuids.toSet,
        11,
      )
      centerGridAgent ! RegisterSuperiorGrid(
        superiorGridAgent.ref,
        superiorGridAgent.nodeUuids.toSet,
        1000,
      )

      centerGridAgent ! CompleteInitialization(false)

      // mock scheduling behavior
      gridAgentCoordinator
        .expectMessageType[FinishedInitialization]
        .gridRef shouldBe centerGridAgent
      scheduler ! ScheduleActivation(gridAgentCoordinator.ref, 3600)

      val scheduleActivationMsg =
        scheduler.expectMessageType[ScheduleActivation]
      scheduleActivationMsg.tick shouldBe 3600
      scheduleActivationMsg.unlockKey shouldBe None

      centerGridAgent
    }

    s"start the simulation when an activation is sent is sent, handle failed power flow if it occurs" in {
      val centerGridAgent = initAndGoToSimulateGrid

      val sweepNo = 0

      // send the start grid simulation trigger
      centerGridAgent ! DoPowerFlowTrigger(3600)

      resultProxy.expectMessageType[ExpectResult] match {
        case ExpectResult(assets, tick, waitForSetPoint) =>
          assets match {
            case uuids: Seq[UUID] =>
              uuids.toSet shouldBe assetsHvPF.toSet
              waitForSetPoint shouldBe false
            case uuid: UUID =>
              fail(s"Received uuid $uuid, but expected grid asset uuids.")
          }
          tick shouldBe 3600
      }

      // we expect a request for grid power values here for sweepNo $sweepNo
      val powerRequestSender = inferiorGridAgent.expectGridPowerRequest()

      // we expect a request for voltage values of slack node
      val slackVoltageRequestSender =
        superiorGridAgent.expectSlackVoltageRequest(sweepNo)

      // normally the inferior grid agents ask for the slack voltage as well to run their power flow calculation
      // we simulate this behavior now by doing the same for our inferior grid agent
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
        inferiorGridAgent.ref,
        inferiorGridAgent.nodeUuids.map(
          ExchangePower(
            _,
            inferiorGridAgent.ref,
            Megawatts(1000.0),
            Megavars(0.0),
          )
        ),
      )

      slackVoltageRequestSender ! SlackVoltageResponse(
        superiorGridAgent.ref,
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
      superiorGridAgent.gaProbe.expectMessage(
        30 seconds,
        FailedPowerFlow(centerGridAgent),
      )

      // normally the slack node would send a FinishGridSimulationTrigger to all
      // connected inferior grids, because the slack node is just a mock, we imitate this behavior
      centerGridAgent ! FinishGridSimulationTrigger(3600)

      // after a FinishGridSimulationTrigger is sent to the inferior grids, they themselves will
      // forward the trigger to their connected inferior grids. Therefore, the inferior grid agent
      // should receive a FinishGridSimulationTrigger
      inferiorGridAgent.gaProbe.expectMessage(FinishGridSimulationTrigger(3600))

      // after all grids have received a FinishGridSimulationTrigger, the coordinator should receive the power flow results
      gridAgentCoordinator
        .expectMessageType[PowerFlowResults]
        .gridAgent shouldBe centerGridAgent

      // the grid agent coordinator sends a completion message to the scheduler
      scheduler ! Completion(gridAgentCoordinator.ref, Some(7200))

      scheduler.expectMessageType[Completion].newTick shouldBe Some(7200)

      resultProxy.expectNoMessage()

      // PowerFlowFailed events are only sent by the slack subgrid
      runtimeEvents.expectNoMessage()
    }

    s"inform its superior GridAgent if a failed power flow occurred" in {
      val centerGridAgent = initAndGoToSimulateGrid

      val sweepNo = 0

      // send the start grid simulation trigger
      centerGridAgent ! DoPowerFlowTrigger(3600)

      resultProxy.expectMessageType[ExpectResult] match {
        case ExpectResult(assets, tick, waitForSetPoint) =>
          assets match {
            case uuids: Seq[UUID] =>
              uuids.toSet shouldBe assetsHvPF.toSet
              waitForSetPoint shouldBe false
            case uuid: UUID =>
              fail(s"Received uuid $uuid, but expected grid asset uuids.")
          }
          tick shouldBe 3600
      }

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
      powerRequestSender ! FailedPowerFlow(inferiorGridAgent.ref)

      slackVoltageRequestSender ! SlackVoltageResponse(
        superiorGridAgent.ref,
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
      val response =
        superiorGridAgent.gaProbe.expectMessageType[FailedPowerFlow](10.seconds)
      response.sender shouldBe centerGridAgent

      // normally the slack node would send a FinishGridSimulationTrigger to all
      // connected inferior grids, because the slack node is just a mock, we imitate this behavior
      centerGridAgent ! FinishGridSimulationTrigger(3600)

      // after a FinishGridSimulationTrigger is sent to the inferior grids, they themselves will
      // forward the trigger to their connected inferior grids. Therefore, the inferior grid agent
      // should receive a FinishGridSimulationTrigger
      inferiorGridAgent.gaProbe.expectMessage(FinishGridSimulationTrigger(3600))

      // after all grids have received a FinishGridSimulationTrigger, the coordinator should receive the power flow results
      gridAgentCoordinator
        .expectMessageType[PowerFlowResults]
        .gridAgent shouldBe centerGridAgent

      // the grid agent coordinator sends a completion message to the scheduler
      scheduler ! Completion(gridAgentCoordinator.ref, Some(7200))

      scheduler.expectMessageType[Completion].newTick shouldBe Some(7200)

      resultProxy.expectNoMessage()

      // PowerFlowFailed events are only sent by the slack subgrid
      runtimeEvents.expectNoMessage()
    }
  }

  "A GridAgent actor in slack position with async test" should {

    "stop simulation if some inferior power flow has failed" in {
      val hvGridAgent =
        InferiorGA(TestProbe("HvGridAgent"), Seq(supNodeA.getUuid))

      val gridModel = GridModel(
        ehvGridContainer,
        RefSystem("5000 MVA", "380 kV"),
        VoltageLimits(0.9, 1.1),
        startTime,
        endTime,
        simonaConfig,
      )

      val gridAgentInitData = GridAgentInitData(
        gridModel,
        startTime,
        AmpacityCalculationParams(simonaConfig.ampacityCalculation),
        PowerFlowParams(simonaConfig.powerflow.value),
      )

      val slackGridAgent = testKit.spawn(GridAgent(gridAgentInitData))

      slackGridAgent ! RegisterInferiorGrid(
        hvGridAgent.ref,
        hvGridAgent.nodeUuids.toSet,
        1,
      )

      val sweepNo = 0

      // finish the initialization
      slackGridAgent ! CompleteInitialization(onlyOneSubGrid = false)

      // mock scheduling behavior
      gridAgentCoordinator
        .expectMessageType[FinishedInitialization]
        .gridRef shouldBe slackGridAgent
      scheduler ! ScheduleActivation(gridAgentCoordinator.ref, 3600)

      val scheduleActivationMsg =
        scheduler.expectMessageType[ScheduleActivation]
      scheduleActivationMsg.tick shouldBe 3600
      scheduleActivationMsg.unlockKey shouldBe None

      // send the start grid simulation trigger
      slackGridAgent ! DoPowerFlowTrigger(3600)

      resultProxy.expectMessageType[ExpectResult] match {
        case ExpectResult(assets, tick, waitForSetPoint) =>
          assets match {
            case uuids: Seq[UUID] =>
              uuids.toSet shouldBe assetsEhv.toSet
              waitForSetPoint shouldBe false
            case uuid: UUID =>
              fail(s"Received uuid $uuid, but expected grid asset uuids.")
          }
          tick shouldBe 3600
      }

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
      powerRequestSender ! FailedPowerFlow(hvGridAgent.ref)

      // runtime event is sent by slack agent
      runtimeEvents.expectMessage(RuntimeEvent.PowerFlowFailed)

      // slack agent should have died now
      val deathWatch = createTestProbe("deathWatch")
      deathWatch.expectTerminated(slackGridAgent.ref)

      // superior GA has died immediately, sends no more messages
      hvGridAgent.gaProbe.expectNoMessage()
      scheduler.expectNoMessage()

      resultProxy.expectNoMessage()
    }
  }

}
