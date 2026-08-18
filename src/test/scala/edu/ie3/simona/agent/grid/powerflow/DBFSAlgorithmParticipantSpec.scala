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
import edu.ie3.simona.agent.grid.{GridAgent, GridAgentCoordinator}
import edu.ie3.simona.agent.grid.GridAgentMessages.*
import edu.ie3.simona.agent.grid.GridAgentMessages.Responses.{
  ExchangePower,
  ExchangeVoltage,
}
import edu.ie3.simona.agent.grid.data.GridAgentData.{
  GridAgentConstantData,
  GridAgentInitData,
}
import edu.ie3.simona.agent.participant.ParticipantAgentInit.{
  ParticipantRefs,
  SimulationParameters,
}
import edu.ie3.simona.agent.participant.{ParticipantAgent, ParticipantAgentInit}
import edu.ie3.simona.config.RuntimeConfig.LoadRuntimeConfig
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.model.InputModelContainer.SimpleInputContainer
import edu.ie3.simona.model.grid.{GridModel, RefSystem, VoltageLimits}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  PrimaryServiceRegistrationMessage,
  RegistrationFailedMessage,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.load.LoadProfileService
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.service.results.ResultServiceProxy
import edu.ie3.simona.service.weather.WeatherService
import edu.ie3.simona.test.common.model.grid.DbfsTestGridWithParticipants
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.config.InputConfig.{Grid, GridDatasource}
import edu.ie3.simona.config.OutputConfig.Base
import edu.ie3.simona.config.{InputConfig, OutputConfig, SimonaConfig}
import edu.ie3.simona.util.ConfigUtil.{NotifierIdentifier, OutputConfigUtil}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.scala.quantities.Megavars
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef
import squants.Each
import squants.electro.Kilovolts
import squants.energy.Megawatts

import java.time.ZonedDateTime
import java.util.UUID
import scala.language.postfixOps

class DBFSAlgorithmParticipantSpec
    extends ScalaTestWithActorTestKit
    with DBFSMockGridAgents
    with DbfsTestGridWithParticipants
    with TestSpawnerTyped {

private val simonaConfig = SimonaConfig(
  input = InputConfig(
    grid = Grid(
      datasource = GridDatasource(
        id = "csv"
      )
    )
  ),
  output = OutputConfig(
    base = Base(
      addTimestampToOutputDir = false,
      dir = "testOutput/"
    )
  ),
  powerflow = Some(
    SimonaConfig.Powerflow(
      maxSweepPowerDeviation = 1e-5,
      newtonraphson = SimonaConfig.Powerflow.Newtonraphson(
        epsilon = List(1e-12),
        iterations = 50,
      ),
      stopOnFailure = true,
    )
  ),
  simulationName = "DBFSAlgorithmParticipantSpec",
  time = SimonaConfig.Time(
    startDateTime = "2011-05-01T00:00:00Z",
    endDateTime = "2011-05-01T01:00:00Z",
  ),
)

private val startTime: ZonedDateTime = simonaConfig.time.simStartTime
private val endTime: ZonedDateTime = simonaConfig.time.simEndTime

  private val scheduler: TestProbe[SchedulerMessage] = TestProbe("scheduler")
  private val runtimeEvents: TestProbe[RuntimeEvent] =
    TestProbe("runtimeEvents")
  private val primaryService =
    TestProbe[PrimaryServiceProxy.Message]("primaryService")
  private val resultProxy = TestProbe[ResultServiceProxy.Message]("resultProxy")
  private val weatherService =
    TestProbe[WeatherService.Message]("weatherService")
  private val loadProfileService =
    TestProbe[LoadProfileService.Message]("loadProfileService")
  private val gridAgentCoordinator: TestProbe[GridAgentCoordinator.Message] =
    TestProbe("gridAgentCoordinator")

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

  private val superiorGridAgent = SuperiorGA(
    TestProbe("superiorGridAgent_1000"),
    Seq(supNodeA.getUuid),
  )

  "Test participant" should {

    given ParticipantRefs = ParticipantRefs(
      primaryService.ref,
      resultProxy.ref,
      environmentRefs.serviceMap,
    )

    given SimulationParameters =
      SimulationParameters(+3600, Each(1e-14), startTime, endTime)

    val gridModel = GridModel(
      hvGridContainer,
      RefSystem("2000 MVA", "110 kV"),
      VoltageLimits(0.9, 1.1),
      startTime,
      endTime,
      simonaConfig,
    )

    val gridAgentInitData = GridAgentInitData(
      gridModel,
      PowerFlowParams(simonaConfig.powerflow.value),
    )

    val gridAgentWithParticipants = testKit.spawn(GridAgent(gridAgentInitData))

    s"initialize itself when it receives an init activation" in {

      gridAgentWithParticipants ! RegisterSuperiorGrid(
        superiorGridAgent.ref,
        superiorGridAgent.nodeUuids.toSet,
        1000,
      )

      // create load agent
      val key = ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      val loadAgent = testKit.spawn(
        ParticipantAgentInit(
          SimpleInputContainer(load1),
          LoadRuntimeConfig(),
          OutputConfigUtil
            .participants(simonaConfig.output.participant)
            .getOrDefault(NotifierIdentifier.Load),
          Left(scheduler.ref),
          key,
        ),
        name = "test_load",
      )

      val nodeToAssets: Map[UUID, Set[ActorRef[ParticipantAgent.Request]]] =
        Map(node1.getUuid -> Set(loadAgent))

      val loadActivation = scheduler.expectMessageType[ScheduleActivation]
      loadActivation.tick shouldBe INIT_SIM_TICK
      loadActivation.actor shouldBe loadAgent

      // register load
      gridAgentWithParticipants ! RegisterParticipants(nodeToAssets)

      gridAgentWithParticipants ! CompleteInitialization(false)

      // mock scheduling behavior
      gridAgentCoordinator
        .expectMessageType[FinishedInitialization]
        .gridRef shouldBe gridAgentWithParticipants

      loadAgent ! Activation(INIT_SIM_TICK)

      val serviceRegistrationMsg = primaryService
        .expectMessageType[PrimaryServiceRegistrationMessage]
      serviceRegistrationMsg.inputModelUuid shouldBe load1.getUuid

      serviceRegistrationMsg.requestingActor ! RegistrationFailedMessage(
        primaryService.ref
      )

      scheduler.expectMessage(Completion(loadAgent, Some(0)))

      // triggering the loadAgent's calculation
      loadAgent ! Activation(0)

      // the load agent should send a Completion
      scheduler.expectMessage(Completion(loadAgent, Some(3600)))

    }

    s"check the request asset power message indirectly" in {

      val firstSweepNo = 0

      // send the start grid simulation trigger
      // the gird agent should send a RequestAssetPowerMessage to the load agent
      gridAgentWithParticipants ! DoPowerFlowTrigger(3600)

      // we expect a request for voltage values of our slack node
      // (voltages are requested by our agent under test from the superior grid)
      val firstSlackVoltageRequestSender =
        superiorGridAgent.expectSlackVoltageRequest(firstSweepNo)

      // we now answer the request of our gridAgentsWithParticipants
      // with a fake slack voltage message
      firstSlackVoltageRequestSender ! SlackVoltageResponse(
        superiorGridAgent.ref,
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
            gridAgentWithParticipants,
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
        superiorGridAgent.ref,
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
            gridAgentWithParticipants,
            Megawatts(135.90837346741768),
            Megavars(60.98643348675892),
          )
        )
      )

      // normally the superior grid agent would send a FinishGridSimulationTrigger to the inferior grid agent after the convergence
      // (here we do it by hand)
      gridAgentWithParticipants ! FinishGridSimulationTrigger(3600L)

      // after all grids have received a FinishGridSimulationTrigger, the coordinator should receive no power flow results
      gridAgentCoordinator
        .expectMessageType[PowerFlowResults]
        .gridAgent shouldBe gridAgentWithParticipants

      // the grid agent coordinator sends a completion message to the scheduler
      scheduler ! Completion(gridAgentCoordinator.ref, Some(7200))

      scheduler.expectMessageType[Completion].newTick shouldBe Some(7200)
    }
  }
}
