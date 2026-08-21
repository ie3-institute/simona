/*
 * © 2020. TU Dortmund University,
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
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
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
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.config.InputConfig.{Grid, GridDatasource}
import edu.ie3.simona.config.OutputConfig.Base
import edu.ie3.simona.config.{InputConfig, OutputConfig, SimonaConfig}
import edu.ie3.util.scala.quantities.Megavars
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef
import squants.electro.Kilovolts
import squants.energy.Megawatts

import java.time.ZonedDateTime
import java.util.UUID
import scala.language.postfixOps

/** Test to ensure the functions that a [[GridAgent]] in center position should
  * be able to do if the DBFSAlgorithm is used. The scheduler, the weather
  * service as well as the inferior and superior [[GridAgent]] s are simulated
  * by the TestKit. By now this test does NOT cover interactions with generation
  * or load asset agents due to unavailability during test development. Hence,
  * it would make sense to extend this test in the future to include asset agent
  * interaction or cover this behaviour by another (integration) test!
  */
class DBFSAlgorithmCenGridSpec
    extends ScalaTestWithActorTestKit
    with DBFSMockGridAgents
    with DbfsTestGrid
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
        dir = "testOutput/",
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
    simulationName = "DBFSAlgorithmCenGridSpec",
    time = SimonaConfig.Time(
      startDateTime = "2011-05-01T00:00:00Z",
      endDateTime = "2011-05-01T01:00:00Z",
    ),
  )

  private val startTime: ZonedDateTime = simonaConfig.time.simStartTime
  private val endTime: ZonedDateTime = simonaConfig.time.simEndTime

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
    Seq(supNodeA.getUuid, supNodeB.getUuid),
  )

  private val inferiorGrid11 =
    InferiorGA(TestProbe("inferiorGridAgent_11"), Seq(node1.getUuid))

  private val inferiorGrid12 =
    InferiorGA(TestProbe("inferiorGridAgent_12"), Seq(node2.getUuid))

  private val inferiorGrid13 = InferiorGA(
    TestProbe("inferiorGridAgent_13"),
    Seq(node3.getUuid, node4.getUuid),
  )

  private val environmentRefs: EnvironmentRefs = EnvironmentRefs(
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

    val centerGridAgent = testKit.spawn(GridAgent(gridAgentInitData))

    s"initialize itself when it receives an init activation" in {

      // this subnet has 1 superior grid (ehv) and 3 inferior grids (mv). Map the gates to test probes accordingly
      centerGridAgent ! RegisterInferiorGrid(
        inferiorGrid11.ref,
        inferiorGrid11.nodeUuids.toSet,
        11,
      )
      centerGridAgent ! RegisterInferiorGrid(
        inferiorGrid12.ref,
        inferiorGrid12.nodeUuids.toSet,
        12,
      )
      centerGridAgent ! RegisterInferiorGrid(
        inferiorGrid13.ref,
        inferiorGrid13.nodeUuids.toSet,
        13,
      )

      centerGridAgent ! RegisterSuperiorGrid(
        superiorGridAgent.ref,
        superiorGridAgent.nodeUuids.toSet,
        1000,
      )

      // finish initialization
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
    }

    s"start the simulation when activation is sent" in {

      val firstSweepNo = 0

      // send the start grid simulation trigger
      centerGridAgent ! DoPowerFlowTrigger(3600)

      resultProxy.expectMessageType[ExpectResult] match {
        case ExpectResult(assets, tick, waitForSetPoint) =>
          assets match {
            case uuids: Seq[UUID] =>
              uuids.toSet shouldBe assetsHv.toSet
              waitForSetPoint shouldBe false
            case uuid: UUID =>
              fail(s"Received uuid $uuid, but expected grid asset uuids.")
          }
          tick shouldBe 3600
      }

      /* We expect one grid power request message per inferior grid */

      val firstPowerRequestSender11 = inferiorGrid11.expectGridPowerRequest()

      val firstPowerRequestSender12 = inferiorGrid12.expectGridPowerRequest()

      val firstPowerRequestSender13 = inferiorGrid13.expectGridPowerRequest()

      // we expect a request for voltage values of two nodes
      // (voltages are requested by our agent under test from the superior grid)
      val firstSlackVoltageRequestSender =
        superiorGridAgent.expectSlackVoltageRequest(firstSweepNo)

      // normally the inferior grid agents ask for the slack voltage as well to run their power flow calculations
      // we simulate this behavior now by doing the same for our three inferior grid agents
      inferiorGrid11.requestSlackVoltage(centerGridAgent, firstSweepNo)

      inferiorGrid12.requestSlackVoltage(centerGridAgent, firstSweepNo)

      inferiorGrid13.requestSlackVoltage(centerGridAgent, firstSweepNo)

      // as we are in the first sweep, all provided slack voltages should be equal
      // to 1 p.u. (in physical values, here: 110kV) from the superior grid agent perspective
      // (here: centerGridAgent perspective)
      inferiorGrid11.expectSlackVoltageProvision(
        firstSweepNo,
        Seq(
          ExchangeVoltage(
            node1.getUuid,
            Kilovolts(110d),
            Kilovolts(0d),
          )
        ),
      )

      inferiorGrid12.expectSlackVoltageProvision(
        firstSweepNo,
        Seq(
          ExchangeVoltage(
            node2.getUuid,
            Kilovolts(110d),
            Kilovolts(0d),
          )
        ),
      )

      inferiorGrid13.expectSlackVoltageProvision(
        firstSweepNo,
        Seq(
          ExchangeVoltage(
            node3.getUuid,
            Kilovolts(110d),
            Kilovolts(0d),
          ),
          ExchangeVoltage(
            node4.getUuid,
            Kilovolts(110d),
            Kilovolts(0d),
          ),
        ),
      )

      // we now answer the request of our centerGridAgent
      // with three fake grid power messages and one fake slack voltage message
      firstPowerRequestSender11 ! GridPowerResponse(
        inferiorGrid11.ref,
        inferiorGrid11.nodeUuids.map(
          ExchangePower(_, inferiorGrid11.ref, Megawatts(0.0), Megavars(0.0))
        ),
      )

      firstPowerRequestSender12 ! GridPowerResponse(
        inferiorGrid12.ref,
        inferiorGrid12.nodeUuids.map(
          ExchangePower(_, inferiorGrid12.ref, Megawatts(0.0), Megavars(0.0))
        ),
      )

      firstPowerRequestSender13 ! GridPowerResponse(
        inferiorGrid13.ref,
        inferiorGrid13.nodeUuids.map(
          ExchangePower(_, inferiorGrid13.ref, Megawatts(0.0), Megavars(0.0))
        ),
      )

      firstSlackVoltageRequestSender ! SlackVoltageResponse(
        centerGridAgent,
        firstSweepNo,
        Seq(
          ExchangeVoltage(
            supNodeA.getUuid,
            Kilovolts(380d),
            Kilovolts(0d),
          ),
          ExchangeVoltage(
            supNodeB.getUuid,
            Kilovolts(380d),
            Kilovolts(0d),
          ),
        ),
      )

      // power flow calculation should run now. After it's done,
      // our test agent should now be ready to provide the grid power values,
      // hence we ask for them and expect a corresponding response
      superiorGridAgent.requestGridPower(centerGridAgent, firstSweepNo)

      superiorGridAgent.expectGridPowerProvision(
        Seq(
          ExchangePower(
            supNodeA.getUuid,
            superiorGridAgent.ref,
            Megawatts(0.0),
            Megavars(0.0),
          ),
          ExchangePower(
            supNodeB.getUuid,
            superiorGridAgent.ref,
            Megawatts(0.160905770717798),
            Megavars(-1.4535602349123878),
          ),
        )
      )

      // we start a second sweep by asking for next sweep values which should trigger the whole procedure again
      val secondSweepNo = firstSweepNo + 1

      superiorGridAgent.requestGridPower(centerGridAgent, secondSweepNo)

      // the agent now should ask for updated slack voltages from the superior grid
      val secondSlackAskSender =
        superiorGridAgent.expectSlackVoltageRequest(secondSweepNo)

      // the superior grid would answer with updated slack voltage values
      secondSlackAskSender ! SlackVoltageResponse(
        superiorGridAgent.ref,
        secondSweepNo,
        Seq(
          ExchangeVoltage(
            supNodeB.getUuid,
            Kilovolts(374.22694614463d), // 380 kV @ 10°
            Kilovolts(65.9863075134335d), // 380 kV @ 10°
          ),
          ExchangeVoltage( // this one should currently be ignored anyway
            supNodeA.getUuid,
            Kilovolts(380d),
            Kilovolts(0d),
          ),
        ),
      )

      // After the intermediate power flow calculation, we expect one grid power
      // request message per inferior subgrid

      val secondPowerRequestSender11 =
        inferiorGrid11.expectGridPowerRequest()

      val secondPowerRequestSender12 =
        inferiorGrid12.expectGridPowerRequest()

      val secondPowerRequestSender13 =
        inferiorGrid13.expectGridPowerRequest()

      // normally the inferior grid agents ask for the slack voltage as well to run their power flow calculations
      // we simulate this behaviour now by doing the same for our three inferior grid agents

      inferiorGrid11.requestSlackVoltage(centerGridAgent, secondSweepNo)

      inferiorGrid12.requestSlackVoltage(centerGridAgent, secondSweepNo)

      inferiorGrid13.requestSlackVoltage(centerGridAgent, secondSweepNo)

      // as we are in the second sweep, all provided slack voltages should be unequal
      // to 1 p.u. (in physical values, here: 110kV) from the superior grid agent perspective
      // (here: centerGridAgent perspective)

      inferiorGrid11.expectSlackVoltageProvision(
        secondSweepNo,
        Seq(
          ExchangeVoltage(
            node1.getUuid,
            Kilovolts(108.487669651919932d),
            Kilovolts(19.101878551141232d),
          )
        ),
      )

      inferiorGrid12.expectSlackVoltageProvision(
        secondSweepNo,
        Seq(
          ExchangeVoltage(
            node2.getUuid,
            Kilovolts(108.449088870497683d),
            Kilovolts(19.10630456834157630d),
          )
        ),
      )

      inferiorGrid13.expectSlackVoltageProvision(
        secondSweepNo,
        Seq(
          ExchangeVoltage(
            node3.getUuid,
            Kilovolts(108.470028019077087d),
            Kilovolts(19.104403047662570d),
          ),
          ExchangeVoltage(
            node4.getUuid,
            Kilovolts(108.482524607256866d),
            Kilovolts(19.1025584700935336d),
          ),
        ),
      )

      // we now answer the requests of our centerGridAgent
      // with three fake grid power message

      secondPowerRequestSender11 ! GridPowerResponse(
        inferiorGrid11.ref,
        inferiorGrid11.nodeUuids.map(
          ExchangePower(_, inferiorGrid11.ref, Megawatts(0.0), Megavars(0.0))
        ),
      )

      secondPowerRequestSender12 ! GridPowerResponse(
        inferiorGrid12.ref,
        inferiorGrid12.nodeUuids.map(
          ExchangePower(_, inferiorGrid12.ref, Megawatts(0.0), Megavars(0.0))
        ),
      )

      secondPowerRequestSender13 ! GridPowerResponse(
        inferiorGrid13.ref,
        inferiorGrid13.nodeUuids.map(
          ExchangePower(_, inferiorGrid13.ref, Megawatts(0.0), Megavars(0.0))
        ),
      )

      // we expect that the GridAgent unstashes the messages and return a value for our power request
      superiorGridAgent.expectGridPowerProvision(
        Seq(
          ExchangePower(
            supNodeA.getUuid,
            centerGridAgent,
            Megawatts(0.0),
            Megavars(0.0),
          ),
          ExchangePower(
            supNodeB.getUuid,
            centerGridAgent,
            Megawatts(0.16090577067051856),
            Megavars(-1.4535602358772026),
          ),
        )
      )

      // normally the slack node would send a FinishGridSimulationTrigger to all
      // connected inferior grids, because the slack node is just a mock, we imitate this behavior
      centerGridAgent ! FinishGridSimulationTrigger(3600)

      // after a FinishGridSimulationTrigger is sent the inferior grids, they themselves will send the
      // Trigger forward the trigger to their connected inferior grids. Therefore, the inferior grid
      // agent should receive a FinishGridSimulationTrigger
      inferiorGrid11.gaProbe.expectMessage(FinishGridSimulationTrigger(3600))

      inferiorGrid12.gaProbe.expectMessage(FinishGridSimulationTrigger(3600))

      inferiorGrid13.gaProbe.expectMessage(FinishGridSimulationTrigger(3600))

      // after all grids have received a FinishGridSimulationTrigger, the grid agent coordinator should receive the results
      val pfResults = gridAgentCoordinator.expectMessageType[PowerFlowResults]
      pfResults.gridAgent shouldBe centerGridAgent

      // the grid agent coordinator will send the results to the result proxy and a Completion to the scheduler
      pfResults.results.foreach(resultProxy ! _)
      scheduler ! Completion(gridAgentCoordinator.ref, Some(7200))
      scheduler.expectMessageType[Completion].newTick shouldBe Some(7200)

      val resultMessage = resultProxy.expectMessageType[ResultEvent]
      resultMessage match {
        case powerFlowResultEvent: PowerFlowResultEvent =>
          // we expect results for 4 nodes, 5 lines and 2 transformer2ws
          powerFlowResultEvent.nodeResults.size shouldBe 4
          powerFlowResultEvent.lineResults.size shouldBe 5
          powerFlowResultEvent.transformer2wResults.size shouldBe 2

          // due to the fact that the used grid does not contain any switches or transformer3ws
          // we do not expect any results for the following elements
          powerFlowResultEvent.transformer3wResults shouldBe empty
          powerFlowResultEvent.switchResults shouldBe empty

        case x =>
          fail(
            s"Invalid message received when expecting a PowerFlowResultEvent message for simulate grid! Message was $x"
          )
      }
    }

  }
}
