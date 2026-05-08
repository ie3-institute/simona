/*
 * © 2020-2026. TU Dortmund University,
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
import edu.ie3.simona.agent.grid.GridAgentMessages.Responses.ExchangePower
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
import edu.ie3.simona.test.common.{ConfigTestData, TestSpawnerTyped, UnitSpec}
import edu.ie3.util.scala.quantities.Megavars
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef
import squants.energy.Megawatts

import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

/** Test to ensure the functions that a [[GridAgent]] in superior position
  * should be able to do if the DBFSAlgorithm is used. The scheduler, the
  * weather service as well as the [[GridAgent]] inferior to the superior
  * [[GridAgent]] are simulated by the TestKit.
  */
class DBFSAlgorithmSupGridSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with ConfigTestData
    with DbfsTestGrid
    with TestSpawnerTyped {

  private val scheduler = TestProbe[SchedulerMessage]("scheduler")
  private val runtimeEvents = TestProbe[RuntimeEvent]("runtimeEvents")
  private val primaryService =
    TestProbe[PrimaryServiceProxy.Message]("primaryService")
  private val resultProxy = TestProbe[ResultServiceProxy.Message]("resultProxy")
  private val weatherService =
    TestProbe[WeatherService.Message]("weatherService")
  private val loadProfileService =
    TestProbe[LoadProfileService.Message]("loadProfileService")

  private val gridAgentCoordinator: TestProbe[GridAgentCoordinator.Message] =
    TestProbe("gridAgentCoordinator")
  private val hvGrid: TestProbe[GridAgent.Message] = TestProbe("hvGrid")

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

  "A GridAgent actor in superior position with async test" should {
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
      PowerFlowParams(simonaConfig.powerflow.value),
    )

    val superiorGridAgent = testKit.spawn(GridAgent(gridAgentInitData))

    s"initialize itself when it receives an init activation" in {
      superiorGridAgent ! RegisterInferiorGrid(
        hvGrid.ref,
        Set(supNodeA.getUuid),
        1,
      )

      superiorGridAgent ! CompleteInitialization(false)

      // mock scheduling behavior
      gridAgentCoordinator
        .expectMessageType[FinishedInitialization]
        .gridRef shouldBe superiorGridAgent
      scheduler ! ScheduleActivation(gridAgentCoordinator.ref, 3600)

      val scheduleActivationMsg =
        scheduler.expectMessageType[ScheduleActivation]
      scheduleActivationMsg.tick shouldBe 3600
      scheduleActivationMsg.unlockKey shouldBe None
    }

    s"start the simulation, do 2 sweeps and should end afterwards when no deviation on nodal " +
      s"power is recognized in the superior when an activation is sent is send" in {

        val requestedConnectionNodeUuids =
          Seq(UUID.fromString("9fe5fa33-6d3b-4153-a829-a16f4347bc4e"))

        // send the start grid simulation trigger
        superiorGridAgent ! DoPowerFlowTrigger(3600)

        for sweepNo <- 0 to 1 do {

          // we expect a request for grid power values here for sweepNo $sweepNo
          val message = hvGrid.expectMessageType[RequestGridPower]

          val lastSender = message match {
            case requestGridPowerMessage: RequestGridPower =>
              requestGridPowerMessage.currentSweepNo shouldBe sweepNo
              requestGridPowerMessage.nodeUuids should contain allElementsOf requestedConnectionNodeUuids

              requestGridPowerMessage.sender
            case x =>
              fail(
                s"Invalid message received when expecting a request for grid power values! Message was $x"
              )
          }

          // we return with a fake grid power message
          // / as we are using the ask pattern, we cannot send it to the grid agent directly but have to send it to the
          // / ask sender
          lastSender ! GridPowerResponse(
            hvGrid.ref,
            requestedConnectionNodeUuids.map(
              ExchangePower(_, hvGrid.ref, Megawatts(0.0), Megavars(0.0))
            ),
          )
        }

        // after all grids have received a FinishGridSimulationTrigger, the grid agent coordinator should receive the results
        val pfResults =
          gridAgentCoordinator.expectMessageType[PowerFlowResults](30.seconds)
        pfResults.gridAgent shouldBe superiorGridAgent

        // the grid agent coordinator will send the results to the result proxy and a Completion to the scheduler
        pfResults.results.foreach(resultProxy ! _)
        scheduler ! Completion(gridAgentCoordinator.ref, Some(7200))
        scheduler.expectMessageType[Completion].newTick shouldBe Some(7200)

        // we expect a completion message here and that the agent goes back to simulate grid
        // and waits until the newly scheduled StartGridSimulationTrigger is sent
        // wait 30 seconds max for power flow to finish

        resultProxy.expectMessageType[ExpectResult]
        // agent should be in Idle again and listener should contain power flow result data
        resultProxy.expectMessageType[ResultEvent] match {
          case powerFlowResultEvent: PowerFlowResultEvent =>
            powerFlowResultEvent.nodeResults.headOption match {
              case Some(value) =>
                value.getvMag().getValue shouldBe 1
                value.getvAng().getValue shouldBe 0
              case None =>
                fail(s"Expected a result but got none.")
            }

            // due to the fact that the used grid does not contain anything besides the one ehv node
            // we do not expect any results for the following elements
            powerFlowResultEvent.lineResults shouldBe empty
            powerFlowResultEvent.switchResults shouldBe empty
            powerFlowResultEvent.transformer2wResults shouldBe empty
            powerFlowResultEvent.transformer3wResults shouldBe empty
        }

        hvGrid.expectMessage(FinishGridSimulationTrigger(3600))
      }

    s"start the simulation when an activation is sent is sent, do 5 sweeps and should end afterwards, if the " +
      s"nodal power exchange converges not before the fifth sweep." in {

        // configuration of the test
        val maxNumberOfTestSweeps = 4
        // / array that holds the deviations that should be recognized
        // // size must be maxNumberOfTestSweeps + 1 and the last two elements MUST be equal, while all other has to be
        // // bigger in difference of p OR q than the epsilon provided in simonaConfig (see above @ head of the test)
        val deviations =
          Array(
            (
              Megawatts(0.0),
              Megavars(0.0),
            ),
            (
              Megawatts(0.1),
              Megavars(0.1),
            ),
            (
              Megawatts(0.0),
              Megavars(0.1),
            ),
            (
              Megawatts(0.0),
              Megavars(0.0),
            ),
            (
              Megawatts(0.0),
              Megavars(0.0),
            ),
          )

        val requestedConnectionNodeUuids =
          Seq(UUID.fromString("9fe5fa33-6d3b-4153-a829-a16f4347bc4e"))

        // send the start grid simulation trigger
        superiorGridAgent ! DoPowerFlowTrigger(3600)

        // go on with testing the sweep behaviour
        for sweepNo <- 0 to maxNumberOfTestSweeps do {

          // we expect a request for grid power values here for sweepNo $sweepNo
          val message = hvGrid.expectMessageType[GridAgent.Message]

          val lastSender = message match {
            case requestGridPowerMessage: RequestGridPower =>
              requestGridPowerMessage.currentSweepNo shouldBe sweepNo
              requestGridPowerMessage.nodeUuids should contain allElementsOf requestedConnectionNodeUuids

              requestGridPowerMessage.sender
            case x =>
              fail(
                s"Invalid message received when expecting a request for grid power values! Message was $x"
              )
          }

          // we return with a fake grid power message
          // / as we are using the ask pattern, we cannot send it to the grid agent directly but have to send it to the
          // / ask sender
          lastSender ! GridPowerResponse(
            hvGrid.ref,
            requestedConnectionNodeUuids.map { uuid =>
              ExchangePower(
                uuid,
                hvGrid.ref,
                deviations(sweepNo)._1,
                deviations(sweepNo)._2,
              )
            },
          )
        }

        // after all grids have received a FinishGridSimulationTrigger, the grid agent coordinator should receive the results
        val pfResults =
          gridAgentCoordinator.expectMessageType[PowerFlowResults](30.seconds)
        pfResults.gridAgent shouldBe superiorGridAgent

        // the grid agent coordinator will send the results to the result proxy and a Completion to the scheduler
        pfResults.results.foreach(resultProxy ! _)
        scheduler ! Completion(gridAgentCoordinator.ref, Some(7200))
        scheduler.expectMessageType[Completion].newTick shouldBe Some(7200)

        // we expect a completion message here and that the agent goes back to simulate grid
        // and waits until the newly scheduled StartGridSimulationTrigger is sent
        // wait 30 seconds max for power flow to finish

        resultProxy.expectMessageType[ExpectResult]
        // agent should be in Idle again and listener should contain power flow result data
        resultProxy.expectMessageType[ResultEvent] match {
          case powerFlowResultEvent: PowerFlowResultEvent =>
            powerFlowResultEvent.nodeResults.headOption match {
              case Some(value) =>
                value.getvMag().getValue shouldBe 1
                value.getvAng().getValue shouldBe 0
              case None =>
                fail(s"Expected a result but got none.")
            }

            // due to the fact that the used grid does not contain anything besides the one ehv node
            // we do not expect any results for the following elements
            powerFlowResultEvent.lineResults shouldBe empty
            powerFlowResultEvent.switchResults shouldBe empty
            powerFlowResultEvent.transformer2wResults shouldBe empty
            powerFlowResultEvent.transformer3wResults shouldBe empty
        }

        // no failed power flow
        runtimeEvents.expectNoMessage()

        hvGrid.expectMessage(FinishGridSimulationTrigger(3600))
      }
  }
}
