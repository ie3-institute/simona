/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentCoordinator.FinishedInitialization
import edu.ie3.simona.agent.grid.GridAgentMessages.{
  CompleteInitialization,
  DoPowerFlowTrigger,
  RegisterSuperiorGrid,
}
import edu.ie3.simona.agent.grid.data.GridAgentData.{
  GridAgentBaseData,
  GridAgentConstantData,
  GridAgentInitData,
}
import edu.ie3.simona.agent.grid.powerflow.{DBFSMockGridAgents, PowerFlowParams}
import edu.ie3.simona.model.grid.ampacity.AmpacityCalculationParams
import edu.ie3.simona.model.grid.ampacity.LineSegmentThermalModel
import edu.ie3.simona.model.grid.{GridModel, RefSystem, VoltageLimits}
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.ServiceMessage
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  RegistrationFailedMessage,
  SecondaryServiceRegistrationMessage,
}
import edu.ie3.simona.service.Data.SecondaryData.CurrentVoltage
import edu.ie3.simona.service.load.LoadProfileService
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.service.results.ResultServiceProxy
import edu.ie3.simona.service.results.ResultServiceProxy.ExpectResult
import edu.ie3.simona.service.weather.WeatherService
import edu.ie3.simona.service.weather.WeatherService.WeatherRegistrationData
import edu.ie3.simona.test.common.input.LineSegmentThermalModelInputData
import edu.ie3.simona.test.common.model.grid.DbfsTestGrid
import edu.ie3.simona.test.common.{
  ConfigTestData,
  TestSpawnerTyped,
  WeatherTestData,
}
import edu.ie3.simona.util.Coordinate
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef
import squants.electro.Kilovolts
import squants.thermal.{Celsius, Kelvin, Temperature}
import scala.concurrent.duration.*

/** Tests the weather data flow for the [[GridAgent]] thermal line segment
  * ampacity calculation: eager initialization of the thermal line states with a
  * fixed ground temperature, per-segment buffering of the received weather
  * data, and the weather-based ground temperature update via
  * [[LineSegmentThermalModel.handleInput]].
  */
class GridAgentWeatherSpec
    extends ScalaTestWithActorTestKit
    with DBFSMockGridAgents
    with ConfigTestData
    with DbfsTestGrid
    with TestSpawnerTyped
    with WeatherTestData
    with LineSegmentThermalModelInputData {

  // Tolerance
  given Temperature = Kelvin(1e-3)

  private val scheduler: TestProbe[SchedulerMessage] = TestProbe("scheduler")
  private val runtimeEvents: TestProbe[
    edu.ie3.simona.event.RuntimeEvent
  ] = TestProbe("runtimeEvents")
  private val primaryService =
    TestProbe[PrimaryServiceProxy.Message]("primaryService")
  private val resultProxy =
    TestProbe[ResultServiceProxy.Message]("resultProxy")
  private val weatherService =
    TestProbe[WeatherService.Message]("weatherService")
  private val loadProfileService =
    TestProbe[LoadProfileService.Message]("loadProfileService")
  private val gridAgentCoordinator: TestProbe[GridAgentCoordinator.Message] =
    TestProbe("gridAgentCoordinator")
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

  /** A [[GridAgentConstantData]] with ampacity calculation activated, used for
    * tests that exercise the ampacity path of the grid agent.
    */
  private val ampacityConstantData: GridAgentConstantData =
    GridAgentConstantData(
      gridAgentCoordinator.ref,
      environmentRefs,
      simonaConfig.copy(
        ampacityCalculation =
          edu.ie3.simona.config.SimonaConfig.AmpacityCalculation(
            activateAmpacityCalculation = true
          )
      ),
      3600,
      startTime,
      endTime,
    )

  /** A [[GridModel]] based on the test grid, augmented with the test thermal
    * line segment and its calculation point.
    */
  private val gridModelWithThermalSegment = {
    val baseModel = GridModel(
      hvGridContainer,
      RefSystem("2000 MVA", "110 kV"),
      VoltageLimits(0.9, 1.1),
      startTime,
      endTime,
      simonaConfig,
    )
    baseModel.copy(
      gridComponents = baseModel.gridComponents.copy(
        thermalLineSegments = Set(cigreLandCable33kVlineSegmentThermalModel),
        segmentCoordinates = Map(
          cigreLandCable33kVlineSegmentThermalModel.uuid -> Coordinate(
            51.0,
            7.0,
          )
        ),
      )
    )
  }

  /** A [[GridModel]] based on the test grid without any thermal line segment.
    */
  private val gridModelWithoutThermalSegments: GridModel = GridModel(
    hvGridContainer,
    RefSystem("2000 MVA", "110 kV"),
    VoltageLimits(0.9, 1.1),
    startTime,
    endTime,
    simonaConfig,
  )
  private val superiorGridAgent = SuperiorGA(
    TestProbe("superiorGridAgent_1000"),
    Seq(supNodeA.getUuid, supNodeB.getUuid),
  )

  /** Spawns a fully initialized [[GridAgent]] whose grid contains the test
    * thermal line segment. The agent registers itself for the weather service
    * during initialization, so the registration message is already queued at
    * the [[weatherService]] probe.
    *
    * @return
    *   The initialized [[GridAgent]] actor reference.
    */
  private def spawnInitializedGridAgent(): ActorRef[GridAgent.Message] = {
    val gridAgentInitData = GridAgentInitData(
      gridModelWithThermalSegment,
      startTime,
      AmpacityCalculationParams(activateAmpacityCalculation = true),
      PowerFlowParams(simonaConfig.powerflow.value),
    )
    val gridAgent = testKit.spawn(GridAgent(gridAgentInitData))
    gridAgent ! RegisterSuperiorGrid(
      superiorGridAgent.ref,
      superiorGridAgent.nodeUuids.toSet,
      1000,
    )
    gridAgent ! CompleteInitialization(false)
    gridAgentCoordinator
      .expectMessageType[FinishedInitialization]
      .gridRef shouldBe
      gridAgent
    gridAgent
  }

  /** Spawns a fully initialized [[GridAgent]] whose grid contains no thermal
    * line segments. Such an agent must not register itself with the weather
    * service.
    *
    * @return
    *   The initialized [[GridAgent]] actor reference.
    */
  private def spawnInitializedGridAgentWithoutThermalSegments(): ActorRef[
    GridAgent.Message
  ] = {
    val gridAgentInitData = GridAgentInitData(
      gridModelWithoutThermalSegments,
      startTime,
      AmpacityCalculationParams(activateAmpacityCalculation = true),
      PowerFlowParams(simonaConfig.powerflow.value),
    )
    val gridAgent = testKit.spawn(GridAgent(gridAgentInitData))
    gridAgent ! RegisterSuperiorGrid(
      superiorGridAgent.ref,
      superiorGridAgent.nodeUuids.toSet,
      1000,
    )
    gridAgent ! CompleteInitialization(false)
    gridAgentCoordinator
      .expectMessageType[FinishedInitialization]
      .gridRef shouldBe
      gridAgent
    gridAgent
  }

  "A GridAgent with thermal line segments" should {
    "register its segments with the weather service, keyed by segment uuid" in {
      spawnInitializedGridAgent()
      val registration =
        weatherService.expectMessageType[ServiceMessage] match {
          case reg: SecondaryServiceRegistrationMessage => reg
          case other =>
            fail(s"Expected a weather registration message, but got: $other")
        }
      registration.data shouldBe WeatherRegistrationData(
        Coordinate(51.0, 7.0),
        Some(cigreLandCable33kVlineSegmentThermalModel.uuid.toString),
      )
    }
    "initialize its thermal line states eagerly with a fixed ground temperature" in {
      val baseData =
        GridAgentBaseData.create(
          gridModelWithThermalSegment,
          Map.empty,
          Map.empty,
          Map.empty,
          Map.empty,
          startTime,
          AmpacityCalculationParams(activateAmpacityCalculation = true),
          PowerFlowParams(simonaConfig.powerflow.value),
          "testGridAgent",
        )
      // The thermal line states are initialized eagerly with a fixed ground temperature of 10 °C
      baseData.thermalLineStates should not be empty
      val lineState = baseData.thermalLineStates(
        cigreLandCable33kVlineSegmentThermalModel.uuid
      )
      lineState.groundTemperature should approximate(Celsius(10d))
      // The ground temperature is updated from the weather data via
      // handleInput (used in afterPowerFlow) once weather is available
      val updatedWeather = weatherData.copy(
        groundTempLvl3 = Some(Celsius(15)),
        groundTempLvl4 = Some(Celsius(10)),
      )
      val lineStateWithInput =
        cigreLandCable33kVlineSegmentThermalModel.handleInput(
          lineState,
          Seq(
            CurrentVoltage(
              cigreLandCable33kVlineSegmentThermalModel.uuid,
              Kilovolts(20),
            ),
            updatedWeather,
          ),
        )
      lineStateWithInput.groundTemperature should approximate(
        LineSegmentThermalModel.groundTemperatureFromWeather(
          cigreLandCable33kVlineSegmentThermalModel.depthCables,
          updatedWeather,
        )
      )
    }
    "buffer the last received weather data per segment (sticky)" in {
      val gridAgent = spawnInitializedGridAgent()
      // Consume the registration message sent during initialization
      weatherService.expectMessageType[ServiceMessage]
      val segmentUuid = cigreLandCable33kVlineSegmentThermalModel.uuid

      // First provision is buffered for the segment (latest known wins later).
      gridAgent ! ServiceMessage.DataProvision(
        0L,
        weatherService.ref,
        weatherData,
        Some(3600L),
        Some(segmentUuid.toString),
      )
      // Second provision for the same segment: the latest weather data wins.
      val updatedWeather = weatherData.copy(
        groundTempLvl3 = Some(Celsius(15)),
        groundTempLvl4 = Some(Celsius(10)),
      )
      gridAgent ! ServiceMessage.DataProvision(
        3600L,
        weatherService.ref,
        updatedWeather,
        Some(7200L),
        Some(segmentUuid.toString),
      )
      // A provision with a key that is not a segment UUID is ignored, the
      // buffered state is left untouched and the agent stays alive.
      gridAgent ! ServiceMessage.DataProvision(
        7200L,
        weatherService.ref,
        weatherData,
        Some(10800L),
        Some("not-a-segment-uuid"),
      )

      // The buffering is internal; assert the agent survives all provisions and
      // does not forward spurious messages to its environment.
      gridAgentCoordinator.expectNoMessage(200.millis)
      resultProxy.expectNoMessage(200.millis)
      // Liveness: the agent must still be alive and processing after the
      // weather provisions. A registration failure now terminates it (this
      // only happens if the actor is still running).
      gridAgent ! RegistrationFailedMessage(weatherService.ref)
      val deathWatch = createTestProbe("deathWatch")
      deathWatch.expectTerminated(gridAgent)
    }
  }
  "A GridAgent without thermal line segments" should {
    "not register itself with the weather service" in {
      val gridAgent = spawnInitializedGridAgentWithoutThermalSegments()
      // The agent completes initialization but sends no weather registration
      weatherService.expectNoMessage()
      gridAgent should not be null
    }
  }
}
