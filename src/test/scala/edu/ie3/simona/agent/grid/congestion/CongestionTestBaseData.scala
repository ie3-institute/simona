/*
 * © 2025-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import com.typesafe.config.ConfigFactory
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.data.GridAgentData.{
  GridAgentBaseData,
  GridAgentConstantData,
}
import edu.ie3.simona.agent.grid.{
  GridAgent,
  GridAgentCoordinator,
  GridEnvironment,
}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.model.grid.{GridModel, RefSystem, VoltageLimits}
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.service.load.LoadProfileService
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.service.results.ResultServiceProxy
import edu.ie3.simona.service.weather.WeatherService
import edu.ie3.simona.test.common.result.CongestedComponentsTestData
import edu.ie3.simona.test.common.{ConfigTestData, TestSpawnerTyped}
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ActorTestKitBase,
  TestProbe,
}
import org.apache.pekko.actor.typed.scaladsl.{
  ActorContext,
  Behaviors,
  StashBuffer,
}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.mockito.Mockito.when
import squants.electro.Kilovolts
import squants.energy.Megawatts

trait CongestionTestBaseData
    extends ConfigTestData
    with CongestedComponentsTestData
    with TestSpawnerTyped {
  this: ActorTestKitBase =>

  protected val config: SimonaConfig = SimonaConfig(
    ConfigFactory
      .parseString("""
        |simona.congestionManagement.enableDetection = true
        |""".stripMargin)
      .withFallback(typesafeConfig)
      .resolve()
  )

  protected val refSystem: RefSystem = RefSystem(
    Megawatts(600d),
    Kilovolts(110d),
  )

  protected val voltageLimits: VoltageLimits = VoltageLimits(0.9, 1.1)

  protected val gridAgentCoordinator: TestProbe[GridAgentCoordinator.Message] =
    TestProbe("gridAgentCoordinator")

  protected val scheduler: TestProbe[SchedulerMessage] = TestProbe("scheduler")
  protected val runtimeEvents: TestProbe[RuntimeEvent] = TestProbe(
    "runtimeEvents"
  )
  protected val primaryService: TestProbe[PrimaryServiceProxy.Message] =
    TestProbe(
      "primaryService"
    )
  protected val resultProxy: TestProbe[ResultServiceProxy.Message] = TestProbe(
    "resultServiceProxy"
  )
  protected val weatherService: TestProbe[WeatherService.Message] = TestProbe(
    "weatherService"
  )
  protected val loadProfileService: TestProbe[LoadProfileService.Message] =
    TestProbe(
      "loadProfileService"
    )

  protected val environmentRefs: EnvironmentRefs = EnvironmentRefs(
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

  protected given constantData: GridAgentConstantData =
    GridAgentConstantData(
      gridAgentCoordinator.ref,
      environmentRefs,
      simonaConfig,
      3600,
      startTime,
      endTime,
    )

  def behaviorWithContextAndBuffer(
      factory: (
          ctx: ActorContext[GridAgent.Message],
          buffer: StashBuffer[GridAgent.Message],
      ) => Behavior[GridAgent.Message]
  )(using
      capacity: Int = 10
  ): Behavior[GridAgent.Message] = Behaviors.withStash(capacity) { buffer =>
    Behaviors.setup { ctx =>
      factory(ctx, buffer)
    }
  }

  def spawnWithBuffer(
      factory: StashBuffer[GridAgent.Message] => Behavior[GridAgent.Message],
      capacity: Int = 10,
  ): ActorRef[GridAgent.Message] =
    testKit.spawn(
      Behaviors.withStash(capacity) { buffer =>
        factory(buffer)
      }
    )

  def gridAgentBaseData(
      inferiorRefs: Set[ActorRef[GridAgent.Message]] = Set.empty,
      isSuperior: Boolean = false,
      gridModel: Option[GridModel] = None,
  ): GridAgentBaseData = {
    val data = mock[GridAgentBaseData]
    val map = inferiorRefs.map(ref => ref -> Set.empty).toMap

    when(data.isSuperior).thenReturn(isSuperior)
    when(data.inferiorGridRefs).thenReturn(map)

    val gridEnv = mock[GridEnvironment]
    when(data.gridEnv).thenReturn(gridEnv)

    when(gridEnv.superiorConnections).thenReturn(Map.empty)
    when(gridEnv.nodeToAssetAgents).thenReturn(Map.empty)

    gridModel match {
      case Some(model) =>
        when(gridEnv.gridModel).thenReturn(model)

      case None =>
        when(gridEnv.gridModel).thenReturn(defaultGridModel)
    }

    data
  }

}
