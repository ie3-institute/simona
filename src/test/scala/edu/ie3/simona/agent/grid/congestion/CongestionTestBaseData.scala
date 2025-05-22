/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import com.typesafe.config.ConfigFactory
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentData.{
  GridAgentBaseData,
  GridAgentConstantData,
}
import edu.ie3.simona.agent.grid.{GridAgent, GridEnvironment}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.model.grid.{GridModel, VoltageLimits}
import edu.ie3.simona.ontology.messages.services.{
  LoadProfileMessage,
  ServiceMessage,
  WeatherMessage,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.test.common.model.grid.DbfsTestGrid
import edu.ie3.simona.test.common.{ConfigTestData, TestSpawnerTyped}
import edu.ie3.util.TimeUtil
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

import java.time.ZonedDateTime
import scala.concurrent.duration.DurationInt

trait CongestionTestBaseData
    extends ConfigTestData
    with DbfsTestGrid
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

  val startTime: ZonedDateTime = TimeUtil.withDefaults.toZonedDateTime(
    config.simona.time.startDateTime
  )

  protected val scheduler: TestProbe[SchedulerMessage] = TestProbe("scheduler")
  protected val runtimeEvents: TestProbe[RuntimeEvent] = TestProbe(
    "runtimeEvents"
  )
  protected val primaryService: TestProbe[ServiceMessage] = TestProbe(
    "primaryService"
  )
  protected val weatherService: TestProbe[WeatherMessage] = TestProbe(
    "weatherService"
  )
  protected val loadProfileService: TestProbe[LoadProfileMessage] = TestProbe(
    "loadProfileService"
  )

  protected val environmentRefs: EnvironmentRefs = EnvironmentRefs(
    scheduler = scheduler.ref,
    runtimeEventListener = runtimeEvents.ref,
    primaryServiceProxy = primaryService.ref,
    weather = weatherService.ref,
    loadProfiles = loadProfileService.ref,
    evDataService = None,
  )

  protected val resultListener: TestProbe[ResultEvent] = TestProbe(
    "resultListener"
  )

  protected val gridAgentActivation: TestProbe[Activation] = TestProbe(
    "gridAgentActivation"
  )

  protected val voltageLimits: VoltageLimits = VoltageLimits(0.9, 1.1)

  protected implicit val constantData: GridAgentConstantData =
    GridAgentConstantData(
      environmentRefs,
      simonaConfig,
      Iterable(resultListener.ref),
      3600,
      startTime,
      gridAgentActivation.ref,
    )

  def behaviorWithContextAndBuffer(
      factory: (
          ctx: ActorContext[GridAgent.Request],
          buffer: StashBuffer[GridAgent.Request],
      ) => Behavior[GridAgent.Request]
  )(using
      capacity: Int = 10
  ): Behavior[GridAgent.Request] = Behaviors.withStash(capacity) { buffer =>
    Behaviors.setup { ctx =>
      factory(ctx, buffer)
    }
  }

  def spawnWithBuffer(
      factory: StashBuffer[GridAgent.Request] => Behavior[GridAgent.Request],
      capacity: Int = 10,
  ): ActorRef[GridAgent.Request] =
    testKit.spawn(
      Behaviors.withStash(capacity) { buffer =>
        factory(buffer)
      }
    )

  def gridAgentBaseData(
      inferiorRefs: Set[ActorRef[GridAgent.Request]] = Set.empty,
      isSuperior: Boolean = false,
  ): GridAgentBaseData = {
    val data = mock[GridAgentBaseData]
    val map = inferiorRefs.map(ref => ref -> Seq.empty).toMap

    val cmParams = CongestionManagementParams(
      detectionEnabled = true,
      30.seconds,
    )

    when(data.isSuperior).thenReturn(isSuperior)
    when(data.congestionManagementParams).thenReturn(cmParams)
    when(data.inferiorGridRefs).thenReturn(map)
    when(data.superiorGridNodeUuids).thenReturn(Vector.empty)

    val gridEnv = mock[GridEnvironment]
    when(data.gridEnv).thenReturn(gridEnv)

    val gridModel = mock[GridModel]
    when(gridEnv.gridModel).thenReturn(gridModel)
    when(gridEnv.subgridGateToActorRef).thenReturn(Map.empty)
    when(gridEnv.nodeToAssetAgents).thenReturn(Map.empty)

    when(gridModel.voltageLimits).thenReturn(voltageLimits)

    data
  }

}
