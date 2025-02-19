/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import com.typesafe.config.ConfigFactory
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.GridAgentData.{
  GridAgentBaseData,
  GridAgentConstantData,
}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.test.common.model.grid.DbfsTestGrid
import edu.ie3.simona.test.common.{ConfigTestData, TestSpawnerTyped, UnitSpec}
import edu.ie3.util.TimeUtil
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import org.apache.pekko.actor.typed.scaladsl.{Behaviors, StashBuffer}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.mockito.ArgumentMatchers.anyBoolean
import org.mockito.Mockito.when

import java.time.ZonedDateTime
import scala.concurrent.duration.DurationInt

class CongestionTestBase
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with ConfigTestData
    with DbfsTestGrid
    with TestSpawnerTyped {

  protected val config: SimonaConfig = SimonaConfig(
    ConfigFactory
      .parseString("""
                     |simona.congestionManagement.enableDetection = true
                     |""".stripMargin)
      .withFallback(typesafeConfig)
      .resolve()
  )

  val startTime: ZonedDateTime = TimeUtil.withDefaults.toZonedDateTime(
    simonaConfig.simona.time.startDateTime
  )

  protected val scheduler: TestProbe[SchedulerMessage] = TestProbe("scheduler")
  protected val runtimeEvents: TestProbe[RuntimeEvent] = TestProbe(
    "runtimeEvents"
  )
  protected val primaryService: TestProbe[Nothing] = TestProbe("primaryService")
  protected val weatherService: TestProbe[Nothing] = TestProbe("weatherService")

  protected val environmentRefs: EnvironmentRefs = EnvironmentRefs(
    scheduler = scheduler.ref,
    runtimeEventListener = runtimeEvents.ref,
    primaryServiceProxy = primaryService.ref.toClassic,
    weather = weatherService.ref.toClassic,
    evDataService = None,
  )

  protected val resultListener: TestProbe[ResultEvent] = TestProbe(
    "resultListener"
  )

  protected implicit val constantData: GridAgentConstantData =
    GridAgentConstantData(
      environmentRefs,
      simonaConfig,
      Iterable(resultListener.ref),
      3600,
      startTime,
      mock[ActorRef[Activation]],
    )

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
    when(data.inferiorGridRefs(anyBoolean())).thenReturn(map)

    data
  }

}
