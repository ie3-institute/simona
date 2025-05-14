/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import com.typesafe.config.ConfigFactory
import edu.ie3.util.quantities.QuantityUtils.asPu
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.{GridAgent, GridEnvironment}
import edu.ie3.simona.agent.grid.GridAgentData.{
  GridAgentBaseData,
  GridAgentConstantData,
}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.grid.{GridModel, RefSystem, VoltageLimits}
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
import org.apache.pekko.actor.typed.scaladsl.{Behaviors, StashBuffer}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.mockito.Mockito.when
import squants.electro.Kilovolts
import squants.energy.Megawatts

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

  protected val refSystem: RefSystem =
    RefSystem(Megawatts(600), Kilovolts(110d))

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

  protected given constantData: GridAgentConstantData =
    GridAgentConstantData(
      environmentRefs,
      simonaConfig,
      Iterable(resultListener.ref),
      3600,
      startTime,
      mock[ActorRef[Activation]],
    )

  def gridAgentBaseData(
      inferiorRefs: Set[ActorRef[GridAgent.Request]] = Set.empty,
      isSuperior: Boolean = false,
      gridModel: Option[GridModel] = None,
  ): GridAgentBaseData = {
    val data = mock[GridAgentBaseData]
    val map = inferiorRefs.map(ref => ref -> Seq.empty).toMap

    val cmParams = CongestionManagementParams(
      detectionEnabled = true,
      enableTransformerTapChange = false,
      30.seconds,
    )

    when(data.isSuperior).thenReturn(isSuperior)
    when(data.congestionManagementParams).thenReturn(cmParams)
    when(data.inferiorGridRefs).thenReturn(map)

    if (gridModel.nonEmpty) {
      val gridEnv = mock[GridEnvironment]
      when(gridEnv.gridModel).thenReturn(
        gridModel.getOrElse(
          throw new CriticalFailureException("No grid model found!")
        )
      )

      when(data.gridEnv).thenReturn(gridEnv)
    }

    data
  }

}
