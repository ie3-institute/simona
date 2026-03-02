/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.data.GridAgentData.{
  GridAgentConstantData,
  GridAgentRef,
}
import edu.ie3.simona.agent.grid.powerflow.PowerFlowParams
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.service.load.LoadProfileService
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.service.results.ResultServiceProxy
import edu.ie3.simona.service.weather.WeatherService
import edu.ie3.simona.test.common.model.grid.DbfsTestGrid
import edu.ie3.simona.test.common.{ConfigTestData, UnitSpec}
import org.apache.pekko.actor.testkit.typed.Effect.{Spawned, Watched}
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  BehaviorTestKit,
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.Behavior
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar

import java.util.UUID

class GridAgentCoordinatorSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with DbfsTestGrid
    with ConfigTestData
    with MockitoSugar {

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

  given environmentRefs: EnvironmentRefs = EnvironmentRefs(
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

  private val cfg = simonaConfig.simona

  given GridAgentConstantData = GridAgentConstantData(
    gridAgentCoordinator.ref,
    environmentRefs,
    simonaConfig.simona,
    3600L,
    startTime,
    endTime,
  )

  "The GridAgentCoordinator" should {

    "build reference maps correctly" in {
      var (subgridToRef, refToNodes) =
        (Map.empty[Int, GridAgentRef], Map.empty[GridAgentRef, Set[UUID]])

      val testKit =
        BehaviorTestKit(Behaviors.setup[GridAgentCoordinator.Message] { ctx =>
          val (_subgridToRef, _refToNodes) =
            GridAgentCoordinator.createGridAgents(
              Iterable(hvGridContainer, ehvGridContainer),
              ctx,
              PowerFlowParams(cfg.powerflow.value),
            )

          subgridToRef = _subgridToRef
          refToNodes = _refToNodes

          Behaviors.stopped
        })

      subgridToRef.keySet shouldBe Set(1000, 1)

      refToNodes(subgridToRef(1)) shouldBe Set(
        supNodeA.getUuid,
        supNodeB.getUuid,
      )
      refToNodes(subgridToRef(1000)) shouldBe Set.empty

      // two actor should be spawned
      testKit.expectEffectPF { case Spawned(_, actorName, _) =>
        actorName shouldBe "GridAgent-1"
      }

      testKit.expectEffectPF { case Watched(gridRef) =>
        subgridToRef(1) shouldBe gridRef
      }

      testKit.expectEffectPF { case Spawned(_, actorName, _) =>
        actorName shouldBe "GridAgent-1000"
      }

      testKit.expectEffectPF { case Watched(gridRef) =>
        subgridToRef(1000) shouldBe gridRef
      }
    }
  }

}
