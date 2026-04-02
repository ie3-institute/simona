/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentCoordinator.{
  CongestionResult,
  RegisterAssets,
  StateData,
}
import edu.ie3.simona.agent.grid.GridAgentMessages.RegisterParticipants
import edu.ie3.simona.agent.grid.congestion.CongestionManagementMessages.{
  GotoIdle,
  NextStep,
}
import edu.ie3.simona.agent.grid.congestion.mitigations.MitigationSteps
import edu.ie3.simona.agent.grid.congestion.{
  CongestionManagementParams,
  Congestions,
}
import edu.ie3.simona.agent.grid.data.GridAgentData.{
  GridAgentConstantData,
  GridAgentRef,
}
import edu.ie3.simona.agent.grid.powerflow.PowerFlowParams
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.service.load.LoadProfileService
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.service.results.ResultServiceProxy
import edu.ie3.simona.service.weather.WeatherService
import edu.ie3.simona.test.common.model.grid.DbfsTestGrid
import edu.ie3.simona.test.common.{ConfigTestData, UnitSpec}
import edu.ie3.simona.util.ReceiveDataMap
import org.apache.pekko.actor.testkit.typed.Effect.{Spawned, Watched}
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  BehaviorTestKit,
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.scaladsl.Behaviors
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

  private val cfg = simonaConfig

  given GridAgentConstantData = GridAgentConstantData(
    gridAgentCoordinator.ref,
    environmentRefs,
    simonaConfig,
    3600L,
    startTime,
    endTime,
  )

  "The GridAgentCoordinator" should {

    "create no grid agents if no power flow is configured" in {
      val cfgWithoutPf = simonaConfig.copy(powerflow = None)

      testKit.spawn(
        GridAgentCoordinator(
          cfgWithoutPf,
          Seq(ehvGridContainer, hvGridContainer),
        )
      )

      // no grid agent is spawned and the scheduler will receive no schedule activation message
      scheduler.expectNoMessage()
    }

    "handles assets correctly" in {
      val subgrid1 = TestProbe[GridAgent.Message]("grid1")
      val subgrid2 = TestProbe[GridAgent.Message]("grid2")

      val participant11 = TestProbe[ParticipantAgent.Request]("participant11")
      val participant12 = TestProbe[ParticipantAgent.Request]("participant12")
      val participant21 = TestProbe[ParticipantAgent.Request]("participant21")
      val participant31 = TestProbe[ParticipantAgent.Request]("participant31")

      val node11 = UUID.randomUUID()
      val node12 = UUID.randomUUID()
      val node21 = UUID.randomUUID()
      val node31 = UUID.randomUUID()

      val nodeToSubgrid =
        Map(node11 -> 1, node12 -> 1, node21 -> 2, node31 -> 3)
      val nodeToAssets = Map(
        node11 -> Set(participant11.ref),
        node12 -> Set(participant12.ref),
        node21 -> Set(participant21.ref),
        node31 -> Set(participant31.ref),
      )

      val stateData = StateData(
        scheduler.ref,
        CongestionManagementParams(false, false),
        resultProxy.ref,
        startTime,
        gridAgentsRef = Set(subgrid1.ref, subgrid2.ref),
        nodeToSubgrid = nodeToSubgrid,
      )

      BehaviorTestKit(
        GridAgentCoordinator.initializing(
          stateData,
          Set.empty,
          Map(1 -> subgrid1.ref, 2 -> subgrid2.ref),
        )
      ).run(RegisterAssets(nodeToAssets))

      // participant 31 should not be registered
      subgrid1
        .expectMessageType[RegisterParticipants]
        .nodeToAssets shouldBe Map(
        node11 -> Set(participant11.ref),
        node12 -> Set(participant12.ref),
      )
      subgrid2
        .expectMessageType[RegisterParticipants]
        .nodeToAssets shouldBe Map(node21 -> Set(participant21.ref))
    }

    "awaits and handles no congestions correctly" in {
      val superiorGrid1 = TestProbe[GridAgent.Message]("superiorGrid1")
      val superiorGrid2 = TestProbe[GridAgent.Message]("superiorGrid2")

      val gridRefs = Set(superiorGrid1.ref, superiorGrid2.ref)

      val stateData = StateData(
        scheduler.ref,
        CongestionManagementParams(true, true),
        resultProxy.ref,
        startTime,
        3600,
        Some(3600),
        gridRefs,
        gridRefs,
      )

      val behavior = BehaviorTestKit(
        GridAgentCoordinator.awaitCongestionResults(
          stateData,
          ReceiveDataMap(gridRefs),
        )
      )

      behavior.run(CongestionResult(superiorGrid1.ref, Congestions.none))
      behavior.run(CongestionResult(superiorGrid2.ref, Congestions.none))

      superiorGrid1.expectMessageType[GotoIdle.type]
      superiorGrid2.expectMessageType[GotoIdle.type]
    }

    "awaits and handles congestions correctly" in {
      val superiorGrid1 = TestProbe[GridAgent.Message]("superiorGrid1")
      val superiorGrid2 = TestProbe[GridAgent.Message]("superiorGrid2")

      val gridRefs = Set(superiorGrid1.ref, superiorGrid2.ref)

      val stateData = StateData(
        scheduler.ref,
        CongestionManagementParams(true, true),
        resultProxy.ref,
        startTime,
        3600,
        Some(3600),
        gridRefs,
        gridRefs,
      )

      val behavior = BehaviorTestKit(
        GridAgentCoordinator.awaitCongestionResults(
          stateData,
          ReceiveDataMap(gridRefs),
        )
      )

      behavior.run(CongestionResult(superiorGrid1.ref, Congestions.none))
      behavior.run(
        CongestionResult(superiorGrid2.ref, Congestions(true, false, false))
      )

      superiorGrid1
        .expectMessageType[NextStep]
        .step shouldBe MitigationSteps.TransformerTapChange
      superiorGrid2
        .expectMessageType[NextStep]
        .step shouldBe MitigationSteps.TransformerTapChange
    }

    "build reference maps correctly" in {
      var (subgridToRef, refToNodes) =
        (Map.empty[Int, GridAgentRef], Map.empty[GridAgentRef, Set[UUID]])

      val testKit =
        BehaviorTestKit(Behaviors.setup[GridAgentCoordinator.Message] { ctx =>
          val (_subgridToRef, _refToNodes) =
            GridAgentCoordinator.createGridAgents(
              Seq(hvGridContainer, ehvGridContainer),
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
