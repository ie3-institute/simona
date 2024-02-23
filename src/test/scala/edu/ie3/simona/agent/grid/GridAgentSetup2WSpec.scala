/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentMessage.{
  FinishGridSimulationTrigger,
  PrepareNextSweepTrigger,
}
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.io.result.ResultSinkType
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.sim.setup.SimonaStandaloneSetup
import edu.ie3.simona.test.common.ConfigTestData
import edu.ie3.simona.test.common.input.TransformerInputTestData
import edu.ie3.simona.util.ResultFileHierarchy
import edu.ie3.simona.util.ResultFileHierarchy.ResultEntityPathConfig
import org.apache.pekko.actor.testkit.typed.Effect.Spawned
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  BehaviorTestKit,
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import org.apache.pekko.actor.{ActorRef => ClassicRef}
import org.scalatest.wordspec.AnyWordSpecLike

class GridAgentSetup2WSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with TransformerInputTestData
    with ConfigTestData
    with LazyLogging {

  "The setup of grid agents" must {

    val scheduler: TestProbe[SchedulerMessage] =
      TestProbe[SchedulerMessage]("scheduler")
    val runtimeEventListener = TestProbe[RuntimeEvent]("listener")
    val primaryServiceProxy = TestProbe("primaryServiceProxy")

    val environmentRefs = EnvironmentRefs(
      scheduler.ref,
      runtimeEventListener.ref,
      primaryServiceProxy = primaryServiceProxy.ref.toClassic,
      weather = ClassicRef.noSender,
      evDataService = None,
    )

    "provide two grid agents on presence of a two winding transformer" in {
      val testKit = BehaviorTestKit(Behaviors.receive[GridAgentMessage] {
        case (ctx, PrepareNextSweepTrigger(tick)) =>
          SimonaStandaloneSetup(
            typesafeConfig,
            ResultFileHierarchy(
              "test/tmp",
              "GridAgentSetup2WSpec",
              ResultEntityPathConfig(
                Set.empty[Class[_ <: ResultEntity]],
                ResultSinkType(
                  simonaConfig.simona.output.sink,
                  simonaConfig.simona.simulationName,
                ),
              ),
            ),
          ).buildSubGridToActorRefMap(
            gridContainer.getSubGridTopologyGraph,
            ctx,
            environmentRefs,
            Seq.empty[ClassicRef],
          )

          ctx.self ! FinishGridSimulationTrigger(tick)
          Behaviors.same

        case other =>
          fail(s"$other was not expected")
      })

      testKit.run(PrepareNextSweepTrigger(0))

      // two actor should be spawned
      testKit.expectEffectPF { case Spawned(_, str, _) =>
        str shouldBe "1"
      }

      testKit.expectEffectPF { case Spawned(_, str, _) =>
        str shouldBe "2"
      }
    }
  }
}
