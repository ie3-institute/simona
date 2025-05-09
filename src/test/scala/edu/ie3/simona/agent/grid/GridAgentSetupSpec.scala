/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.sim.setup.SimonaStandaloneSetup
import edu.ie3.simona.test.common.input.TransformerInputTestData
import edu.ie3.simona.test.common.{
  ConfigTestData,
  ThreeWindingTestData,
  UnitSpec,
}
import edu.ie3.simona.util.ResultFileHierarchy
import org.apache.pekko.actor.testkit.typed.Effect.Spawned
import org.apache.pekko.actor.testkit.typed.scaladsl.BehaviorTestKit
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.scalatestplus.mockito.MockitoSugar

class GridAgentSetupSpec
    extends UnitSpec
    with MockitoSugar
    with TransformerInputTestData
    with ConfigTestData
    with ThreeWindingTestData {

  "The setup of grid agents" must {

    "provide two grid agents on presence of a two winding transformer" in {

      val testKit = BehaviorTestKit(Behaviors.setup[AnyRef] { ctx =>
        SimonaStandaloneSetup(
          typesafeConfig,
          simonaConfig,
          mock[ResultFileHierarchy],
        ).buildSubGridToActorRefMap(
          gridContainer.getSubGridTopologyGraph,
          ctx,
          mock[EnvironmentRefs],
          Seq.empty,
        )

        Behaviors.stopped
      })

      // two actor should be spawned
      testKit.expectEffectPF { case Spawned(_, actorName, _) =>
        actorName shouldBe "1"
      }

      testKit.expectEffectPF { case Spawned(_, actorName, _) =>
        actorName shouldBe "2"
      }
    }

    "provide three grid agents on presence of a three winding transformer" in {

      val testKit = BehaviorTestKit(Behaviors.setup[AnyRef] { ctx =>
        SimonaStandaloneSetup(
          typesafeConfig,
          simonaConfig,
          mock[ResultFileHierarchy],
        ).buildSubGridToActorRefMap(
          threeWindingTestGrid.getSubGridTopologyGraph,
          ctx,
          mock[EnvironmentRefs],
          Seq.empty,
        )

        Behaviors.stopped
      })

      // three actor should be spawned
      testKit.expectEffectPF { case Spawned(_, actorName, _) =>
        actorName shouldBe "1"
      }

      testKit.expectEffectPF { case Spawned(_, actorName, _) =>
        actorName shouldBe "2"
      }

      testKit.expectEffectPF { case Spawned(_, actorName, _) =>
        actorName shouldBe "3"
      }
    }
  }
}
