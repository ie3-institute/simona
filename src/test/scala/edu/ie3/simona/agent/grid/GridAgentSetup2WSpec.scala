/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentMessage.{
  FinishGridSimulationTrigger,
  PrepareNextSweepTrigger,
}
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.io.result.ResultSinkType
import edu.ie3.simona.sim.setup.SimonaStandaloneSetup
import edu.ie3.simona.test.common.ConfigTestData
import edu.ie3.simona.test.common.input.TransformerInputTestData
import edu.ie3.simona.util.ResultFileHierarchy
import edu.ie3.simona.util.ResultFileHierarchy.ResultEntityPathConfig
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  BehaviorTestKit,
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.{ActorRef, Scheduler}
import org.apache.pekko.actor.typed.receptionist.Receptionist.{
  Listing,
  Register,
  Subscribe,
}
import org.apache.pekko.actor.typed.receptionist.ServiceKey
import org.apache.pekko.actor.typed.scaladsl.AskPattern.Askable
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.scaladsl.adapter.{
  TypedActorContextOps,
  TypedActorRefOps,
}
import org.apache.pekko.actor.{
  ActorContext => ClassicContext,
  ActorRef => ClassicRef,
}
import org.apache.pekko.util.Timeout
import org.mockito.Mock
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class GridAgentSetup2WSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with TransformerInputTestData
    with ConfigTestData
    with LazyLogging {

  "The setup of grid agents" must {

    val scheduler = TestProbe("scheduler")
    val runtimeEventListener = TestProbe[RuntimeEvent]("listener")
    val primaryServiceProxy = TestProbe("primaryServiceProxy")

    val environmentRefs = EnvironmentRefs(
      scheduler.ref.toClassic,
      runtimeEventListener.ref,
      primaryServiceProxy = primaryServiceProxy.ref.toClassic,
      weather = ClassicRef.noSender,
      evDataService = None,
    )

    "provide two grid agents on presence of a two winding transformer" in {

      implicit val timeout: Timeout = Timeout(1, TimeUnit.SECONDS)
      implicit val scheduler: Scheduler = system.scheduler

      // in order to get an actor system we need a tmp actor that calls the corresponding method
      val serviceKey = ServiceKey[GridAgentMessage]("gridAgent")

      val actor = testKit.spawn(Behaviors.setup[GridAgentMessage] { ctx =>
        ctx.system.receptionist ! Register(serviceKey, ctx.self)

        Behaviors.receive[GridAgentMessage] {
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
              ctx.toClassic,
              environmentRefs,
              Seq.empty[ClassicRef],
            )

            ctx.self ! FinishGridSimulationTrigger(tick)
            Behaviors.same

          case other =>
            fail(s"$other was not expected")
        }
      })

      Await.ready(
        actor.ask[GridAgentMessage](_ => PrepareNextSweepTrigger(0)),
        Duration(10, TimeUnit.SECONDS),
      )

      BehaviorTestKit(Behaviors.setup[Listing] { ctx =>
        logger.debug("Subscribing to the actors.")
        ctx.system.receptionist ! Subscribe(serviceKey, ctx.self)

        Behaviors.receiveMessagePartial[Listing] {
          case serviceKey.Listing(listings) =>
            logger.debug("All responses received. Evaluating...")

            listings.size should be(2)

            val regex = """GridAgent_\d*""".r
            val expectedSenders = Vector("GridAgent_1", "GridAgent_2")
            val actualSenders = listings
              .collect { case actorId: ActorRef[GridAgentMessage] =>
                val actorRefString = actorId.toString
                regex.findFirstIn(actorRefString)
              }
              .flatten
              .toVector

            actualSenders should be(expectedSenders)

            Behaviors.same
        }
      })
    }
  }
}
