/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentMessage.StringAdapter
import edu.ie3.simona.io.result.ResultSinkType
import edu.ie3.simona.sim.setup.SimonaStandaloneSetup
import edu.ie3.simona.test.common.ConfigTestData
import edu.ie3.simona.test.common.input.TransformerInputTestData
import edu.ie3.simona.util.ResultFileHierarchy
import edu.ie3.simona.util.ResultFileHierarchy.ResultEntityPathConfig
import org.apache.pekko.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.apache.pekko.actor.typed.receptionist.Receptionist.{
  Listing,
  Register,
  Subscribe
}
import org.apache.pekko.actor.typed.receptionist.ServiceKey
import org.apache.pekko.actor.typed.scaladsl.AskPattern.Askable
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.scaladsl.adapter.{
  TypedActorContextOps,
  TypedActorRefOps
}
import org.apache.pekko.actor.typed.{ActorRef, Scheduler}
import org.apache.pekko.actor.{ActorRef => classicRef}
import org.apache.pekko.util.Timeout
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
    "provide two grid agents on presence of a two winding transformer" in {

      implicit val timeout: Timeout = Timeout(1, TimeUnit.SECONDS)
      implicit val scheduler: Scheduler = system.scheduler

      // in order to get an actor system we need a tmp actor that calls the corresponding method
      val serviceKey = ServiceKey[GridAgentMessage]("gridAgent")

      val actor = testKit.spawn(Behaviors.setup[GridAgentMessage] { ctx =>
        ctx.system.receptionist ! Register(serviceKey, ctx.self)

        Behaviors.receive[GridAgentMessage] {
          case (ctx, StringAdapter("ping", sender)) =>
            // replying to ping signal
            sender ! StringAdapter("pong", ctx.self)
            Behaviors.same
          case (ctx, StringAdapter("setup", _)) =>
            val environmentRefs = EnvironmentRefs(
              scheduler = ctx.self.toClassic,
              runtimeEventListener = ctx.self.toClassic,
              primaryServiceProxy = ctx.self.toClassic,
              weather = classicRef.noSender,
              evDataService = None
            )

            SimonaStandaloneSetup(
              typesafeConfig,
              ResultFileHierarchy(
                "test/tmp",
                "GridAgentSetup2WSpec",
                ResultEntityPathConfig(
                  Set.empty[Class[_ <: ResultEntity]],
                  ResultSinkType(
                    simonaConfig.simona.output.sink,
                    simonaConfig.simona.simulationName
                  )
                )
              )
            ).buildSubGridToActorRefMap(
              gridContainer.getSubGridTopologyGraph,
              ctx.toClassic,
              environmentRefs,
              Seq.empty[classicRef]
            )
            ctx.self ! StringAdapter("done", ctx.self)
            Behaviors.same
        }
      })

      Await.ready(
        actor.ask[GridAgentMessage](ref => StringAdapter("setup", ref)),
        Duration(10, TimeUnit.SECONDS)
      )

      testKit.spawn(Behaviors.setup[Listing] { ctx =>
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
