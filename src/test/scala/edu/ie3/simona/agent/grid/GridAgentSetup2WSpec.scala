/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import akka.actor.{Actor, ActorIdentity, ActorRef, ActorSystem, Identify, Props}
import akka.testkit.ImplicitSender
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.io.result.ResultSinkType
import edu.ie3.simona.sim.setup.SimonaStandaloneSetup
import edu.ie3.simona.test.common.input.TransformerInputTestData
import edu.ie3.simona.test.common.{ConfigTestData, TestKitWithShutdown}
import edu.ie3.simona.util.ResultFileHierarchy
import edu.ie3.simona.util.ResultFileHierarchy.ResultEntityPathConfig
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class GridAgentSetup2WSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "GridAgentSetupSpec",
        ConfigFactory
          .parseString("""
            |akka.loggers =["akka.event.slf4j.Slf4jLogger"]
            |akka.loglevel="OFF"
        """.stripMargin)
      )
    )
    with ImplicitSender
    with AnyWordSpecLike
    with TransformerInputTestData
    with ConfigTestData
    with LazyLogging {

  "The setup of grid agents" must {
    "provide two grid agents on presence of a two winding transformer" in {

      import akka.pattern._
      implicit val timeout: Timeout = Timeout(1, TimeUnit.SECONDS)

      // in order to get an actor system we need a tmp actor that calls the corresponding method
      Await.ready(
        system.actorOf(Props(new Actor {
          override def receive: Receive = { case "setup" =>
            val environmentRefs = EnvironmentRefs(
              scheduler = self,
              primaryServiceProxy = self,
              weather = ActorRef.noSender,
              evDataService = None
            )

            SimonaStandaloneSetup(
              simonaConfig,
              ConfigFactory.empty(),
              ResultFileHierarchy(
                "test/tmp",
                "GridAgentSetup2WSpec",
                ResultEntityPathConfig(
                  Set.empty[Class[_ <: ResultEntity]],
                  ResultSinkType(
                    simonaConfig.output.sink,
                    simonaConfig.simulationName
                  )
                )
              )
            ).buildSubGridToActorRefMap(
              gridContainer.getSubGridTopologyGraph,
              context,
              environmentRefs,
              Seq.empty[ActorRef]
            )
            sender() ! "done"
          }
        })) ? "setup",
        Duration(1, TimeUnit.SECONDS)
      )

      val sel = system.actorSelection("user/**/GridAgent_*")
      sel ! Identify(0)

      logger.debug("Waiting 500 ms to collect all responses")
      val responses: Seq[ActorIdentity] =
        receiveWhile(
          max = Duration.create(500, "ms"),
          idle = Duration.create(250, "ms")
        ) { case msg: ActorIdentity =>
          msg
        }
      logger.debug("All responses received. Evaluating...")

      responses.size should be(2)

      val regex = """GridAgent_\d*""".r
      val expectedSenders = Vector("GridAgent_1", "GridAgent_2")
      val actualSenders = responses
        .collect { case actorId: ActorIdentity =>
          val actorRefString = actorId.getActorRef.toString
          regex.findFirstIn(actorRefString)
        }
        .flatten
        .sorted
        .toVector

      actualSenders should be(expectedSenders)
    }
  }
}
