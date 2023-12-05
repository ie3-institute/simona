/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import org.apache.pekko.actor.{
  Actor,
  ActorIdentity,
  ActorRef,
  ActorSystem,
  Identify,
  Props
}
import org.apache.pekko.testkit.ImplicitSender
import org.apache.pekko.util.Timeout
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.io.result.ResultSinkType
import edu.ie3.simona.sim.setup.SimonaStandaloneSetup
import edu.ie3.simona.test.common.{
  ConfigTestData,
  TestKitWithShutdown,
  ThreeWindingTestData
}
import edu.ie3.simona.util.ResultFileHierarchy
import edu.ie3.simona.util.ResultFileHierarchy.ResultEntityPathConfig
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class GridAgentSetup3WSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "GridAgentSetupSpec",
        ConfigFactory
          .parseString("""
            |pekko.loggers =["org.apache.pekko.event.slf4j.Slf4jLogger"]
            |pekko.loglevel="DEBUG"
        """.stripMargin)
      )
    )
    with ImplicitSender
    with AnyWordSpecLike
    with ThreeWindingTestData
    with ConfigTestData
    with LazyLogging {

  "The setup of grid agents" must {
    "provide three grid agents on presence of a three winding transformer" in {
      import org.apache.pekko.pattern._
      implicit val timeout: Timeout = Timeout(1, TimeUnit.SECONDS)

      // in order to get an actor system we need a tmp actor that calls the corresponding method
      Await.ready(
        system.actorOf(Props(new Actor {
          override def receive: Receive = { case "setup" =>
            val environmentRefs = EnvironmentRefs(
              scheduler = self,
              runtimeEventListener = self,
              primaryServiceProxy = self,
              weather = ActorRef.noSender,
              evDataService = None
            )

            SimonaStandaloneSetup(
              typesafeConfig,
              ResultFileHierarchy(
                "test/tmp",
                "GridAgentSetup3WSpec",
                ResultEntityPathConfig(
                  Set.empty[Class[_ <: ResultEntity]],
                  ResultSinkType(
                    simonaConfig.simona.output.sink,
                    simonaConfig.simona.simulationName
                  )
                )
              )
            ).buildSubGridToActorRefMap(
              threeWindingTestGrid.getSubGridTopologyGraph,
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

      logger.debug("Waiting 500ms to collect all responses")
      val responses: Seq[ActorIdentity] =
        receiveWhile(
          max = Duration.create(500, "ms"),
          idle = Duration.create(250, "ms")
        ) { case msg: ActorIdentity =>
          msg
        }
      logger.debug("All responses received. Evaluating...")

      responses.size should be(3)
      val regex = """GridAgent_\d*""".r
      val expectedSenders = Vector("GridAgent_1", "GridAgent_2", "GridAgent_3")
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
