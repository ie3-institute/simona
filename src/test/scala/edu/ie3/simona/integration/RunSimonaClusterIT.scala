/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.integration

import akka.actor.ActorSystem
import akka.cluster.Cluster
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}
import edu.ie3.simona.config.{ConfigFailFast, SimonaConfig}
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.integration.common.IntegrationSpecCommon
import edu.ie3.simona.main.RunSimonaStandalone
import edu.ie3.simona.main.start.SimonaClusterStarter
import edu.ie3.simona.sim.setup.SimonaStandaloneSetup
import edu.ie3.simona.test.common.{AsyncSpec, IOTestCommons}
import edu.ie3.util.io.FileIOUtils
import org.scalatest.BeforeAndAfterAll
import org.scalatest.compatible.Assertion

import java.io.File
import java.util.concurrent.LinkedBlockingQueue
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration.DurationInt

/** Cluster IT tests are currently disabled because on errors, the simulation
  * sometimes does not terminate even with a timeout set and because cluster
  * simulations can behave in unpredictable ways due to node addresses needing
  * to be free upon start.
  */
class RunSimonaClusterIT
    extends IntegrationSpecCommon
    with AsyncSpec
    with BeforeAndAfterAll
    with IOTestCommons {

  // simulation test runs in ~1 minute on a fast laptop
  // this is not passed as implicit in order to enforce the argument in 'run'
  private val timeout: Timeout = 15.minutes

  private val startClusterShardingAllMethod =
    PrivateMethod[ActorSystem](Symbol("startClusterShardingAll"))

  override def afterAll(): Unit = {
    FileIOUtils.deleteRecursively(testTmpDir)
  }

  "A clustered simona simulation" must {

    // replace "ignore" with "in" to enable test
    "run und produce results based on a valid config correctly without grid scheduler" ignore {

      /* setup config */
      val parsedConfig = getConfig

      runClusteredSimulation(parsedConfig)
    }

    def getConfig =
      ConfigFactory
        .parseString(
          s"""|
              |simona.execution.computationMode = ClusterSingleJVM
              |
              |simona.time.startDateTime = "2011-01-01 00:00:00"
              |simona.time.endDateTime =   "2011-01-01 02:00:00"
              |
              |akka.actor.provider = cluster
              |
              |akka.actor.allow-java-serialization = on
              |akka.actor.warn-about-java-serializer-usage = off
              |
              |akka.remote.artery.enabled = on
              |akka.remote.artery.transport = tcp
              |akka.remote.artery.canonical.hostname = 127.0.0.1
              |akka.remote.artery.advanced.maximum-frame-size = 512 KiB
              |
              |akka.cluster.min-nr-of-members = 3
              |akka.cluster.seed-nodes = [
              |  "akka://simona@127.0.0.1:2551",
              |  "akka://simona@127.0.0.1:2552",
              |  "akka://simona@127.0.0.1:2553"
              |]
              |
              |akka.cluster.jmx.multi-mbeans-in-same-jvm = on
              |
              |akka.cluster.sharding.number-of-shards = 12
              |
              |akka.cluster.sharding.remember-entities = off
              |akka.cluster.sharding.passivate-idle-entity-after = off
              |""".stripMargin
        )
        .withValue(
          "simona.output.base.dir",
          ConfigValueFactory.fromAnyRef(testTmpDir)
        )
        .withFallback(ConfigFactory.parseFile(new File(configFile)))
        .withFallback(ConfigFactory.parseString(s"config=$configFile"))
        .resolve()
  }

  private def runClusteredSimulation(config: Config): Future[Assertion] = {
    /* validate config */
    val simonaConfig = SimonaConfig(config)
    ConfigFailFast.check(simonaConfig)

    val resultFileHierarchy =
      SimonaStandaloneSetup.buildResultFileHierarchy(config)

    val runtimeEventQueue = new LinkedBlockingQueue[RuntimeEvent]()

    val simonaSetup = SimonaStandaloneSetup(
      simonaConfig,
      resultFileHierarchy,
      Some(runtimeEventQueue),
      Array.empty[String]
    )

    val simonaClusterStarter = SimonaClusterStarter(simonaSetup, config)

    /* run simulation */
    // Start cluster sharding
    val oneActorSystem =
      simonaClusterStarter invokePrivate startClusterShardingAllMethod(
        simonaSetup,
        config
      )

    // Defining a Future runs the code right away, which makes it run twice in this context.
    // Using Await also runs the Future.
    // Using a Promise does NOT run the code right away, which is needed here.
    // Apparently there is no way to run define the future without running it, while at
    // the same time blocking until completion.
    val p = Promise[Unit]()

    Cluster(oneActorSystem).registerOnMemberUp {
      akka.pattern.after(1.second, oneActorSystem.scheduler) {
        p.success {
          RunSimonaStandalone.run(simonaSetup, oneActorSystem)(timeout)
        }
        p.future
      }(oneActorSystem.dispatcher)
    }

    p.future.map { _ =>
      checkResults(resultFileHierarchy, runtimeEventQueue)
    }
  }
}
