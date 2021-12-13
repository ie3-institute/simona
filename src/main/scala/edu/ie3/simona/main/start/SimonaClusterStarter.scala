/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.main.start

import akka.actor.{ActorSystem, AddressFromURIString}
import akka.cluster.Cluster
import com.typesafe.config.{Config, ConfigFactory}
import edu.ie3.simona.cluster.SimonaSharding
import edu.ie3.simona.config.SimonaConfig.ComputationMode._
import edu.ie3.simona.sim.setup.SimonaStandaloneSetup

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.CollectionHasAsScala

/** Run a cluster simulation of simona
  */
final case class SimonaClusterStarter(
    protected val simonaSetup: SimonaStandaloneSetup,
    protected val config: Config
) extends SimonaStarter {

  override def start(runAndExit: ActorSystem => Unit): Unit = {
    simonaSetup.simonaConfig.simona.execution.computationMode match {
      case ClusterSingleJVM =>
        val firstSystem = startClusterShardingAll(simonaSetup, config)
        startOnUp(firstSystem, runAndExit)
      case ClusterStartWorker =>
        val mainSystem = startClusterSharding(simonaSetup, config)
        startOnUp(mainSystem, runAndExit)
      case ClusterWorker =>
        startClusterSharding(simonaSetup, config)
      case Local =>
        throw new IllegalArgumentException(
          s"Wrong ComputationMode 'Local' when starting clustered simulation"
        )
    }
  }

  private def startOnUp(
      system: ActorSystem,
      runAndExit: ActorSystem => Unit
  ): Unit =
    // when node is up (all nodes go up at the same time), start simulation
    Cluster(system).registerOnMemberUp {
      // wait a bit for Singletons to sort out etc.
      akka.pattern.after(1.seconds, system.scheduler) {
        Future {
          runAndExit(system)
        }(system.dispatcher)
      }(system.dispatcher)
    }

  /** Start all cluster nodes according to seed nodes in this JVM
    *
    * @param simonaSetup
    *   the simona setup
    * @param config
    *   the typesafe config
    * @return
    *   the ActorSystem of the first node
    */
  private def startClusterShardingAll(
      simonaSetup: SimonaStandaloneSetup,
      config: Config
  ): ActorSystem = {
    // pre-creating an ActorSystem, we do not have access to ActorSystem.Settings
    val seedNodePorts =
      config.getStringList("akka.cluster.seed-nodes").asScala.flatMap {
        case AddressFromURIString(s) => s.port
      }

    seedNodePorts
      .foldLeft[Option[ActorSystem]](None) { (mainSystem, port) =>
        val nodeConfig = configWithPort(config, port)

        val actorSystem = startClusterSharding(simonaSetup, nodeConfig)

        if (mainSystem.isEmpty)
          Some(actorSystem)
        else
          mainSystem
      }
      .getOrElse(throw new RuntimeException("No node was started"))
  }

  private def configWithPort(config: Config, port: Int): Config =
    ConfigFactory
      .parseString(s"""
       akka.remote.artery.canonical.port = $port
       akka.management.http.port = ${port + 10000}
        """)
      .withFallback(config)

  /** Start a cluster node with given config
    *
    * @param simonaSetup
    *   the simona setup
    * @param nodeConfig
    *   the typesafe config for this node
    * @return
    *   the ActorSystem of the node that was started
    */
  private def startClusterSharding(
      simonaSetup: SimonaStandaloneSetup,
      nodeConfig: Config
  ): ActorSystem = {
    val actorSystemName = "simona"

    val actorSystem = ActorSystem(actorSystemName, nodeConfig)

    Cluster(actorSystem).registerOnMemberUp {
      SimonaSharding.startGridSharding(
        actorSystem,
        simonaSetup
      )
    }

    Cluster(actorSystem).registerOnMemberRemoved {
      // when node is removed (e.g. after simulation ends), terminate
      actorSystem.terminate()
    }

    actorSystem
  }
}
