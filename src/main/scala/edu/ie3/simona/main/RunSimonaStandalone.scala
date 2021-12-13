/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.main

import akka.actor.ActorSystem
import akka.cluster.MemberStatus.{Exiting, Leaving, Removed}
import akka.cluster.{Cluster, MemberStatus}
import akka.pattern.gracefulStop
import akka.util.Timeout
import akka.util.Timeout.durationToTimeout
import com.typesafe.config.ConfigFactory
import edu.ie3.simona.akka.SimonaActorRef
import edu.ie3.simona.akka.SimonaActorRef.{LocalActorRef, RichActorRefFactory}
import edu.ie3.simona.config.SimonaConfig.ComputationMode
import edu.ie3.simona.config.{ArgsParser, ConfigFailFast, SimonaConfig}
import edu.ie3.simona.main.start.{
  SimonaClusterStarter,
  SimonaLocalStarter,
  SimonaStarter
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  InitSimMessage,
  SimulationFailureMessage,
  SimulationSuccessfulMessage
}
import edu.ie3.simona.sim.SimonaSim
import edu.ie3.simona.sim.setup.SimonaStandaloneSetup

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

/** Run a standalone simulation of simona
  *
  * @since 01.07.20
  */
object RunSimonaStandalone extends RunSimona[SimonaStandaloneSetup] {

  override protected implicit val timeout: Timeout = 12.hours

  override protected def setup(
      args: Array[String]
  ): Seq[(SimonaStandaloneSetup, SimonaStarter)] = {
    val config = ArgsParser.prepareConfig(args)._2

    // config fail fast check
    val simonaConfig = SimonaConfig(config)
    ConfigFailFast.check(simonaConfig)

    val isLocal =
      simonaConfig.simona.execution.computationMode == ComputationMode.Local

    val provider =
      if (isLocal) "local" else "cluster"
    // get the config and prepare it with the provided args
    val configWithAkka = ConfigFactory
      .parseString(s"""
       akka.actor.provider = $provider
        """)
      .withFallback(
        config
      )

    val simonaSetup = SimonaStandaloneSetup(
      simonaConfig,
      SimonaStandaloneSetup.buildResultFileHierarchy(config),
      args = args
    )

    val starter =
      if (isLocal)
        SimonaLocalStarter(configWithAkka)
      else
        SimonaClusterStarter(simonaSetup, configWithAkka)

    Seq(
      (simonaSetup, starter)
    )
  }

  override def run(
      simonaSetup: SimonaStandaloneSetup,
      actorSystem: ActorSystem
  )(implicit
      timeout: Timeout
  ): Unit = {

    // build the simulation container actor
    val simonaSim = actorSystem.createSingletonOf(
      SimonaSim.props(simonaSetup)
    )

    implicit val system: ActorSystem = actorSystem
    // run the simulation
    val terminated = simonaSim ? InitSimMessage

    Await.result(terminated, timeout.duration) match {
      case SimulationFailureMessage | SimulationSuccessfulMessage =>
        shutdown(simonaSim)
      case unknown =>
        throw new RuntimeException(
          s"Unexpected message from SimonaSim $unknown"
        )
    }

    // includes graceful shutdown by means of CoordinatedShutdown
    Await.ready(actorSystem.terminate(), 1.minute)
  }

  private def shutdown(
      simonaSim: SimonaActorRef
  )(implicit system: ActorSystem): Unit = {
    simonaSim match {
      case LocalActorRef(actorRef) =>
        val timeout = 1.minute
        Await.ready(
          gracefulStop(actorRef, timeout),
          timeout.duration
        )
      case _ =>
        Cluster(system).state.members
          .filter(member =>
            !(Set[MemberStatus](
              Leaving,
              Exiting,
              Removed
            ) contains member.status) &&
              member.uniqueAddress.address != Cluster(system).selfAddress
          )
          .foreach(member => Cluster(system).leave(member.address))
    }
  }

}
