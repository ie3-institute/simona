/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.main

import akka.actor.ActorSystem

import java.util.concurrent.TimeUnit
import akka.pattern.ask
import akka.util.Timeout
import edu.ie3.simona.config.{ArgsParser, ConfigFailFast, SimonaConfig}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  InitSimMessage,
  SimulationFailureMessage,
  SimulationSuccessfulMessage
}
import edu.ie3.simona.sim.SimonaSim
import edu.ie3.simona.sim.setup.SimonaStandaloneSetup

import java.nio.file.{Paths}
import scala.concurrent.Await

/** Run a standalone simulation of simona
  *
  * @version 0.1
  * @since 01.07.20
  */
object RunSimonaStandalone extends RunSimona[SimonaStandaloneSetup] {

  override implicit val timeout: Timeout = Timeout(50000, TimeUnit.SECONDS)

  override def setup(
      args: Array[String]
  ): Seq[SimonaStandaloneSetup] = {

    // Note: We parse the config as tscfg separately, as it includes the akka configuration,
    // which is passed to the actor system
    val (arguments, tscfg) = ArgsParser.prepareConfig(args)
    val cfgPath = Paths.get(
      arguments.configLocation.getOrElse(
        throw new RuntimeException(
          "Please provide a valid config file via --config <path-to-config-file>."
        )
      )
    )
    val simonaConfig = SimonaConfig(cfgPath)
    ConfigFailFast.check(tscfg, simonaConfig)

    Seq(
      SimonaStandaloneSetup(
        simonaConfig,
        tscfg,
        SimonaStandaloneSetup.buildResultFileHierarchy(tscfg, simonaConfig),
        mainArgs = arguments.mainArgs
      )
    )
  }

  override def run(
      simonaSetup: SimonaStandaloneSetup
  ): Unit = {
    val actorSystem: ActorSystem = simonaSetup.buildActorSystem.apply()
    // build the simulation container actor
    val simonaSim = actorSystem.actorOf(
      SimonaSim.props(simonaSetup)
    )

    // run the simulation
    val terminated = simonaSim ? InitSimMessage

    Await.result(terminated, timeout.duration) match {
      case SimulationFailureMessage | SimulationSuccessfulMessage =>
        Await.ready(shutdownGracefully(simonaSim), timeout.duration)
        Await.ready(actorSystem.terminate(), timeout.duration)

      case unknown =>
        throw new RuntimeException(
          s"Unexpected message from SimonaSim $unknown"
        )
    }

  }

}
