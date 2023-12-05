/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.main

import org.apache.pekko.actor.ActorSystem

import org.apache.pekko.pattern.ask
import org.apache.pekko.util.Timeout
import edu.ie3.simona.config.{ArgsParser, ConfigFailFast, SimonaConfig}
import edu.ie3.simona.sim.SimMessage.{
  InitSim,
  SimulationFailure,
  SimulationSuccessful,
  StartSimulation
}
import edu.ie3.simona.sim.SimonaSim
import edu.ie3.simona.sim.setup.SimonaStandaloneSetup

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

/** Run a standalone simulation of simona
  *
  * @version 0.1
  * @since 01.07.20
  */
object RunSimonaStandalone extends RunSimona[SimonaStandaloneSetup] {

  override implicit val timeout: Timeout = 7.days

  override def setup(
      args: Array[String]
  ): Seq[SimonaStandaloneSetup] = {
    // get the config and prepare it with the provided args
    val (arguments, parsedConfig) = ArgsParser.prepareConfig(args)

    // config fail fast check
    val simonaConfig = SimonaConfig(parsedConfig)
    ConfigFailFast.check(parsedConfig, simonaConfig)

    Seq(
      SimonaStandaloneSetup(
        parsedConfig,
        SimonaStandaloneSetup.buildResultFileHierarchy(parsedConfig),
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
    val terminated = simonaSim ? InitSim

    Await.result(terminated, timeout.duration) match {
      case SimulationFailure | SimulationSuccessful =>
        Await.ready(shutdownGracefully(simonaSim), timeout.duration)
        Await.ready(actorSystem.terminate(), timeout.duration)

      case unknown =>
        throw new RuntimeException(
          s"Unexpected message from SimonaSim $unknown"
        )
    }

  }

}
