/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.main

import edu.ie3.simona.config.{ArgsParser, ConfigFailFast, SimonaConfig}
import edu.ie3.simona.main.RunSimona._
import edu.ie3.simona.sim.SimonaSim
import edu.ie3.simona.sim.setup.SimonaStandaloneSetup
import org.apache.pekko.actor.typed.scaladsl.AskPattern._
import org.apache.pekko.actor.typed.{ActorSystem, Scheduler}
import org.apache.pekko.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

/** Run a standalone simulation of simona
  *
  * @since 01.07.20
  */
object RunSimonaStandalone extends RunSimona[SimonaStandaloneSetup] {

  override implicit val timeout: Timeout = Timeout(12.hours)

  override def setup(args: Array[String]): SimonaStandaloneSetup = {
    // get the config and prepare it with the provided args
    val (arguments, parsedConfig) = ArgsParser.prepareConfig(args)

    // config fail fast check
    val simonaConfig = SimonaConfig(parsedConfig)
    ConfigFailFast.check(parsedConfig, simonaConfig)

    SimonaStandaloneSetup(
      parsedConfig,
      SimonaStandaloneSetup.buildResultFileHierarchy(parsedConfig),
      mainArgs = arguments.mainArgs,
    )
  }

  override def run(simonaSetup: SimonaStandaloneSetup): Boolean = {
    val simonaSim = ActorSystem(
      SimonaSim(simonaSetup),
      name = "Simona",
      config = simonaSetup.typeSafeConfig,
    )

    implicit val scheduler: Scheduler = simonaSim.scheduler

    // run the simulation
    val terminated = simonaSim.ask[SimonaEnded](ref => SimonaSim.Start(ref))

    Await.result(terminated, timeout.duration) match {
      case SimonaEnded(successful) =>
        simonaSim.terminate()

        successful
    }
  }

}
