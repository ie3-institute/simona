/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.main

import edu.ie3.simona.config.{ArgsParser, ConfigFailFast, SimonaConfig}
import edu.ie3.simona.main.RunSimona.*
import edu.ie3.simona.sim.SimonaSim
import edu.ie3.simona.sim.setup.SimonaSetup
import org.apache.pekko.actor.typed.scaladsl.AskPattern.*
import org.apache.pekko.actor.typed.{ActorSystem, Scheduler}
import org.apache.pekko.util.Timeout

import scala.concurrent.{Await, TimeoutException}

/** Run a standalone simulation of simona
  *
  * @since 01.07.20
  */
object RunSimonaStandalone extends RunSimona[SimonaSetup] {

  override def setup(args: Array[String]): SimonaSetup = {
    // get the config and prepare it with the provided args
    val (arguments, parsedConfig) = ArgsParser.prepareConfig(args)

    // config fail fast check
    val simonaConfig = SimonaConfig(parsedConfig)
    ConfigFailFast.check(parsedConfig, simonaConfig)

    SimonaSetup(
      parsedConfig,
      simonaConfig,
      arguments.mainArgs,
    )
  }

  override def run(simonaSetup: SimonaSetup): Boolean = {
    val simonaSim = ActorSystem(
      SimonaSim(simonaSetup),
      name = "Simona",
      config = simonaSetup.typeSafeConfig,
    )

    given scheduler: Scheduler = simonaSim.scheduler
    given timeout: Timeout = simonaSetup.simonaConfig.simulationTimeout

    try {
      // run the simulation
      val terminated = simonaSim.ask[SimonaEnded](ref => SimonaSim.Start(ref))

      Await.result(terminated, timeout.duration) match {
        case SimonaEnded(successful) =>
          simonaSim.terminate()

          successful
      }
    } catch {
      case te: TimeoutException =>
        simonaSim.terminate()
        logger.error(
          s"Simulation timeout reached! Stopping the simulation.",
          te,
        )
        false
    }

  }

}
