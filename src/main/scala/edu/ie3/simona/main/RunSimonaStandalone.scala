/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.main

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import edu.ie3.simona.config.{ArgsParser, ConfigFailFast, ConfigProvider}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  InitSimMessage,
  SimulationFailureMessage,
  SimulationSuccessfulMessage
}
import edu.ie3.simona.sim.SimonaSim
import edu.ie3.simona.sim.setup.SimonaStandaloneSetup

import java.util.concurrent.TimeUnit
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
  ): Seq[() => SimonaStandaloneSetup] = {
    // get the config and prepare it with the provided args
    val (arguments, parsedConfig) = ArgsParser.prepareConfig(args)

    // config fail fast check
    ConfigProvider
      .deriveConfigs(parsedConfig)
      .map { case (simonaCfg, typesafeCfg) =>
        ConfigFailFast.check(typesafeCfg, simonaCfg)
        () =>
          SimonaStandaloneSetup(
            typesafeCfg,
            SimonaStandaloneSetup.buildResultFileHierarchy(typesafeCfg),
            mainArgs = arguments.mainArgs
          )

      }
      .toSeq
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
