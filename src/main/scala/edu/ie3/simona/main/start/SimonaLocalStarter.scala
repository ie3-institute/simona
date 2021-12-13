/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.main.start

import akka.actor.ActorSystem
import com.typesafe.config.Config

/** Run a local simulation of simona
  */
final case class SimonaLocalStarter(
    protected val config: Config
) extends SimonaStarter {

  override def start(runAndExit: ActorSystem => Unit): Unit = {
    runAndExit(ActorSystem("simona", config))
  }
}
