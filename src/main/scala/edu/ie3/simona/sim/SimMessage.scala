/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim

import edu.ie3.simona.main.RunSimona.SimonaEnded
import org.apache.pekko.actor.typed.ActorRef

sealed trait SimMessage

object SimMessage {

  /** Starts simulation by activating the next (or first) tick */
  final case class StartSimulation(
      starter: ActorRef[SimonaEnded]
  ) extends SimMessage

  /** Indicate that the simulation has ended successfully */
  case object SimulationEnded extends SimMessage

}
