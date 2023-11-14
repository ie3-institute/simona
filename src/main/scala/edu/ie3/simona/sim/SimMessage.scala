/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim

trait SimMessage
object SimMessage {

  /** Starts simulation by activating the next (or first) tick
    *
    * @param pauseTick
    *   Last tick that can be activated or completed before the simulation is
    *   paused
    */
  final case class StartSimulation(
      pauseTick: Option[Long] = None
  ) extends SimMessage

  /** Reported back from the scheduler if an error occurred during the
    * simulation
    */
  case object SimulationFailureMessage extends SimMessage

  /** Reported back from the scheduler if the simulation terminated as expected
    */
  case object SimulationSuccessfulMessage extends SimMessage

}
