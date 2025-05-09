/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event

import edu.ie3.simona.event.listener.RuntimeEventListener.Request

/** Event type for simulation control */
sealed trait RuntimeEvent extends Event with Request

object RuntimeEvent {

  /** Indicates that all required agents and services are currently being
    * initialized for simulation
    */
  final case object Initializing extends RuntimeEvent

  /** Indicates that the initialization process of all agents and actors is
    * finished
    *
    * @param duration
    *   duration needed for the initialization process in milliseconds
    */
  final case class InitComplete(duration: Long) extends RuntimeEvent

  /** A status indication that the
    * [[edu.ie3.simona.config.SimonaConfig.simona.time.schedulerReadyCheckWindow]]
    * has been passed. Normally used by the
    * [[edu.ie3.simona.event.listener.RuntimeEventListener]] to print status
    * information about the current simulation run.
    *
    * @param tick
    *   the tick of the simulation that has been passed
    * @param duration
    *   the duration that has been taken since the last time a
    *   [[CheckWindowPassed]] event has been issued in milliseconds
    */
  final case class CheckWindowPassed(tick: Long, duration: Long)
      extends RuntimeEvent

  /** Indicates that the scheduler is currently executing a simulation within
    * the provided start and end tick
    *
    * @param startTick
    *   the starting tick of the simulation
    * @param endTick
    *   the tick the simulation will stop
    */
  final case class Simulating(startTick: Long, endTick: Long)
      extends RuntimeEvent

  /** Indicates that the current simulation over entire time horizon is
    * completed
    * @param tick
    *   the tick when the event is issued
    * @param duration
    *   the duration of the overall simulation in milliseconds
    * @param errorInSim
    *   true if an error occurred in the simulation, false otherwise
    */
  final case class Done(
      tick: Long,
      duration: Long,
      errorInSim: Boolean,
  ) extends RuntimeEvent

  /** Indicates that a power flow calculation has failed. This event is not
    * forwarded to sinks, but rather counted in runtime statistics.
    */
  final case object PowerFlowFailed extends RuntimeEvent

  /** Indicates that an error occurred during the simulation, thereby preventing
    * continuation
    */
  final case class Error(errMsg: String) extends RuntimeEvent

}
