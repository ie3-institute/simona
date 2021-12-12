/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

/** Defines several constants that are used as constant parameters in the whole
  * simulation
  *
  * @version 0.1
  * @since 12.01.20
  */
object SimonaConstants {

  @deprecated("This pattern is not used anywhere")
  val DEFAULT_DATE_TIME_PATTERN = "yyyy-MM-dd HH:mm:ss"

  /** Tick, in which the initialization of the simulation shall take place
    */
  val INIT_SIM_TICK: Long = -1L

  /** First tick, at which an actual simulation takes place
    */
  val FIRST_TICK_IN_SIMULATION: Long = 0L

  /** Amount of ticks, that an agent is allowed to be ahead of the slowest agent
    * in population
    */
  val PARALLELISM_WINDOW = 0L

}
