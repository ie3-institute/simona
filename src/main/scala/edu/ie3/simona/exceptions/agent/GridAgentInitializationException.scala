/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.exceptions.agent

/** Exception that should be used whenever inconsistency during initialization
  * of a [[edu.ie3.simona.agent.grid.GridAgent]] is detected.
  * @param message
  *   Specific error message
  */
class GridAgentInitializationException(message: String)
    extends AgentInitializationException(message) {

  def this(message: String, cause: Throwable) = {
    this(message)
    initCause(cause)
  }

  def this(cause: Throwable) = {
    this(Option(cause).map(_.toString).orNull, cause)
  }

  def this() = {
    this(null: String)
  }

}
