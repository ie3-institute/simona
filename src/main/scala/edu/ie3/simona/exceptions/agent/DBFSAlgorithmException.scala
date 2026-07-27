/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.exceptions.agent

/** Exception that should be used whenever an exception occurs in
  * [[edu.ie3.simona.agent.grid.DBFSAlgorithm]]
  *
  * @param message
  *   specific error message
  */
class DBFSAlgorithmException(message: String) extends Exception(message) {

  def this(message: String, cause: Throwable) = {
    this(message)
    initCause(cause)
  }

}
