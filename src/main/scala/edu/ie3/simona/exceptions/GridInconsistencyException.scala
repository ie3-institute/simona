/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.exceptions

/** Exception that should be used whenever an exception occurs in
  * [[edu.ie3.simona.util.ResultFileHierarchy]]
  * @param message
  *   specific error message
  */
class GridInconsistencyException(message: String) extends Exception(message) {

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
