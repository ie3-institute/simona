/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.exceptions

/** Exception that is thrown if an expected result is not present.
  * @param message
  *   exception message
  */
class ResultException(message: String) extends Exception(message) {}
