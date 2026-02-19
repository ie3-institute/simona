/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.exceptions.agent

/** Exception that should be used whenever an exception occurs while building
  * participants.
  *
  * @param message
  *   specific error message
  */
class SystemParticipantException(message: String) extends Exception(message)
