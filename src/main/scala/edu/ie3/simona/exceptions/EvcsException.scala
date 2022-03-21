/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.exceptions

sealed trait EvcsException extends Exception

object EvcsException {
  final case class SchedulingException(
      msg: String = "",
      cause: Throwable = None.orNull
  ) extends Exception(msg, cause)
      with EvcsException
}
