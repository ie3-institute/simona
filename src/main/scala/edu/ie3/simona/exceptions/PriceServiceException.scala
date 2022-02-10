/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.exceptions

abstract class PriceServiceException(msg: String, cause: Throwable)
    extends Exception

object PriceServiceException {
  final case class PriceNotFoundException(
      msg: String = "",
      cause: Throwable = None.orNull
  ) extends PriceServiceException(msg, cause)
}
