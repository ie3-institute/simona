/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import edu.ie3.simona.exceptions.CriticalFailureException

/** Extensions for scala's [[Option]].
  */
object OptionExtension {

  extension [T](opt: Option[T])
    /** Returns the value contained in the [[Option]] or throws a
      * [[CriticalFailureException]] with given message.
      *
      * @param msg
      *   The message of the exception thrown if the option is empty.
      * @return
      *   The value contained in the option.
      */
    def orFail(msg: String): T =
      opt.getOrElse(throw new CriticalFailureException(msg))

}
