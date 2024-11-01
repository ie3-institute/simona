/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala

import edu.ie3.util.interval.ClosedInterval

/** Wrapper class for an operation interval, as the superclass
  * [[ClosedInterval]] only accepts [[java.lang.Long]] as type parameter
  *
  * @param start
  *   Start of operation period (included)
  * @param end
  *   End of operation period (included)
  */
final case class OperationInterval(start: Long, end: Long)
    extends ClosedInterval[java.lang.Long](start, end)
