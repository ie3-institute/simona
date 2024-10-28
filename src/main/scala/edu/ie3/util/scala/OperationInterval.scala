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
final case class OperationInterval(start: java.lang.Long, end: java.lang.Long)
    extends ClosedInterval[java.lang.Long](start, end) {

  /** Get the first tick, in which the operation starts
    *
    * @return
    *   Tick, in which operation starts
    */
  def getStart: Long = getLower

  /** Get the last tick, in which the operation end
    *
    * @return
    *   Tick, in which operation end
    */
  def getEnd: Long = getUpper
}

object OperationInterval {
  def apply(start: Long, end: Long) = new OperationInterval(start, end)
}
