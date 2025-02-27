/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load.random

import edu.ie3.simona.model.participant2.load.DayType
import edu.ie3.util.TimeUtil

import java.time.ZonedDateTime

/** Stores a slice of random load parameters, that comprises a whole day (96
  * quarter-hours). The data describes a typical day, that can unequivocally be
  * identified by its [[DayType]].
  *
  * @param values
  *   96 quarter-hour values for this day type
  */
final case class TypeDayParameters(
    private val values: Array[RandomLoadParameters]
) {
  if (values.length != 96)
    throw new IllegalArgumentException(
      s"You may only instantiate type day parameters with 96 values. Apparent: ${values.length}."
    )

  /** Returns a value for given time. If time is not a 15-min step, it is
    * rounded up to the next 15-min slice.
    *
    * @param time
    *   the time
    * @return
    *   the random load parameters
    */
  def getQuarterHourParameters(time: ZonedDateTime): RandomLoadParameters = {
    val quartHour = TimeUtil.withDefaults.getQuarterHourOfDay(time)
    values(quartHour)
  }
}
