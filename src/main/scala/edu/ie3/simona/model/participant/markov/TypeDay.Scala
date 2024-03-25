/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.Markov.profile

import java.time.ZonedDateTime

import edu.ie3.util.TimeUtil

// needs to be imported for max function
import scala.math.Ordering.Double.IeeeOrdering

/** Stores a slice of Markov Househould data, that comprises a whole day (96 quarter
  * hours). The data describes a typical day, that can unequivocally be
  * identified by a [[MarkovHousehouldKey]].
  *
  * @param values
  *   96 quarter-hour values for this Markov Household,
  */
final case class TypeDayProfile(private val values: Array[Double]) {
  if (values.length != 96)
    throw new IllegalArgumentException(
      "You may only instantiate type day parameters with 96 values."
    )

  /** Returns a value for given time. If time is not a 15 min step, it is
    * rounded up to the next 15 min slice.
    *
    * @param time
    *   the time
    * @return
    *   the load value
    */
  def getQuarterHourEnergy(time: ZonedDateTime): Double = {
    val quartH = TimeUtil.withDefaults.getQuarterHourOfDay(time)
    values(quartH)
  }

  /** @return
    *   the maximum value of this MarkovHousehold
    */
  def getMaxValue: Double = values.maxOption.getOrElse(Double.PositiveInfinity)
}
