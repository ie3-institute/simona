/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import breeze.numerics.floor
import edu.ie3.util.TimeUtil
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit

import javax.measure.quantity.Time

/** Provides handy methods to convert ZonedDateTime to ticks and vice versa
  */
object TickUtil {

  /** Provides conversions from ZonedDateTime into ticks (actually seconds) */
  implicit class RichZonedDateTime(private val zdt: ZonedDateTime)
      extends AnyVal {

    /** Calculates the difference between this date time and the provided date
      * time in ticks (= actual seconds)
      */
    def toTick(implicit startDateTime: ZonedDateTime): Long =
      ChronoUnit.SECONDS.between(startDateTime, zdt)

  }

  /** Provides conversions from ticks (seconds) into instances of
    * [[ZonedDateTime]]
    */
  implicit class TickLong(private val tick: Long) extends AnyVal {

    /** Calculates the current [[ZonedDateTime]] based on this tick */
    def toDateTime(implicit startDateTime: ZonedDateTime): ZonedDateTime =
      startDateTime.plusSeconds(tick)

    /** Calculates time spam of given time bin resolution */
    def toTimespan: ComparableQuantity[Time] =
      Quantities.getQuantity(tick, Units.SECOND)

    /** Calculate the length for the time interval */
    def durationUntil(
        otherTick: Long,
        tickDuration: ComparableQuantity[Time] =
          Quantities.getQuantity(1d, Units.SECOND)
    ): ComparableQuantity[Time] =
      tickDuration.multiply(otherTick - tick)

  }

  /** Determine an Array with all ticks between the request frame's start and
    * end according to the data resolution
    *
    * @param frameStart
    *   Beginning of the announced request frame
    * @param frameEnd
    *   End of the announced request frame
    * @param resolution
    *   Data resolution (only applicable for equidistant data points)
    * @return
    *   Array with data ticks
    */
  def getTicksInBetween(frameStart: Long, frameEnd: Long, resolution: Long)(
      implicit startDateTime: ZonedDateTime
  ): Array[Long] = {
    val firstFullHourTick = TimeUtil
      .toNextFull(frameStart.toDateTime, ChronoUnit.HOURS)
      .toTick
    val lastAvailableTick = floor(
      frameEnd.doubleValue / resolution
    ).longValue * resolution
    (firstFullHourTick to lastAvailableTick by resolution.intValue).toArray
  }

}
