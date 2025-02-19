/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import breeze.numerics.floor
import edu.ie3.util.TimeUtil
import squants.Time
import squants.time.Seconds

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit

/** Provides handy methods to convert ZonedDateTime to ticks and vice versa
  */
object TickUtil {

  /** Provides conversions from ZonedDateTime into ticks (actually seconds) */
  implicit class RichZonedDateTime(private val zdt: ZonedDateTime) {

    /** Calculates the difference between this date time and the provided date
      * time in ticks (= actual seconds)
      */
    def toTick(implicit startDateTime: ZonedDateTime): Long =
      ChronoUnit.SECONDS.between(startDateTime, zdt)

  }

  /** Provides conversions from ticks (seconds) into instances of
    * [[ZonedDateTime]]
    */
  implicit class TickLong(private val tick: Long) {

    /** Calculates the current [[ZonedDateTime]] based on this tick */
    def toDateTime(implicit startDateTime: ZonedDateTime): ZonedDateTime =
      startDateTime.plusSeconds(tick)

    /** Calculates time spam of given time bin resolution */
    def toTimespan: Time =
      Seconds(tick)

    /** Calculate the length for the time interval */
    def durationUntil(
        otherTick: Long,
        tickDuration: Time = Seconds(1d),
    ): Time =
      tickDuration * (otherTick - tick).toDouble

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

  /** Rounds given tick and datetime with regard to their (implicit) minutes and
    * seconds.
    *
    * @param tick
    *   The given tick to round
    * @param dateTime
    *   The given date and time to round
    * @param resolution
    *   Resolution in seconds. Should divide 3600 without remainder, i.e.
    *   {{{3600 % resolution == 0}}}
    */
  def roundToResolution(
      tick: Long,
      dateTime: ZonedDateTime,
      resolution: Int,
  ): (Long, ZonedDateTime) = {

    val givenHourSeconds = dateTime.getMinute * 60 + dateTime.getSecond

    val adaptedHourSeconds = givenHourSeconds / resolution * resolution
    val adaptedMinute = adaptedHourSeconds / 60
    val adaptedSecond = adaptedHourSeconds % 60

    val adaptedDateTime =
      dateTime.withMinute(adaptedMinute).withSecond(adaptedSecond).withNano(0)

    val tickDifference = givenHourSeconds - adaptedHourSeconds

    val adaptedTick = tick - tickDifference

    (adaptedTick, adaptedDateTime)
  }
}
