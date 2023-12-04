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

  /** Calculates the difference between this date time and the provided date
    * time in ticks (= actual seconds)
    */
  extension (zdt: ZonedDateTime)
    def toTick(using startDateTime: ZonedDateTime): Long =
      ChronoUnit.SECONDS.between(startDateTime, zdt)

  /** Calculates the current [[ZonedDateTime]] based on this tick */
  extension (tick: Long)
    def toDateTime(using startDateTime: ZonedDateTime): ZonedDateTime =
      startDateTime.plusSeconds(tick)

    /** Calculates time spam of given time bin resolution */
    def toTimespan: Time =
      Seconds(tick)

    /** Calculate the length for the time interval */
    def durationUntil(
        otherTick: Long,
        tickDuration: Time = Seconds(1d)
    ): Time =
      tickDuration * (otherTick - tick).toDouble

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
      using startDateTime: ZonedDateTime
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
