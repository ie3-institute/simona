/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service

import edu.ie3.datamodel.exceptions.SourceException
import edu.ie3.datamodel.io.source.TimeSeriesSource
import edu.ie3.simona.util.TickUtil.toTick
import edu.ie3.util.scala.collection.immutable.ActivationTickQueue
import org.slf4j.Logger

import java.time.ZonedDateTime
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try}

object TimeSeriesUtil {

  def getTicksAdaptedToSimulation(
      source: TimeSeriesSource[?],
      simulationStart: ZonedDateTime,
  )(using
      log: Logger
  ): Try[ActivationTickQueue] = {
    given startDateTime: ZonedDateTime = simulationStart

    // Note: because we want data for the start tick as well, we need to use any tick before the start tick
    val intervalStart = simulationStart.minusSeconds(1)

    val ticks =
      source
        .getTimeKeysAfter(intervalStart)
        .asScala
        .toSeq
        .map(_.toTick)

    val maybeFirstTickBeforeStart =
      source.getLastTimeKeyBefore(simulationStart).toScala

    (ticks.headOption, maybeFirstTickBeforeStart) match {
      case (Some(tick), _) if tick == 0L =>
        /* Set up the state data and determine the next activation tick. */

        Success(ActivationTickQueue(ticks))

      case (Some(tick), None) if tick > 0L =>
        /* No data for the first tick or before, but the start of the data needs to be at the first tick of the simulation. */
        Failure(
          new SourceException(
            s"The data for the timeseries '${source.getTimeSeries.getUuid}' starts after the start of this simulation (tick: $tick)! This is not allowed!"
          )
        )

      case (_, Some(value)) =>
        if ticks.nonEmpty then
          /* We have data before and after the start of the simulation, but not at tick 0 */
          log.debug(
            s"No data at the start of the simulation. Use last know data for tick: ${value.toTick}"
          )
        else
          /* We have data before, but not after the start of the simulation */
          log.warn(
            s"Only found data before the start of the simulation. Tick: ${value.toTick}"
          )

        val startTick = 0L

        Success(ActivationTickQueue(ticks.prepended(startTick)))

      case _ =>
        /* No data for the simulation. */
        Failure(
          new SourceException(
            s"No appropriate data found within simulation time range in timeseries '${source.getTimeSeries.getUuid}'!"
          )
        )
    }
  }

}
