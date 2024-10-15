/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.runtime

import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.RuntimeEvent._
import edu.ie3.simona.io.runtime.RuntimeEventSink.RuntimeStats
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.TimeUtil
import org.slf4j.Logger

import java.time.ZonedDateTime
import scala.concurrent.duration.DurationLong

/** Runtime event sink that just logs all received events.
  *
  * @param simulationStartDate
  *   the simulation start date time, used for calculating simulation time from
  *   ticks
  * @param log
  *   The logger to use
  */
final case class RuntimeEventLogSink(
    simulationStartDate: ZonedDateTime,
    log: Logger,
) extends RuntimeEventSink {

  override def handleRuntimeEvent(
      runtimeEvent: RuntimeEvent,
      runtimeStats: RuntimeStats,
  ): Unit =
    runtimeEvent match {
      case Initializing =>
        log.info("Initializing Agents and Services for Simulation ... ")

      case InitComplete(duration) =>
        log.info(
          s"Initialization complete. (duration: ${convertDuration(duration)} )"
        )

      case CheckWindowPassed(tick, duration) =>
        log.info(
          s"******* Simulation until ${calcTime(tick)} completed. ${durationAndMemoryString(duration)} ******"
        )

      case Ready(tick, duration) =>
        log.info(
          s"******* Switched from 'Simulating' to 'Ready'. Last simulated time: ${calcTime(tick)}. ${durationAndMemoryString(duration)}  ******"
        )

      case Simulating(startTick, endTick) =>
        log.info(
          s"******* Simulating from ${calcTime(startTick)} until ${calcTime(endTick)}. *******"
        )

      case Done(currentTick, duration, errorInSim) =>
        val simStatus =
          if (errorInSim)
            s"\u001b[0;31mERROR (Failed PF: ${runtimeStats.failedPowerFlows})\u001b[0;0m"
          else
            s"\u001b[0;32mSUCCESS (Failed PF: ${runtimeStats.failedPowerFlows})\u001b[0;0m"
        log.info(
          s"******* Simulation completed with $simStatus in time step ${calcTime(currentTick)}. Total runtime: ${convertDuration(duration)} *******"
        )

      case Error(errMsg) =>
        log.error(s"$errMsg")

      case _ =>
        log.error(
          s"Unexpected run time event received: $runtimeEvent."
        )
    }

  private def calcTime(currentTick: Long): String = {
    TimeUtil.withDefaults.toString(
      currentTick.toDateTime(
        simulationStartDate
      )
    )
  }

  private def convertDuration(duration: Long): String = {
    val time = duration.milliseconds

    val hours = time.toHours
    val minutes = time.toMinutes % 60
    val seconds = time.toSeconds % 60
    val milliseconds =
      (time - hours.hours - minutes.minutes - seconds.seconds).toMillis
    s"${hours}h : ${minutes}m : ${seconds}s : ${milliseconds}ms"
  }

  private def durationAndMemoryString(duration: Long) = {
    val memory = math.round(
      10 * (Runtime.getRuntime.totalMemory() - Runtime.getRuntime
        .freeMemory()) / Math
        .pow(1000, 3)
    ) / 10.0

    s"(duration: ${convertDuration(duration)} , memory: $memory GB)"
  }

  override def close(): Unit = {
    // nothing to close
  }
}
