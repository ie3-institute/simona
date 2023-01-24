/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.runtime
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.RuntimeEvent._
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.TimeUtil
import org.slf4j.Logger

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId, ZonedDateTime}

/** Runtime event sink that just logs all received events.
  * @param simulationStartDate
  *   the simulation start date time, used for calculating simulation time from
  *   ticks
  */
final case class RuntimeEventLogSink(
    simulationStartDate: ZonedDateTime
) extends RuntimeEventSink {

  override def handleRuntimeEvent(
      runtimeEvent: RuntimeEvent,
      log: Logger
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

      case Done(currentTick, duration, noOfFailedPF, errorInSim) =>
        val simStatus =
          if (errorInSim)
            s"\u001b[0;31mERROR (Failed PF: $noOfFailedPF)\u001b[0;30m"
          else s"\u001b[0;32mSUCCESS (Failed PF: $noOfFailedPF)\u001b[0;30m"
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

  def convertDuration(duration: Long): String = {
    val hh = duration / 1000 / 3600
    val mm = (duration / 1000 / 60) % 60
    val ss = (duration / 1000) % 60
    s"${hh}h : ${mm}m : ${ss}s"
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
