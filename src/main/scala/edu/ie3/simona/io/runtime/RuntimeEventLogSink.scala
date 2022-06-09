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

import java.time.ZonedDateTime

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
          s"Initialization complete. (duration: ${roundDuration(duration)} s)"
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
          s"******* Simulation completed with $simStatus in time step ${calcTime(currentTick)}. Total runtime: ${roundDuration(duration)} s *******"
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

  private def roundDuration(duration: Double): Double = {
    roundAt(5)(duration / 1000)
  }

  private def roundAt(precision: Int)(number: Double): Double = {
    val s = math pow (10, precision)
    (math round number * s) / s
  }

  private def durationAndMemoryString(duration: Double) = {
    val memory = math.round(
      10 * (Runtime.getRuntime.totalMemory() - Runtime.getRuntime
        .freeMemory()) / Math
        .pow(1000, 3)
    ) / 10.0

    s"(duration: ${roundDuration(duration)} s, memory: $memory GB)"
  }

  /** Contains all cleanup operations before closing this sink. Should be
    * blocking to ensure that everything inside a buffer or similar is written
    * out.
    */
  override def close(): Unit = {}
}
