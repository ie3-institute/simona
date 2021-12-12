/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import akka.actor.{ActorRef, Props}
import edu.ie3.simona.event.RuntimeEvent._
import edu.ie3.simona.event.{Event, RuntimeEvent}
import edu.ie3.simona.logging.SimonaActorLogging
import edu.ie3.simona.util.TickUtil._
import edu.ie3.util.TimeUtil

import java.util.concurrent.BlockingQueue
import scala.reflect.ClassTag

object RuntimeEventListener extends SimonaListenerCompanion {

  override def props[A: ClassTag](
      eventsToProcess: Option[List[String]]
  ): Props =
    Props(new RuntimeEventListener(eventsToProcess))

  def props(
      eventsToProcess: Option[List[String]],
      queue: Option[BlockingQueue[RuntimeEvent]],
      startDateTimeString: String
  ): Props =
    Props(
      new RuntimeEventListener(
        eventsToProcess,
        queue,
        startDateTimeString
      )
    )

}

// receive and selects valid runtime status events to process further
class RuntimeEventListener(
    eventsToProcess: Option[List[String]] = None,
    queue: Option[BlockingQueue[RuntimeEvent]] = None,
    startDateTimeString: String = ""
) extends SimonaListenerWithFilter(eventsToProcess)
    with SimonaActorLogging {

  override protected def processEvent(event: Event, ref: ActorRef): Unit = {
    event match {
      case event: RuntimeEvent =>
        handleEvent(event)

      case _ =>
        log.debug(
          "Skipping event {}, because it is not in the list of events to process.",
          event.id
        )
    }
  }

  // processes each runtime event, adds them to a queue and prints further information
  private def handleEvent(event: RuntimeEvent): Unit = {
    event match {
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
          s"Unexpected run time event received: $event."
        )
    }

    queue.foreach(_.put(event))
  }

  private def calcTime(currentTick: Long): String = {
    TimeUtil.withDefaults.toString(
      currentTick.toDateTime(
        TimeUtil.withDefaults.toZonedDateTime(startDateTimeString)
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
}
