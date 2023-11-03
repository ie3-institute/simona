/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor.typed.ActorRef
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.RuntimeEvent._
import edu.ie3.simona.exceptions.SchedulerException
import edu.ie3.simona.scheduler.RuntimeNotifier.now
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK

/** @param eventListener
  *   The event listener to send runtime notifications to
  * @param readyCheckWindow
  *   Size of a check window in ticks
  * @param lastCheckWindow
  *   Last check window for which a notification has been issued
  * @param simStartTime
  *   Time in milliseconds when the simulation as a whole was started (before
  *   initialization)
  * @param lastStartTime
  *   Time in milliseconds when the simulation was last started (before
  *   initialization or after a pause)
  * @param lastCheckWindowTime
  *   Time when in milliseconds when
  * @param numberOfFailedPf
  *   The number of failed powerflow calculations encountered so far
  */
case class RuntimeNotifier(
    eventListener: ActorRef[RuntimeEvent],
    readyCheckWindow: Long,
    lastCheckWindow: Long = 0L,
    simStartTime: Long = -1L,
    lastStartTime: Long = -1L,
    lastCheckWindowTime: Long = -1L,
    numberOfFailedPf: Int = 0
) {

  def starting(newTick: Long, pauseOrEndTick: Long): RuntimeNotifier = {
    val nowTime = now()

    eventListener ! (newTick match {
      case INIT_SIM_TICK => Initializing
      case _ =>
        Simulating(newTick, pauseOrEndTick)
    })

    if (newTick > 0L)
      copy(lastStartTime = nowTime)
    else
      copy(simStartTime = nowTime, lastStartTime = nowTime)
  }

  def pausing(lastActiveTick: Long): RuntimeNotifier = {
    eventListener ! Ready(lastActiveTick, now() - simStartTime)

    this
  }

  def completing(completedTick: Long): RuntimeNotifier = {
    val nowTime = now()

    val completedWindows =
      (lastCheckWindow + readyCheckWindow) to completedTick by readyCheckWindow

    completedTick match {
      case INIT_SIM_TICK =>
        eventListener ! InitComplete(nowTime - simStartTime)
        this

      case _ if completedWindows.nonEmpty =>
        completedWindows.foreach { tick =>
          eventListener ! CheckWindowPassed(tick, nowTime - lastCheckWindowTime)
        }
        val lastPassedCheckWindow = completedWindows.lastOption.getOrElse(
          throw new SchedulerException(
            "This should not happen, as completedWindows are non-empty"
          )
        )
        copy(
          lastCheckWindow = lastPassedCheckWindow,
          lastCheckWindowTime = nowTime
        )

      case _ =>
        // no check windows passed
        this
    }
  }

  def finishing(lastActiveTick: Long, error: Boolean): Unit = {
    eventListener ! Done(
      lastActiveTick,
      simStartTime - now(),
      numberOfFailedPf,
      error
    )
  }

}

object RuntimeNotifier {

  /** @return
    *   Current time in milliseconds
    */
  private def now(): Long = System.nanoTime() / 1000000L

}
