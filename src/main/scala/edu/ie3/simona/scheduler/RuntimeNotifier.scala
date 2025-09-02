/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import org.apache.pekko.actor.typed.ActorRef
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.RuntimeEvent.*
import edu.ie3.simona.scheduler.RuntimeNotifier.*
import edu.ie3.simona.util.SimonaConstants.{INIT_SIM_TICK, PRE_INIT_TICK}

/** Determines runtime events at different stages of the simulation and notifies
  * listeners
  *
  * @param eventListener
  *   The event listener to send runtime notifications to
  * @param readyCheckWindow
  *   Size of a check window in ticks
  * @param lastCheck
  *   Last check window for which a notification has been issued
  * @param simStartTime
  *   Time in milliseconds when the simulation as a whole was started (before
  *   initialization)
  * @param lastStartTime
  *   Time in milliseconds when the simulation was last started (before
  *   initialization or after a pause)
  * @param lastCheckWindowTime
  *   Time in milliseconds when the last check window was passed
  */
final case class RuntimeNotifier(
    private val eventListener: ActorRef[RuntimeEvent],
    private val readyCheckWindow: Option[Int],
    private val lastCheck: Option[Long] = None,
    private val simStartTime: Option[Long] = None,
    private val lastStartTime: Option[Long] = None,
    private val lastCheckWindowTime: Option[Long] = None,
) {

  /** Notifier listeners that simulation has started or continued with given
    * tick
    * @param tick
    *   Tick that the simulation has started or continued with
    * @param endTick
    *   Last tick of the simulation
    * @return
    *   Updated notifier
    */
  def starting(
      tick: Long,
      endTick: Long,
  ): RuntimeNotifier = {
    val nowTime = now()

    notify(tick match {
      case PRE_INIT_TICK | INIT_SIM_TICK => Initializing
      case _ =>
        Simulating(tick, endTick)
    })

    if simStartTime.nonEmpty then
      // Has been started before: resuming simulation
      copy(lastStartTime = Some(nowTime), lastCheckWindowTime = Some(nowTime))
    else
      copy(
        simStartTime = Some(nowTime),
        lastStartTime = Some(nowTime),
        lastCheckWindowTime = Some(nowTime),
      )
  }

  /** Notifier listeners that simulation has completed the given tick. This
    * usually means issuing CheckWindowPassed messages.
    *
    * @param completedTick
    *   Tick that has just been completed
    * @return
    *   Updated notifier
    */
  def completing(completedTick: Long): RuntimeNotifier = {
    val nowTime = now()

    // first tick (usually INIT_SIM_TICK) has completed
    if lastCheck.isEmpty then notify(InitComplete(duration(simStartTime)))

    // start with 0 if this is the first completed tick
    val adjustedLastCheck = lastCheck.getOrElse(0L)

    readyCheckWindow
      .flatMap { checkWindow =>
        val completedWindows =
          (adjustedLastCheck + checkWindow) to completedTick by checkWindow

        completedWindows.foldLeft(lastCheckWindowTime) {
          case (lastTime, tick) =>
            notify(
              CheckWindowPassed(tick, duration(lastTime, nowTime))
            )
            None
        }

        completedWindows.lastOption
          .map { lastPassedCheck =>
            copy(
              lastCheck = Some(lastPassedCheck),
              lastCheckWindowTime = Some(nowTime),
            )
          }
      }
      .getOrElse {
        // no check window set or no windows passed
        copy(lastCheck = Some(adjustedLastCheck))
      }
  }

  /** Notifier listeners that simulation has ended successfully with given tick
    *
    * @param endTick
    *   Last tick of the simulation
    */
  def finishing(endTick: Long): Unit = {
    notify(
      Done(
        endTick,
        duration(simStartTime),
        errorInSim = false,
      )
    )
  }

  /** Notifier listeners that simulation has ended with error at given tick
    *
    * @param endTick
    *   last tick of the simulation (usually when error happened)
    * @param errorMsg
    *   The error message
    */
  def error(endTick: Long, errorMsg: String): Unit = {
    notify(
      Error(errorMsg)
    )

    notify(
      Done(
        endTick,
        duration(simStartTime),
        errorInSim = true,
      )
    )
  }

  private def notify(event: RuntimeEvent): Unit =
    eventListener ! event

}

object RuntimeNotifier {

  /** Returns the duration from given interval start to end, with a fallback
    * duration of 0 if the start is not (yet) set (which can happen in edge
    * cases, e.g. simulation failed and aborts)
    *
    * @param intervalStart
    *   Optional start of the interval
    * @param intervalEnd
    *   The end of the interval
    * @return
    *   The duration in milliseconds
    */
  private def duration(
      intervalStart: Option[Long],
      intervalEnd: Long = now(),
  ): Long =
    intervalStart.map(intervalEnd - _).getOrElse(0)

  /** @return
    *   Current time in milliseconds
    */
  private def now(): Long = System.nanoTime() / 1000000L

}
