/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import org.apache.pekko.actor.typed.ActorRef
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.RuntimeEvent.*
import edu.ie3.simona.scheduler.RuntimeNotifier.now
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK

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
  *   Time when in milliseconds when
  */
final case class RuntimeNotifier(
    eventListener: ActorRef[RuntimeEvent],
    readyCheckWindow: Option[Int],
    lastCheck: Long = -1,
    simStartTime: Long = -1,
    lastStartTime: Long = -1,
    lastCheckWindowTime: Long = -1
) {

  /** Notifier listeners that simulation has started or continued with given
    * tick
    * @param tick
    *   Tick that the simulation has started or continued with
    * @param pauseTick
    *   Next tick that the simulation pauses (if applicable)
    * @param endTick
    *   Last tick of the simulation
    * @return
    *   Updated notifier
    */
  def starting(
      tick: Long,
      pauseTick: Option[Long],
      endTick: Long
  ): RuntimeNotifier = {
    val nowTime = now()

    val pauseOrEndTick = pauseTick.map(math.min(_, endTick)).getOrElse(endTick)

    notify(tick match {
      case INIT_SIM_TICK => Initializing
      case _ =>
        Simulating(tick, pauseOrEndTick)
    })

    if tick > 0L then copy(lastStartTime = nowTime)
    else copy(simStartTime = nowTime, lastStartTime = nowTime)
  }

  /** Notifier listeners that simulation is pausing at given tick
    * @param pauseTick
    *   Last tick before simulation pauses or ends
    * @return
    *   Updated notifier
    */
  def pausing(pauseTick: Long): RuntimeNotifier = {
    notify(Ready(pauseTick, now() - simStartTime))

    this
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

    // check if InitComplete should be sent, then adjust lastCheck
    val adjustedLastCheck = if lastCheck <= -1 then {
      if completedTick >= INIT_SIM_TICK then
        notify(InitComplete(nowTime - simStartTime))
      0
    } else lastCheck

    readyCheckWindow
      .flatMap { checkWindow =>
        val completedWindows =
          (adjustedLastCheck + checkWindow) to completedTick by checkWindow

        completedWindows.lastOption
          .map { lastPassedCheck =>
            // at least one window has been passed
            completedWindows.foreach { tick =>
              notify(CheckWindowPassed(tick, nowTime - lastCheckWindowTime))
            }

            copy(
              lastCheck = lastPassedCheck,
              lastCheckWindowTime = nowTime
            )
          }
      }
      .getOrElse {
        // no check window set or no windows passed
        copy(lastCheck = adjustedLastCheck)
      }
  }

  /** Notifier listeners that simulation has ended successfully with given tick
    *
    * @param endTick
    *   Last tick of the simulation
    */
  def finishing(endTick: Long): Unit =
    notify(
      Done(
        endTick,
        simStartTime - now(),
        errorInSim = false
      )
    )

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
        simStartTime - now(),
        errorInSim = true
      )
    )
  }

  private def notify(event: RuntimeEvent): Unit =
    eventListener ! event

}

object RuntimeNotifier {

  /** @return
    *   Current time in milliseconds
    */
  private def now(): Long = System.nanoTime() / 1000000L

}
