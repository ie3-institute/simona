/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent

import org.apache.pekko.actor.FSM.CurrentState
import org.apache.pekko.actor.{LoggingFSM, PoisonPill, Stash, Status}
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.event.notifier.Notifier
import edu.ie3.simona.logging.SimonaFSMActorLogging

import scala.language.postfixOps

/** Trait that is mixed into every agent providing methods and services common
  * to all or most of all agents in the SIMONA simulation environment.
  *
  * @tparam D
  *   FSM state data
  */
trait SimonaAgent[D]
    extends Notifier
    with LoggingFSM[AgentState, D]
    with Stash
    with SimonaFSMActorLogging {

  private var _currentTick: Option[Long] = None

  onTransition { case oldState -> newState =>
    log.debug("{} -> {}", oldState, newState)
  }

  def myUnhandled(): StateFunction = {

    case Event(Status.Failure(ex), _) =>
      log.error(
        ex,
        "Received a failure status message with following exception.",
      )
      self ! PoisonPill
      stay()

    case Event(CurrentState, _) =>
      goto(stateName) replying stateName

    case event =>
      log.error(s"Unhandled event '$event' from '${sender()}''")
      stay()
  }

  def holdTick(tick: Long): Unit = {
    if (_currentTick.isDefined)
      throw new IllegalStateException(
        s"Expected both _currentTick to be 'None' but found ${_currentTick} instead, respectively."
      )
    _currentTick = Some(tick)
  }

  def releaseTick(): Long = {
    val currentTick = _currentTick.getOrElse(
      throw new RuntimeException(
        "Tried to access currentTick and currentTriggerId without having them set before!"
      )
    )
    _currentTick = None
    currentTick
  }

  def currentTick: Long = {
    _currentTick.getOrElse(
      throw new RuntimeException(
        s"$actorName: CurrentTick has been requested, but is not set!"
      )
    )
  }

  def currentTickDefined: Boolean = _currentTick.isDefined

}
