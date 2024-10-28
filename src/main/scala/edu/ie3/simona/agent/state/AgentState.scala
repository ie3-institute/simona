/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.state

import org.apache.pekko.actor.{ActorRef, FSM}

trait AgentState

/** A general description of agent states that is used by all agents. Agent
  * specific implementation should extend this trait in its own file like e.g.
  * [[GridAgentState]].
  */
object AgentState {

  final case class TerminatedPrematurelyEvent(
      actorRef: ActorRef,
      reason: FSM.Reason,
      tick: Option[Int],
  )

  case object Uninitialized extends AgentState

  case object Idle extends AgentState

}
