/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.state

/** Current implementation of the state of a
  * [[edu.ie3.simona.agent.participant.ParticipantAgent]].
  */
sealed trait ParticipantAgentState extends AgentState

object ParticipantAgentState {

  /** State in which a [[edu.ie3.simona.agent.participant.ParticipantAgent]]
    * receives data
    */
  case object HandleInformation extends ParticipantAgentState

  /** State in which a [[edu.ie3.simona.agent.participant.ParticipantAgent]]
    * does its calculations
    */
  case object Calculate extends ParticipantAgentState

}
