/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import akka.actor.{ActorRef, Props}
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ApparentPower,
  ApparentPowerAndHeat
}
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.model.participant.EmModel
import edu.ie3.simona.model.participant.EmModel.EmRelevantData

object EmAgent {
  def props(
      scheduler: ActorRef,
      listener: Iterable[ActorRef]
  ): Props =
    Props(
      new EmAgent(
        scheduler,
        listener
      )
    )
}

/** Creating an Energy Management Agent (EmAgent)
  *
  * @param scheduler
  *   Actor reference of the scheduler
  * @param listener
  *   List of listeners interested in results
  */
class EmAgent(
    scheduler: ActorRef,
    override val listener: Iterable[ActorRef]
) extends ParticipantAgent[
      ApparentPowerAndHeat,
      EmRelevantData,
      ParticipantStateData[ApparentPowerAndHeat],
      EmInput,
      EmRuntimeConfig,
      EmModel
    ](
      scheduler
    )
    with EmAgentFundamentals {

  /*
   * "Hey, SIMONA! What is handled in ParticipantAgent?"
   * "Hey, dude! The following things are handled in ParticipantAgent:
   *   1) Initialization of Agent
   *   2) Event reactions in Idle state
   *   3) Handling of incoming information
   *   4) Performing model calculations
   * "
   */
}
