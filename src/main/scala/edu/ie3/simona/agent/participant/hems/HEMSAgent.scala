/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.hems

import akka.actor.{ActorRef, Props}
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
import edu.ie3.simona.config.SimonaConfig.PvRuntimeConfig


object HEMSAgent {
  def props(
             scheduler: ActorRef,
             listener: Iterable[ActorRef]
           ): Props =
    Props(
      new HEMSAgent(
        scheduler,
        listener
      )
    )
  )
}

/** Creating a home energy management agent (HEMSAgent)
  *
  * @param scheduler
  *   Actor reference of the scheduler
  * @param listener
  *   List of listeners interested in results
  */
class HEMSAgent(
               scheduler: ActorRef,
               override val listener: Iterable[ActorRef]
             ) extends ParticipantAgent[
  ApparentPower,
  HEMSRelevantData,
  ParticipantStateData[ApparentPower],
  HEMSInput,
  PvRuntimeConfig,
  HEMSModel
](
  scheduler
)
  with HEMSAgentFundamentals {

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
