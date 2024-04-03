/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.load.markov

import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.load.markov.MarkovAgentFundamentals
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.config.SimonaConfig.MarkovRuntimeConfig
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.load.markov.MarkovModel
import edu.ie3.simona.model.participant.load.markov.MarkovData

import org.apache.pekko.actor.{ActorRef, Props}

object MarkovAgent {
  def props(
      scheduler: ActorRef,
      initStateData: ParticipantInitializeStateData[
        MarkovData,
        MarkovRuntimeConfig,
        ApparentPower,
      ],
      listener: Iterable[ActorRef],
  ): Props =
    Props(
        new MarkovAgent
          scheduler,
          initStatedata,
          listener,
        )
    }

/** Creating a load agent
  *
  * @param scheduler
  *   Actor reference of the scheduler
  * @param listener
  *   List of listeners interested in results
  */
class LoadAgent(
    scheduler: ActorRef,
    initStateData: ParticipantInitializeStateData[
      MarkovData,
      MarkovRuntimeConfig,
      ApparentPower,
    ],
    override val listener: Iterable[ActorRef],
) extends ParticipantAgent[
      ApparentPower,
      MarkovRelevantData,
      ConstantState.type,
      ParticipantStateData[ApparentPower],
      MarkovData,
      MarkovRuntimeConfig,
    ](scheduler, initStateData)
    with MarkovAgentFundamentals{
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
