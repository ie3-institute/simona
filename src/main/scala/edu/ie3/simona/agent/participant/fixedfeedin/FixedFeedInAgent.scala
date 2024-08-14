/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.fixedfeedin

import akka.actor.{ActorRef, Props}
import edu.ie3.datamodel.models.input.system.FixedFeedInInput
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
import edu.ie3.simona.config.RuntimeConfig.{LoadRuntimeConfig, SimpleRuntimeConfig}
import edu.ie3.simona.model.participant.CalcRelevantData.FixedRelevantData
import edu.ie3.simona.model.participant.FixedFeedInModel

object FixedFeedInAgent {
  def props(
      scheduler: ActorRef,
      listener: Iterable[ActorRef]
  ): Props =
    Props(new FixedFeedInAgent(scheduler, listener))
}

/** Creating a fixed feed in agent
  *
  * @param scheduler
  *   Actor reference of the scheduler
  * @param listener
  *   List of listeners interested in results
  */
class FixedFeedInAgent(
    scheduler: ActorRef,
    override val listener: Iterable[ActorRef]
) extends ParticipantAgent[
      ApparentPower,
      FixedRelevantData.type,
      ParticipantStateData[ApparentPower],
      FixedFeedInInput,
      SimpleRuntimeConfig,
      FixedFeedInModel
    ](scheduler)
    with FixedFeedInAgentFundamentals {

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
