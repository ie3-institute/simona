/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.markov

import edu.ie3.datamodel.models.input.system.MarkovInput
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.markov.MarkovAgentFundamentals.{
  FixedMarkovAgentFundamentals,
  ProfileMarkovAgentFundamentals,
  RandomMarkovAgentFundamentals,
}
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.config.SimonaConfig.MarkovRuntimeConfig
import edu.ie3.simona.model.participant.CalcRelevantData.MarkovRelevantData
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.markov.profile.ProfileMarkovModel
import edu.ie3.simona.model.participant.markov.profile.ProfileMarkovModel.ProfileRelevantData
import edu.ie3.simona.model.participant.markov.random.RandomMarkovModel
import edu.ie3.simona.model.participant.markov.random.RandomMarkovModel.RandomRelevantData
import edu.ie3.simona.model.participant.markov.{
  FixedMarkovModel,
  MarkovModel,
  MarkovModelBehaviour,
}
import org.apache.pekko.actor.{ActorRef, Props}

object MarkovAgent {
  def props(
      scheduler: ActorRef,
      initStateData: ParticipantInitializeStateData[
        MarkovInput,
        MarkovRuntimeConfig,
        ApparentPower,
      ],
      listener: Iterable[ActorRef],
  ): Props =
    MarkovModelBehaviour(initStateData.modelConfig.modelBehaviour) match {
      case MarkovModelBehaviour.FIX =>
        Props(new FixedMarkovAgent(scheduler, initStateData, listener))
      case MarkovModelBehaviour.PROFILE =>
        Props(new ProfileMarkovAgent(scheduler, initStateData, listener))
      case MarkovModelBehaviour.RANDOM =>
        Props(new RandomMarkovAgent(scheduler, initStateData, listener))
      case unsupported =>
        throw new IllegalArgumentException(
          s"The markov agent behaviour '$unsupported' is currently not supported."
        )
    }

  final class FixedMarkovAgent(
      scheduler: ActorRef,
      initStateData: ParticipantInitializeStateData[
        MarkovInput,
        MarkovRuntimeConfig,
        ApparentPower,
      ],
      override val listener: Iterable[ActorRef],
  ) extends MarkovAgent[
        FixedMarkovModel.FixedMarkovRelevantData.type,
        FixedMarkovModel,
      ](scheduler, initStateData, listener)
      with FixedMarkovAgentFundamentals

  final class ProfileMarkovAgent(
      scheduler: ActorRef,
      initStateData: ParticipantInitializeStateData[
        MarkovInput,
        MarkovRuntimeConfig,
        ApparentPower,
      ],
      override val listener: Iterable[ActorRef],
  ) extends MarkovAgent[
        ProfileRelevantData,
        ProfileMarkovModel,
      ](scheduler, initStateData, listener)
      with ProfileMarkovAgentFundamentals

  final class RandomMarkovAgent(
      scheduler: ActorRef,
      initStateData: ParticipantInitializeStateData[
        MarkovInput,
        MarkovRuntimeConfig,
        ApparentPower,
      ],
      override val listener: Iterable[ActorRef],
  ) extends MarkovAgent[
        RandomRelevantData,
        RandomMarkovModel,
      ](scheduler, initStateData, listener)
      with RandomMarkovAgentFundamentals
}

/** Creating a markov agent
  *
  * @param scheduler
  *   Actor reference of the scheduler
  * @param listener
  *   List of listeners interested in results
  */
abstract class MarkovAgent[MD <: MarkovRelevantData, MM <: MarkovModel[MD]](
    scheduler: ActorRef,
    initStateData: ParticipantInitializeStateData[
      MarkovInput,
      MarkovRuntimeConfig,
      ApparentPower,
    ],
    override val listener: Iterable[ActorRef],
) extends ParticipantAgent[
      ApparentPower,
      MD,
      ConstantState.type,
      ParticipantStateData[ApparentPower],
      MarkovInput,
      MarkovRuntimeConfig,
      MM,
    ](scheduler, initStateData)
    with MarkovAgentFundamentals[MD, MM] {
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
