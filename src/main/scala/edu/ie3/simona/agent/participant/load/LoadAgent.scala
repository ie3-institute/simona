/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.load

import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPower
import edu.ie3.simona.agent.participant.load.LoadAgentFundamentals.{
  FixedLoadAgentFundamentals,
  ProfileLoadAgentFundamentals,
  RandomLoadAgentFundamentals,
}
import edu.ie3.simona.agent.participant.load.markov.MarkovAgent
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.config.SimonaConfig.LoadRuntimeConfig
import edu.ie3.simona.model.participant.CalcRelevantData.LoadRelevantData
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.load.profile.ProfileLoadModel
import edu.ie3.simona.model.participant.load.profile.ProfileLoadModel.ProfileRelevantData
import edu.ie3.simona.model.participant.load.random.RandomLoadModel
import edu.ie3.simona.model.participant.load.random.RandomLoadModel.RandomRelevantData
import edu.ie3.simona.model.participant.load.{
  FixedLoadModel,
  LoadModel,
  LoadModelBehaviour,
}
import org.apache.pekko.actor.{ActorRef, Props}

object LoadAgent {
  def props(
      scheduler: ActorRef,
      initStateData: ParticipantInitializeStateData[
        LoadInput,
        LoadRuntimeConfig,
        ComplexPower,
      ],
      listener: Iterable[ActorRef],
  ): Props =
    LoadModelBehaviour(initStateData.modelConfig.modelBehaviour) match {
      case LoadModelBehaviour.FIX =>
        Props(new FixedLoadAgent(scheduler, initStateData, listener))
      case LoadModelBehaviour.PROFILE =>
        Props(new ProfileLoadAgent(scheduler, initStateData, listener))
      case LoadModelBehaviour.RANDOM =>
        Props(new RandomLoadAgent(scheduler, initStateData, listener))
      case LoadModelBehaviour.MARKOV =>
        Props(new MarkovAgent(scheduler, initStateData, listener))
      case unsupported =>
        throw new IllegalArgumentException(
          s"The load agent behaviour '$unsupported' is currently not supported."
        )
    }

  final class FixedLoadAgent(
      scheduler: ActorRef,
      initStateData: ParticipantInitializeStateData[
        LoadInput,
        LoadRuntimeConfig,
        ComplexPower,
      ],
      override val listener: Iterable[ActorRef],
  ) extends LoadAgent[
        FixedLoadModel.FixedLoadRelevantData.type,
        FixedLoadModel,
      ](scheduler, initStateData, listener)
      with FixedLoadAgentFundamentals

  final class ProfileLoadAgent(
      scheduler: ActorRef,
      initStateData: ParticipantInitializeStateData[
        LoadInput,
        LoadRuntimeConfig,
        ComplexPower,
      ],
      override val listener: Iterable[ActorRef],
  ) extends LoadAgent[
        ProfileRelevantData,
        ProfileLoadModel,
      ](scheduler, initStateData, listener)
      with ProfileLoadAgentFundamentals

  final class RandomLoadAgent(
      scheduler: ActorRef,
      initStateData: ParticipantInitializeStateData[
        LoadInput,
        LoadRuntimeConfig,
        ComplexPower,
      ],
      override val listener: Iterable[ActorRef],
  ) extends LoadAgent[
        RandomRelevantData,
        RandomLoadModel,
      ](scheduler, initStateData, listener)
      with RandomLoadAgentFundamentals
}

/** Creating a load agent
  *
  * @param scheduler
  *   Actor reference of the scheduler
  * @param listener
  *   List of listeners interested in results
  */
abstract class LoadAgent[LD <: LoadRelevantData, LM <: LoadModel[LD]](
    scheduler: ActorRef,
    initStateData: ParticipantInitializeStateData[
      LoadInput,
      LoadRuntimeConfig,
      ComplexPower,
    ],
    override val listener: Iterable[ActorRef],
) extends ParticipantAgent[
      ComplexPower,
      LD,
      ConstantState.type,
      ParticipantStateData[ComplexPower],
      LoadInput,
      LoadRuntimeConfig,
      LM,
    ](scheduler, initStateData)
    with LoadAgentFundamentals[LD, LM] {
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
