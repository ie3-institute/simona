/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.load

import akka.actor.{ActorRef, Props}
import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.load.LoadAgentFundamentals.{
  FixedLoadAgentFundamentals,
  ProfileLoadAgentFundamentals,
  RandomLoadAgentFundamentals,
  TemperatureProfileLoadAgentFundamentals
}
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
import edu.ie3.simona.config.SimonaConfig.LoadRuntimeConfig
import edu.ie3.simona.model.participant.CalcRelevantData.LoadRelevantData
import edu.ie3.simona.model.participant.load.profile.ProfileLoadModel
import edu.ie3.simona.model.participant.load.profile.ProfileLoadModel.{
  ProfileRelevantData,
  StandardProfileRelevantData,
  TemperatureProfileRelevantData
}
import edu.ie3.simona.model.participant.load.profile.standard.StandardProfileLoadModel
import edu.ie3.simona.model.participant.load.profile.temperature.TemperatureDependantProfileLoadModel
import edu.ie3.simona.model.participant.load.random.RandomLoadModel
import edu.ie3.simona.model.participant.load.random.RandomLoadModel.RandomRelevantData
import edu.ie3.simona.model.participant.load.{
  FixedLoadModel,
  LoadModel,
  LoadModelBehaviour
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
    override val listener: Iterable[ActorRef]
) extends ParticipantAgent[
      ApparentPower,
      LD,
      ParticipantStateData[ApparentPower],
      LoadInput,
      LoadRuntimeConfig,
      LM
    ](scheduler)
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

object LoadAgent {
  def props(
      scheduler: ActorRef,
      listener: Iterable[ActorRef],
      modelConfig: LoadRuntimeConfig
  ): Props =
    LoadModelBehaviour(modelConfig.modelBehaviour) match {
      case LoadModelBehaviour.FIX =>
        Props(new FixedLoadAgent(scheduler, listener))
      case LoadModelBehaviour.PROFILE =>
        Props(new ProfileLoadAgent(scheduler, listener))
      case LoadModelBehaviour.RANDOM =>
        Props(new RandomLoadAgent(scheduler, listener))
      case unsupported =>
        throw new IllegalArgumentException(
          s"The load agent behaviour '$unsupported' is currently not supported."
        )
    }

  final class FixedLoadAgent(
      scheduler: ActorRef,
      override val listener: Iterable[ActorRef]
  ) extends LoadAgent[
        FixedLoadModel.FixedLoadRelevantData.type,
        FixedLoadModel
      ](scheduler, listener)
      with FixedLoadAgentFundamentals

  final class ProfileLoadAgent(
      scheduler: ActorRef,
      override val listener: Iterable[ActorRef]
  ) extends LoadAgent[
        StandardProfileRelevantData,
        StandardProfileLoadModel
      ](scheduler, listener)
      with ProfileLoadAgentFundamentals

  final class TemperatureProfileLoadAgent(
      scheduler: ActorRef,
      override val listener: Iterable[ActorRef]
  ) extends LoadAgent[
        TemperatureProfileRelevantData,
        TemperatureDependantProfileLoadModel
      ](scheduler, listener)
      with TemperatureProfileLoadAgentFundamentals

  final class RandomLoadAgent(
      scheduler: ActorRef,
      override val listener: Iterable[ActorRef]
  ) extends LoadAgent[
        RandomRelevantData,
        RandomLoadModel
      ](scheduler, listener)
      with RandomLoadAgentFundamentals
}
