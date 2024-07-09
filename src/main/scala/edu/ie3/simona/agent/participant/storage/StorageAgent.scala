/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.storage

import edu.ie3.datamodel.models.input.system.StorageInput
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ApparentPower => ComplexPower
}
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.config.SimonaConfig.StorageRuntimeConfig
import edu.ie3.simona.model.participant.StorageModel
import edu.ie3.simona.model.participant.StorageModel.{
  StorageRelevantData,
  StorageState,
}
import org.apache.pekko.actor.{ActorRef, Props}

object StorageAgent {
  def props(
      scheduler: ActorRef,
      initStateData: ParticipantInitializeStateData[
        StorageInput,
        StorageRuntimeConfig,
        ComplexPower,
      ],
      listener: Iterable[ActorRef],
  ): Props =
    Props(
      new StorageAgent(
        scheduler,
        initStateData,
        listener,
      )
    )
}

/** Creating a battery storage agent
  *
  * @param scheduler
  *   Actor reference of the scheduler
  * @param listener
  *   List of listeners interested in results
  */
class StorageAgent(
    scheduler: ActorRef,
    initStateData: ParticipantInitializeStateData[
      StorageInput,
      StorageRuntimeConfig,
      ComplexPower,
    ],
    override val listener: Iterable[ActorRef],
) extends ParticipantAgent[
      ComplexPower,
      StorageRelevantData,
      StorageState,
      ParticipantStateData[ComplexPower],
      StorageInput,
      StorageRuntimeConfig,
      StorageModel,
    ](
      scheduler,
      initStateData,
    )
    with StorageAgentFundamentals {}
