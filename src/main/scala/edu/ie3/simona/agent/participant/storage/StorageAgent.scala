/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.storage

import akka.actor.{ActorRef, Props}
import edu.ie3.datamodel.models.input.system.StorageInput
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
import edu.ie3.simona.config.SimonaConfig.StorageRuntimeConfig
import edu.ie3.simona.model.participant.StorageModel
import edu.ie3.simona.model.participant.StorageModel.StorageRelevantData

object StorageAgent {
  def props(
      scheduler: ActorRef,
      listener: Iterable[ActorRef]
  ): Props =
    Props(
      new StorageAgent(
        scheduler,
        listener
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
    override val listener: Iterable[ActorRef]
) extends ParticipantAgent[
      ApparentPower,
      StorageRelevantData,
      ParticipantStateData[ApparentPower],
      StorageInput,
      StorageRuntimeConfig,
      StorageModel
    ](
      scheduler
    )
    with StorageAgentFundamentals {}
