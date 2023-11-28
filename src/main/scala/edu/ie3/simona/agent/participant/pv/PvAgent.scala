/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.pv

import org.apache.pekko.actor.{ActorRef, Props}
import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
import edu.ie3.simona.config.SimonaConfig.PvRuntimeConfig
import edu.ie3.simona.model.participant.PvModel
import edu.ie3.simona.model.participant.PvModel.PvRelevantData

object PvAgent {
  def props(
      scheduler: ActorRef,
      listener: Iterable[ActorRef]
  ): Props =
    Props(
      new PvAgent(
        scheduler,
        listener
      )
    )

  val neededServices: Vector[Class[_ <: SecondaryDataService[_]]] = Vector(
    classOf[ActorWeatherService]
  )
}

/** Creating a photovoltaic plant agent
  *
  * @param scheduler
  *   Actor reference of the scheduler
  * @param listener
  *   List of listeners interested in results
  */
class PvAgent(
    scheduler: ActorRef,
    override val listener: Iterable[ActorRef]
) extends ParticipantAgent[
      ApparentPower,
      PvRelevantData,
      ParticipantStateData[ApparentPower],
      PvInput,
      PvRuntimeConfig,
      PvModel
    ](
      scheduler
    )
    with PvAgentFundamentals {

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
