/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.pv

import akka.actor.Props
import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
import edu.ie3.simona.akka.SimonaActorRef
import edu.ie3.simona.config.SimonaConfig.PvRuntimeConfig
import edu.ie3.simona.model.participant.PVModel
import edu.ie3.simona.model.participant.PVModel.PVRelevantData

object PVAgent {
  def props(
      scheduler: SimonaActorRef,
      listener: Iterable[SimonaActorRef]
  ): Props =
    Props(
      new PVAgent(
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
class PVAgent(
    scheduler: SimonaActorRef,
    override val listener: Iterable[SimonaActorRef]
) extends ParticipantAgent[
      ApparentPower,
      PVRelevantData,
      ParticipantStateData[ApparentPower],
      PvInput,
      PvRuntimeConfig,
      PVModel
    ](
      scheduler
    )
    with PVAgentFundamentals {

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
