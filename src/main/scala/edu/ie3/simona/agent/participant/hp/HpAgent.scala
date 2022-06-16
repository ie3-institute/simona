/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.hp

import akka.actor.{ActorRef, Props}
import edu.ie3.datamodel.models.input.system.HpInput
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPowerAndHeat
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
import edu.ie3.simona.config.SimonaConfig.HpRuntimeConfig
import edu.ie3.simona.model.participant.HpModel
import edu.ie3.simona.model.participant.HpModel.HpData

object HpAgent {
  def props(
      scheduler: ActorRef,
      listener: Iterable[ActorRef]
  ): Props =
    Props(
      new HpAgent(
        scheduler,
        listener
      )
    )

  val neededServices: Vector[Class[_ <: SecondaryDataService[_]]] = Vector(
    classOf[ActorWeatherService]
  )
}

class HpAgent(
    scheduler: ActorRef,
    override val listener: Iterable[ActorRef]
) extends ParticipantAgent[ApparentPowerAndHeat, HpData, ParticipantStateData[
      ApparentPowerAndHeat
    ], HpInput, HpRuntimeConfig, HpModel](scheduler)
    with HpAgentFundamentals {

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
