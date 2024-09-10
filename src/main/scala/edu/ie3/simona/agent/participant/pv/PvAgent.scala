/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.pv

import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPower
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.config.SimonaConfig.PvRuntimeConfig
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.PvModel
import edu.ie3.simona.model.participant.PvModel.PvRelevantData
import org.apache.pekko.actor.{ActorRef, Props}

object PvAgent {
  def props(
      scheduler: ActorRef,
      initStateData: ParticipantInitializeStateData[
        PvInput,
        PvRuntimeConfig,
        ComplexPower,
      ],
      listener: Iterable[ActorRef],
  ): Props =
    Props(
      new PvAgent(
        scheduler,
        initStateData,
        listener,
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
    initStateData: ParticipantInitializeStateData[
      PvInput,
      PvRuntimeConfig,
      ComplexPower,
    ],
    override val listener: Iterable[ActorRef],
) extends ParticipantAgent[
      ComplexPower,
      PvRelevantData,
      ConstantState.type,
      ParticipantStateData[ComplexPower],
      PvInput,
      PvRuntimeConfig,
      PvModel,
    ](
      scheduler,
      initStateData,
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
