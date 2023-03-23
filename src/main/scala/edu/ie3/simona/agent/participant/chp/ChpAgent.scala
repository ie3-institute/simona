package edu.ie3.simona.agent.participant.chp

import akka.actor.{ActorRef, FSM, Props}
import edu.ie3.datamodel.models.input.system.ChpInput
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{ApparentPower, ApparentPowerAndHeat}
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.statedata.{DataCollectionStateData, ParticipantStateData}
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.config.SimonaConfig.ChpRuntimeConfig
import edu.ie3.simona.model.participant.ChpModel
import edu.ie3.simona.model.participant.ChpModel.ChpData
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import javax.measure.quantity.Power

object ChpAgent {
  def props(
             scheduler: ActorRef,
             listener: Iterable[ActorRef]
           ): Props =
    Props(
      new ChpAgent(
        scheduler,
        listener
      )
    )

  val neededServices: Vector[Class[_ <: SecondaryDataService[_]]] = Vector(
    classOf[ActorWeatherService]
  )
}



/** Creating a combined heat and power plant agent.
  *
  * @param scheduler
  *   Actor reference of the scheduler
  * @param listener
  *   List of listeners interested in results
  */
class ChpAgent(
              scheduler: ActorRef,
              override val listener: Iterable[ActorRef]
              ) extends ParticipantAgent[
  ApparentPowerAndHeat,
  ChpData,
  ParticipantStateData[ApparentPowerAndHeat],
  ChpInput,
  ChpRuntimeConfig,
  ChpModel
  ](scheduler) with ChpAgentFundamentals {
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
