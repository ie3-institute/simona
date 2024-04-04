/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.load.markov

import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.statedata.{
  BaseStateData,
  ParticipantStateData,
}
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.config.SimonaConfig.LoadRuntimeConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.model.participant.{FlexChangeIndicator, ModelState}
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.load.markov.{
  MarkovModel,
  MarkovRelevantData,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage
import edu.ie3.util.scala.quantities.ReactivePower
import org.apache.pekko.actor.{ActorRef, FSM, Props, typed}
import squants.{Dimensionless, Power}

import java.time.ZonedDateTime
import java.util.UUID

object MarkovAgent {
  def props(
      scheduler: ActorRef,
      initStateData: ParticipantInitializeStateData[
        LoadInput,
        LoadRuntimeConfig,
        ApparentPower,
      ],
      listener: Iterable[ActorRef],
  ): Props =
    Props(
      new MarkovAgent(
        scheduler,
        initStateData: ParticipantInitializeStateData[
          LoadInput,
          LoadRuntimeConfig,
          ApparentPower,
        ],
        listener,
      )
    )
}

/** Creating a load agent
  *
  * @param scheduler
  *   Actor reference of the scheduler
  * @param listener
  *   List of listeners interested in results
  */
class MarkovAgent(
    scheduler: ActorRef,
    initStateData: ParticipantInitializeStateData[
      LoadInput,
      LoadRuntimeConfig,
      ApparentPower,
    ],
    override val listener: Iterable[ActorRef],
) extends ParticipantAgent[
      ApparentPower,
      MarkovRelevantData,
      ConstantState.type,
      ParticipantStateData[ApparentPower],
      LoadInput,
      LoadRuntimeConfig,
      MarkovModel,
    ](scheduler, initStateData)
    with MarkovAgentFundamentals {
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
