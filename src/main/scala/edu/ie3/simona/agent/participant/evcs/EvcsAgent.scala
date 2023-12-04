/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.evcs

import org.apache.pekko.actor.{ActorRef, Props}
import edu.ie3.datamodel.models.input.system.EvcsInput
import edu.ie3.simona.agent.participant.{
  ParticipantAgent,
  ParticipantAgentFundamentals
}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ApparentPower,
  ZERO_POWER
}
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorEvMovementsService
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.agent.state.AgentState.Idle
import edu.ie3.simona.config.SimonaConfig.EvcsRuntimeConfig
import edu.ie3.simona.model.participant.EvcsModel
import edu.ie3.simona.model.participant.EvcsModel.EvcsRelevantData
import edu.ie3.simona.ontology.messages.services.EvMessage.{
  DepartingEvsRequest,
  EvFreeLotsRequest
}
import edu.ie3.util.scala.quantities.ReactivePower
import squants.Power

object EvcsAgent {
  def props(
      scheduler: ActorRef,
      initStateData: ParticipantInitializeStateData[
        EvcsInput,
        EvcsRuntimeConfig,
        ApparentPower
      ],
      listener: Iterable[ActorRef]
  ): Props =
    Props(
      new EvcsAgent(
        scheduler,
        initStateData,
        listener
      )
    )

  val neededServices: Vector[Class[? <: SecondaryDataService[?]]] = Vector(
    classOf[ActorEvMovementsService]
  )
}

class EvcsAgent(
    scheduler: ActorRef,
    initStateData: ParticipantInitializeStateData[
      EvcsInput,
      EvcsRuntimeConfig,
      ApparentPower
    ],
    override val listener: Iterable[ActorRef]
) extends ParticipantAgent[
      ApparentPower,
      EvcsRelevantData,
      ParticipantStateData[ApparentPower],
      EvcsInput,
      EvcsRuntimeConfig,
      EvcsModel
    ](scheduler, initStateData)
    with EvcsAgentFundamentals {
  override val alternativeResult: ApparentPower = ZERO_POWER

  when(Idle) {
    case Event(
          EvFreeLotsRequest(tick),
          modelBaseStateData: ParticipantModelBaseStateData[
            ApparentPower,
            EvcsRelevantData,
            EvcsModel
          ]
        ) =>
      handleFreeLotsRequest(tick, modelBaseStateData)
      stay()
    case Event(
          DepartingEvsRequest(tick, departingEvs),
          modelBaseStateData: ParticipantModelBaseStateData[
            ApparentPower,
            EvcsRelevantData,
            EvcsModel
          ]
        ) =>
      val updatedStateData =
        handleDepartingEvsRequest(tick, modelBaseStateData, departingEvs)
      stay() using updatedStateData
  }

  /** Determine the average result within the given tick window
    *
    * @param tickToResults
    *   Mapping from data tick to actual data
    * @param windowStart
    *   First, included tick of the time window
    * @param windowEnd
    *   Last, included tick of the time window
    * @param activeToReactivePowerFuncOpt
    *   An Option on a function, that transfers the active into reactive power
    * @return
    *   The averaged result
    */
  override def averageResults(
      tickToResults: Map[Long, ApparentPower],
      windowStart: Long,
      windowEnd: Long,
      activeToReactivePowerFuncOpt: Option[
        Power => ReactivePower
      ]
  ): ApparentPower =
    ParticipantAgentFundamentals.averageApparentPower(
      tickToResults,
      windowStart,
      windowEnd,
      activeToReactivePowerFuncOpt,
      log
    )
}
