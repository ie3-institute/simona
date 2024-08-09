/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.evcs

import edu.ie3.datamodel.models.input.system.EvcsInput
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ApparentPower,
  ZERO_POWER,
}
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorEvMovementsService
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.{
  BaseStateData,
  DataCollectionStateData,
  ParticipantStateData,
}
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.agent.participant.{
  ParticipantAgent,
  ParticipantAgentFundamentals,
}
import edu.ie3.simona.agent.state.AgentState.Idle
import edu.ie3.simona.agent.state.ParticipantAgentState.HandleInformation
import edu.ie3.simona.config.SimonaConfig.EvcsRuntimeConfig
import edu.ie3.simona.model.participant.evcs.EvcsModel
import edu.ie3.simona.model.participant.evcs.EvcsModel.{
  EvcsRelevantData,
  EvcsState,
}
import edu.ie3.simona.ontology.messages.services.EvMessage.{
  DepartingEvsRequest,
  EvFreeLotsRequest,
}
import edu.ie3.util.scala.quantities.ReactivePower
import org.apache.pekko.actor.{ActorRef, Props}
import squants.Power

object EvcsAgent {
  def props(
      scheduler: ActorRef,
      initStateData: ParticipantInitializeStateData[
        EvcsInput,
        EvcsRuntimeConfig,
        ApparentPower,
      ],
      listener: Iterable[ActorRef],
  ): Props =
    Props(
      new EvcsAgent(
        scheduler,
        initStateData,
        listener,
      )
    )

  val neededServices: Vector[Class[_ <: SecondaryDataService[_]]] = Vector(
    classOf[ActorEvMovementsService]
  )
}

class EvcsAgent(
    scheduler: ActorRef,
    initStateData: ParticipantInitializeStateData[
      EvcsInput,
      EvcsRuntimeConfig,
      ApparentPower,
    ],
    override val listener: Iterable[ActorRef],
) extends ParticipantAgent[
      ApparentPower,
      EvcsRelevantData,
      EvcsState,
      ParticipantStateData[ApparentPower],
      EvcsInput,
      EvcsRuntimeConfig,
      EvcsModel,
    ](scheduler, initStateData)
    with EvcsAgentFundamentals {
  override val alternativeResult: ApparentPower = ZERO_POWER

  when(Idle) {
    case Event(
          EvFreeLotsRequest(tick),
          modelBaseStateData: ParticipantModelBaseStateData[
            ApparentPower,
            EvcsRelevantData,
            EvcsState,
            EvcsModel,
          ],
        ) =>
      handleFreeLotsRequest(tick, modelBaseStateData)
      stay()

    case Event(
          DepartingEvsRequest(tick, departingEvs),
          modelBaseStateData: ParticipantModelBaseStateData[
            ApparentPower,
            EvcsRelevantData,
            EvcsState,
            EvcsModel,
          ],
        ) =>
      val updatedStateData =
        handleDepartingEvsRequest(tick, departingEvs, modelBaseStateData)
      stay() using updatedStateData
  }

  when(HandleInformation) {
    // FreeLotsRequest and DepartingEvsRequest also need to be handled
    // in case the activation has arrived first

    case Event(
          EvFreeLotsRequest(tick),
          stateData: DataCollectionStateData[ApparentPower],
        ) =>
      stateData.baseStateData match {
        case modelStateData: BaseStateData.ParticipantModelBaseStateData[
              ApparentPower,
              EvcsRelevantData,
              EvcsState,
              EvcsModel,
            ] =>
          handleFreeLotsRequest(tick, modelStateData)
          stay()
        case x =>
          throw new IllegalStateException(
            s"Unsupported base state data '$x' when receiving FreeLotsRequest"
          )
      }

    case Event(
          DepartingEvsRequest(tick, departingEvs),
          stateData: DataCollectionStateData[ApparentPower],
        ) =>
      stateData.baseStateData match {
        case modelStateData: BaseStateData.ParticipantModelBaseStateData[
              ApparentPower,
              EvcsRelevantData,
              EvcsState,
              EvcsModel,
            ] =>
          val updatedStateData =
            handleDepartingEvsRequest(tick, departingEvs, modelStateData)
          stay() using stateData.copy(baseStateData = updatedStateData)
        case x =>
          throw new IllegalStateException(
            s"Unsupported base state data '$x' when receiving DepartingEvsRequest"
          )
      }
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
      ],
  ): ApparentPower =
    ParticipantAgentFundamentals.averageApparentPower(
      tickToResults,
      windowStart,
      windowEnd,
      activeToReactivePowerFuncOpt,
      log,
    )
}
