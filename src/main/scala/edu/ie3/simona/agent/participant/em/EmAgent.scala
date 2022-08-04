/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import akka.actor.{Actor, ActorRef, Props}
import edu.ie3.datamodel.models.input.system.{EmInput, SystemParticipantInput}
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPowerAndHeat
import edu.ie3.simona.agent.participant.data.Data.{PrimaryData, SecondaryData}
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.em.EmAgent.{
  EmAgentInitializeStateData,
  EmAgentModelBaseStateData
}
import edu.ie3.simona.agent.participant.em.EmSchedulerStateData.TriggerData
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.InitializeStateData
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.model.participant.EmModel
import edu.ie3.simona.model.participant.EmModel.EmRelevantData
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeParticipantAgentTrigger
}
import edu.ie3.simona.util.SimonaConstants
import edu.ie3.util.quantities.PowerSystemUnits.PU
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.Dimensionless

object EmAgent {
  def props(
      scheduler: ActorRef,
      listener: Iterable[ActorRef]
  ): Props =
    Props(
      new EmAgent(
        scheduler,
        listener
      )
    )

  final case class EmAgentInitializeStateData[
      PD <: PrimaryData
  ](
      inputModel: EmInput,
      modelConfig: EmRuntimeConfig,
      primaryServiceProxy: ActorRef,
      secondaryDataServices: Option[
        Vector[SecondaryDataService[_ <: SecondaryData]]
      ],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: ParticipantNotifierConfig,
      connectedAgents: Seq[
        (
            ActorRef,
            InitializeParticipantAgentTrigger[_, _],
            SystemParticipantInput
        )
      ]
  ) extends InitializeStateData[PD]

  final case class EmAgentModelBaseStateData(
      startDate: ZonedDateTime,
      endDate: ZonedDateTime,
      model: EmModel,
      services: Option[
        Vector[SecondaryDataService[_ <: SecondaryData]]
      ],
      outputConfig: ParticipantNotifierConfig,
      additionalActivationTicks: Array[Long],
      foreseenDataTicks: Map[ActorRef, Option[Long]],
      requestVoltageDeviationThreshold: Double,
      voltageValueStore: ValueStore[
        ComparableQuantity[Dimensionless]
      ],
      resultValueStore: ValueStore[ApparentPowerAndHeat],
      requestValueStore: ValueStore[ApparentPowerAndHeat],
      calcRelevantDateStore: ValueStore[EmRelevantData],
      receivedResultsValueStore: ValueStore[
        Map[ActorRef, Option[SystemParticipantResult]]
      ],
      schedulerStateData: EmSchedulerStateData,
      connectedAgents: Seq[(ActorRef, SystemParticipantInput)]
  ) extends ModelBaseStateData[ApparentPowerAndHeat, EmRelevantData, EmModel] {
    override val modelUuid: UUID = model.getUuid
  }

}

/** Creating an Energy Management Agent (EmAgent)
  *
  * @param scheduler
  *   Actor reference of the scheduler
  * @param listener
  *   List of listeners interested in results
  */
class EmAgent(
    scheduler: ActorRef,
    listener: Iterable[ActorRef]
) extends Actor
    with EmSchedulerHelper {
  override def receive: Receive = receiveUninitialized

  def receiveUninitialized: Receive = {
    case TriggerWithIdMessage(
          InitializeParticipantAgentTrigger(
            EmAgentInitializeStateData(
              inputModel,
              modelConfig,
              _, // replaying primary data is disabled here for now
              services,
              simulationStartDate,
              simulationEndDate,
              resolution,
              requestVoltageDeviationThreshold,
              outputConfig,
              connectedAgents
            )
          ),
          triggerId,
          _
        ) =>
      // sending init triggers
      val triggerData = connectedAgents.foldLeft(TriggerData()) {
        case (triggerData, (actor, initTrigger, _)) =>
          context.watch(actor)
          scheduleTrigger(
            initTrigger,
            actor,
            triggerData,
            SimonaConstants.INIT_SIM_TICK
          )
      }

      val connectedInputs = connectedAgents.map { case (actor, _, input) =>
        (actor, input)
      }

      val model = EmModel(
        inputModel,
        modelConfig,
        simulationStartDate,
        simulationEndDate,
        connectedInputs
      )

      val baseStateData = EmAgentModelBaseStateData(
        simulationStartDate,
        simulationEndDate,
        model,
        services,
        outputConfig,
        Array.empty,
        Map.empty,
        requestVoltageDeviationThreshold,
        ValueStore.forVoltage(
          resolution * 10,
          inputModel.getNode
            .getvTarget()
            .to(PU)
        ),
        ValueStore.forResult(resolution, 10),
        ValueStore(resolution * 10),
        ValueStore(resolution * 10),
        ValueStore(resolution * 10),
        EmSchedulerStateData(trigger = triggerData),
        connectedInputs
      )

      setActiveTickAndSendTriggers(
        baseStateData,
        SimonaConstants.INIT_SIM_TICK,
        triggerId
      )
  }

  def receiveIdle(baseStateData: EmAgentModelBaseStateData): Receive = {
    case triggerToSchedule: ScheduleTriggerMessage =>
      context become receiveIdle(
        baseStateData.copy(schedulerStateData =
          sendEligibleTrigger(
            scheduleTrigger(triggerToSchedule, baseStateData.schedulerStateData)
          )
        )
      )

    /* process completion messages */
    case completionMessage: CompletionMessage =>
      // there can be new triggers for the current tick, which need to be sent out immediately
      val updatedStateData =
        maybeTicksCompleted(
          sendEligibleTrigger(
            handleCompletionMessage(
              completionMessage,
              baseStateData.schedulerStateData
            )
          )
        )

      context become receiveIdle(
        baseStateData.copy(schedulerStateData = updatedStateData)
      )

    case TriggerWithIdMessage(ActivityStartTrigger(newTick), triggerId, _) =>
      // prepare map for expected results for this tick
      val resultsForTick =
        baseStateData.schedulerStateData.trigger.triggerQueue
          .get(newTick)
          .map {
            _.map { _.agent }.map(_ -> None).toMap
          }
          .getOrElse(Map.empty)

      val updatedValueStore = ValueStore.updateValueStore(
        baseStateData.receivedResultsValueStore,
        newTick,
        resultsForTick
      )

      val updatedBaseStateData =
        baseStateData.copy(receivedResultsValueStore = updatedValueStore)

      setActiveTickAndSendTriggers(updatedBaseStateData, newTick, triggerId)

    case ParticipantResultEvent(systemParticipantResult) =>
      val tick = baseStateData.schedulerStateData.nowInTicks

      val resultsForTick =
        baseStateData.receivedResultsValueStore.getOrElse(tick, Map.empty)

      val updatedResultsForTick =
        resultsForTick.updated(
          sender(),
          Some(systemParticipantResult)
        )

      val updatedValueStore = ValueStore.updateValueStore(
        baseStateData.receivedResultsValueStore,
        tick,
        updatedResultsForTick
      )

      // check if all expected results arrived
      // TODO

      context become receiveIdle(
        baseStateData.copy(
          receivedResultsValueStore = updatedValueStore
        )
      )
  }

  private def setActiveTickAndSendTriggers(
      baseStateData: EmAgentModelBaseStateData,
      newTick: Long,
      triggerId: Long
  ): Unit = {
    val updatedStateData = baseStateData.schedulerStateData.copy(
      nowInTicks = newTick,
      mainTrigger = baseStateData.schedulerStateData.mainTrigger +
        (newTick -> Some(triggerId))
    )

    context become receiveIdle(
      baseStateData.copy(schedulerStateData =
        sendEligibleTrigger(updatedStateData)
      )
    )
  }
}
