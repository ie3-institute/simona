/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import akka.actor.{ActorRef, Props}
import edu.ie3.datamodel.models.input.system.{
  EmInput,
  EvcsInput,
  HpInput,
  SystemParticipantInput
}
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPowerAndHeat
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.em.EmAgent.{
  EmAgentInitializeStateData,
  EmAgentModelBaseStateData
}
import edu.ie3.simona.agent.participant.em.EmSchedulerStateData.TriggerData
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.InitializeStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantUninitializedStateData
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.model.participant.EmModel
import edu.ie3.simona.model.participant.EmModel.EmRelevantData
import edu.ie3.simona.ontology.messages.FlexibilityMessage.{
  ProvideFlexibilityOptions,
  RequestFlexibilityOptions
}
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

  final case class EmAgentInitializeStateData(
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
  ) extends InitializeStateData[ApparentPowerAndHeat]

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
      receivedFlexOptionsValueStore: ValueStore[
        Map[ActorRef, Option[ProvideFlexibilityOptions]]
      ],
      schedulerStateData: EmSchedulerStateData,
      uncontrolledAgents: Seq[ActorRef],
      controlledAgents: Seq[ActorRef]
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
    val scheduler: ActorRef,
    override val listener: Iterable[ActorRef]
) extends ParticipantAgent[
      ApparentPowerAndHeat,
      EmRelevantData,
      EmAgentModelBaseStateData,
      EmInput,
      EmRuntimeConfig,
      EmModel
    ](
      scheduler
    )
    with EmAgentFundamentals
    with EmSchedulerHelper {

  private val handleUnitializedEm: StateFunction = {
    case Event(
          TriggerWithIdMessage(
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
          ),
          _: ParticipantUninitializedStateData[ApparentPowerAndHeat]
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

      val (uncontrolledAgents, controlledAgents) = connectedAgents.partition {
        case (_, _, _: HpInput)   => true
        case (_, _, _: EvcsInput) => true
        case _                    => false
      }

      val model = EmModel(
        inputModel,
        modelConfig,
        simulationStartDate,
        simulationEndDate,
        uncontrolledAgents.map { case (actor, _, input) => (actor, input) },
        controlledAgents.map { case (actor, _, input) => (actor, input) }
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
        ValueStore(resolution * 10),
        EmSchedulerStateData(trigger = triggerData),
        uncontrolledAgents.map { case (actor, _, _) => actor },
        controlledAgents.map { case (actor, _, _) => actor }
      )

      setActiveTickAndSendTriggers(
        baseStateData,
        SimonaConstants.INIT_SIM_TICK,
        triggerId
      )

  }

  private val handleIdleEm: StateFunction = {
    case Event(
          triggerToSchedule: ScheduleTriggerMessage,
          baseStateData: EmAgentModelBaseStateData
        ) =>
      stay() using
        baseStateData.copy(schedulerStateData =
          sendEligibleTrigger(
            scheduleTrigger(triggerToSchedule, baseStateData.schedulerStateData)
          )
        )

    case Event(
          completionMessage: CompletionMessage,
          baseStateData: EmAgentModelBaseStateData
        ) =>
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

      stay() using baseStateData.copy(schedulerStateData = updatedStateData)

    case Event(
          TriggerWithIdMessage(ActivityStartTrigger(newTick), triggerId, _),
          baseStateData: EmAgentModelBaseStateData
        ) =>
      // prepare map for expected results for this tick
      // TODO do we also always expected results for controlled agents? probably yes
      val resultsForTick =
        baseStateData.schedulerStateData.trigger.triggerQueue
          .get(newTick)
          .map {
            _.map { _.agent }
              .map(_ -> None)
              .toMap
          }
          .getOrElse(Map.empty)

      val updatedResValueStore = ValueStore.updateValueStore(
        baseStateData.receivedResultsValueStore,
        newTick,
        resultsForTick
      )

      // prepare map for expected flex options for this tick
      val expectedFlexOptions = baseStateData.controlledAgents
        .map(_ -> None)
        .toMap

      val updatedFlexValueStore = ValueStore.updateValueStore(
        baseStateData.receivedFlexOptionsValueStore,
        newTick,
        expectedFlexOptions
      )

      val updatedBaseStateData =
        baseStateData.copy(
          receivedResultsValueStore = updatedResValueStore,
          receivedFlexOptionsValueStore = updatedFlexValueStore
        )

      // send out flex requests for all controlled agents
      baseStateData.controlledAgents.foreach {
        _ ! RequestFlexibilityOptions
      }

      // send out all ActivityStartTriggers
      setActiveTickAndSendTriggers(updatedBaseStateData, newTick, triggerId)

    case Event(
          ParticipantResultEvent(systemParticipantResult),
          baseStateData: EmAgentModelBaseStateData
        ) =>
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

      val updatedBaseStateData = baseStateData.copy(
        receivedResultsValueStore = updatedValueStore
      )

      maybeIssueFlexControl(updatedBaseStateData)

      stay() using updatedBaseStateData

    case Event(
          flexOptions: ProvideFlexibilityOptions,
          baseStateData: EmAgentModelBaseStateData
        ) =>
      val tick = baseStateData.schedulerStateData.nowInTicks

      val flexOptionsForTick =
        baseStateData.receivedFlexOptionsValueStore.getOrElse(tick, Map.empty)

      val updatedFlexOptionsTick =
        flexOptionsForTick.updated(
          sender(),
          Some(flexOptions)
        )

      val updatedValueStore = ValueStore.updateValueStore(
        baseStateData.receivedFlexOptionsValueStore,
        tick,
        updatedFlexOptionsTick
      )

      val updatedBaseStateData = baseStateData.copy(
        receivedFlexOptionsValueStore = updatedValueStore
      )

      maybeIssueFlexControl(updatedBaseStateData)

      stay() using updatedBaseStateData
  }

  when(Uninitialized) { handleUnitializedEm orElse handleUnitialized }

  when(Idle) { handleIdleEm orElse handleIdle }

  private def setActiveTickAndSendTriggers(
      baseStateData: EmAgentModelBaseStateData,
      newTick: Long,
      triggerId: Long
  ): State = {
    val updatedStateData = baseStateData.schedulerStateData.copy(
      nowInTicks = newTick,
      mainTrigger = baseStateData.schedulerStateData.mainTrigger +
        (newTick -> Some(triggerId))
    )

    goto(Idle) using baseStateData.copy(schedulerStateData =
      sendEligibleTrigger(updatedStateData)
    )
  }

  private def maybeIssueFlexControl(
      baseStateData: EmAgentModelBaseStateData
  ): Unit = {
    val tick = baseStateData.schedulerStateData.nowInTicks

    // check if all expected results arrived
    val resultsForTick =
      baseStateData.receivedResultsValueStore.getOrElse(tick, Map.empty)

    val uncontrolledResultsReceived =
      resultsForTick.collect { case (actor, None) =>
        baseStateData.uncontrolledAgents.contains(actor)
      }.isEmpty

    // check if flex answers arrived
    val receivedFlexOptions = baseStateData.receivedFlexOptionsValueStore
      .getOrElse(tick, Map.empty)

    val flexAnswersReceived = !receivedFlexOptions
      .exists { case (_, None) => true }

    if (uncontrolledResultsReceived && flexAnswersReceived) {
      baseStateData.model
        .determineDeviceControl(
          receivedFlexOptions
        )
        .foreach { case (agent, flexOptions) =>
          agent ! flexOptions
        }
    }
  }
}
