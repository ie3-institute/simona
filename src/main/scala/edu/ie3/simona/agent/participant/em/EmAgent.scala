/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import akka.actor.{ActorRef, Props}
import edu.ie3.datamodel.models.input.system.{EmInput, SystemParticipantInput}
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
import edu.ie3.simona.agent.state.ParticipantAgentState.Calculate
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.model.participant.EmModel
import edu.ie3.simona.model.participant.EmModel.EmRelevantData
import edu.ie3.simona.ontology.messages.FlexibilityMessage.{
  ProvideFlexOptions,
  ProvideMinMaxFlexOptions,
  RequestFlexibilityOptions
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.trigger.Trigger.ParticipantTrigger.StartCalculationTrigger
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
      receivedFlexOptionsValueStore: Map[UUID, ValueStore[
        Option[ProvideFlexOptions]
      ]], // TODO enhanced todo: MultiValueStore
      // TODO issued ctrl value store ?
      actorRefToUuid: Map[ActorRef, UUID],
      connectedModels: Map[UUID, SystemParticipantInput],
      schedulerStateData: EmSchedulerStateData
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

      val model = EmModel(
        inputModel,
        modelConfig,
        simulationStartDate,
        simulationEndDate
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
        connectedAgents.map { case (_, _, sp) =>
          sp.getUuid -> ValueStore(resolution * 10)
        }.toMap,
        connectedAgents.map { case (actor, _, sp) =>
          actor -> sp.getUuid
        }.toMap,
        connectedAgents.map { case (_, _, sp) =>
          sp.getUuid -> sp
        }.toMap,
        EmSchedulerStateData(trigger = triggerData)
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
      val expectedActors =
        baseStateData.schedulerStateData.trigger.triggerQueue
          .get(newTick)
          .map {
            _.map(_.agent)
              .flatMap(actor =>
                baseStateData.actorRefToUuid.get(actor).map(actor -> _)
              )
              .toMap
          }
          .getOrElse(Map.empty)

      // prepare map for expected flex options and expected results for this tick
      val expectedParticipants =
        expectedActors.map { case (_, uuid) => uuid }

      val updatedReceivedFlexOptions = expectedParticipants.foldLeft(
        baseStateData.receivedFlexOptionsValueStore
      ) { case (stores, uuid) =>
        val participantValueStore = stores.getOrElse(
          uuid,
          throw new RuntimeException(s"ValueStore for UUID $uuid not found")
        )

        val updatedFlexOptionsStore =
          ValueStore.updateValueStore(
            participantValueStore,
            newTick,
            None
          )

        stores.updated(uuid, updatedFlexOptionsStore)
      }

      val updatedBaseStateData =
        baseStateData.copy(
          receivedFlexOptionsValueStore = updatedReceivedFlexOptions
        )

      // send out flex requests for all expected agents
      expectedActors.foreach { case (actor, _) =>
        actor ! RequestFlexibilityOptions
      }

      // send out all ActivityStartTriggers
      setActiveTickAndSendTriggers(updatedBaseStateData, newTick, triggerId)

    case Event(
          flexOptions: ProvideFlexOptions,
          baseStateData: EmAgentModelBaseStateData
        ) =>
      val tick = baseStateData.schedulerStateData.nowInTicks

      val receivedFlexOptions =
        baseStateData.receivedFlexOptionsValueStore.getOrElse(
          flexOptions.modelUuid,
          throw new RuntimeException(
            s"No received flex options store found for ${flexOptions.modelUuid}"
          )
        )

      val updatedValueStore = ValueStore.updateValueStore(
        receivedFlexOptions,
        tick,
        Some(flexOptions)
      )

      val updatedReceivedFlexOptions =
        baseStateData.receivedFlexOptionsValueStore.updated(
          flexOptions.modelUuid,
          updatedValueStore
        )

      val updatedBaseStateData = baseStateData.copy(
        receivedFlexOptionsValueStore = updatedReceivedFlexOptions
      )

      maybeIssueFlexControl(updatedBaseStateData)
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
  ): State = {
    val tick = baseStateData.schedulerStateData.nowInTicks

    // check if expected flex answers for this tick arrived
    val flexAnswers = baseStateData.receivedFlexOptionsValueStore.map {
      case (uuid, store) =>
        val spi = baseStateData.connectedModels.getOrElse(
          uuid,
          throw new RuntimeException(
            s"There's no flex options store for $uuid whatsoever"
          )
        )
        val (dataTick, flexOptions) = store
          .last()
          .getOrElse(
            throw new RuntimeException(
              s"There's no expected flex options for $spi whatsoever"
            )
          )

        (spi, flexOptions, dataTick == tick)
    }

    val flexAnswersReceived = !flexAnswers.exists {
      case (_, None, _) => true
      case _            => false
    }

    if (flexAnswersReceived) {
      // All flex options and all results have been received.

      val flexStratInput = flexAnswers.flatMap {
        case (spi, flexOptionsOpt, _) => flexOptionsOpt.map(spi -> _)
      }

      baseStateData.model
        .determineDeviceControl(
          flexStratInput.collect {
            case (spi, flexOption: ProvideMinMaxFlexOptions) =>
              (spi, flexOption)
          }.toSeq
        )
        .foreach { case (participantUuid, flexOptions) =>
          // send out flex control messages
          val agent = baseStateData.actorRefToUuid
            .collectFirst {
              case (actor, uuid) if uuid.equals(participantUuid) => actor
            }
            .getOrElse(
              throw new RuntimeException(
                s"Could not find actor for uuid $participantUuid"
              )
            )

          // activate the participants that have not been activated before
          val notActivatedBefore = flexAnswers.exists {
            case (spi, _, activated) =>
              spi.getUuid.equals(participantUuid) && !activated
          }

          if (notActivatedBefore)
            agent ! ActivityStartTrigger(tick)

          agent ! flexOptions
        }

      stay() using baseStateData

      // TODO
      self ! StartCalculationTrigger

      goto(Calculate) using baseStateData

    } else
      stay() using baseStateData
  }
}
