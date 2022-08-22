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
  EmAgentModelBaseStateData,
  FlexCorrespondence
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
import edu.ie3.simona.ontology.messages.FlexibilityMessage._
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
      flexCorrespondences: Map[UUID, ValueStore[
        FlexCorrespondence
      ]], // TODO enhanced todo: MultiValueStore
      actorRefToUuid: Map[ActorRef, UUID],
      connectedAgents: Map[UUID, (SystemParticipantInput, ActorRef)],
      schedulerStateData: EmSchedulerStateData
  ) extends ModelBaseStateData[ApparentPowerAndHeat, EmRelevantData, EmModel] {
    override val modelUuid: UUID = model.getUuid
  }

  final case class FlexCorrespondence(
      receivedFlexOptions: Option[ProvideFlexOptions] = None,
      issuedCtrlMsgs: Option[IssueFlexControl] = None
  ) {
    def hasOptions: Boolean =
      receivedFlexOptions.nonEmpty
  }

  object FlexCorrespondence {
    def apply(flexOptions: ProvideFlexOptions): FlexCorrespondence =
      FlexCorrespondence(
        Some(flexOptions),
        None
      )
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
        connectedAgents.map { case (actor, _, sp) =>
          sp.getUuid -> (sp, actor)
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
        baseStateData.flexCorrespondences
      ) { case (stores, uuid) =>
        val participantValueStore = stores.getOrElse(
          uuid,
          throw new RuntimeException(s"ValueStore for UUID $uuid not found")
        )

        val updatedFlexOptionsStore =
          ValueStore.updateValueStore(
            participantValueStore,
            newTick,
            FlexCorrespondence()
          )

        stores.updated(uuid, updatedFlexOptionsStore)
      }

      val updatedBaseStateData =
        baseStateData.copy(
          flexCorrespondences = updatedReceivedFlexOptions
        )

      // send out flex requests for all expected agents
      expectedActors.foreach { case (actor, _) =>
        actor ! RequestFlexOptions
      }

      // send out all ActivityStartTriggers
      setActiveTickAndSendTriggers(updatedBaseStateData, newTick, triggerId)

    case Event(
          flexOptions: ProvideFlexOptions,
          baseStateData: EmAgentModelBaseStateData
        ) =>
      val tick = baseStateData.schedulerStateData.nowInTicks

      val receivedFlexOptions =
        baseStateData.flexCorrespondences.getOrElse(
          flexOptions.modelUuid,
          throw new RuntimeException(
            s"No received flex options store found for ${flexOptions.modelUuid}"
          )
        )

      val updatedValueStore = ValueStore.updateValueStore(
        receivedFlexOptions,
        tick,
        FlexCorrespondence(flexOptions)
      )

      val updatedReceivedFlexOptions =
        baseStateData.flexCorrespondences.updated(
          flexOptions.modelUuid,
          updatedValueStore
        )

      val updatedBaseStateData = baseStateData.copy(
        flexCorrespondences = updatedReceivedFlexOptions
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
    val flexOptions = baseStateData.flexCorrespondences.map {
      case (uuid, store) =>
        val (spi, actor) = baseStateData.connectedAgents.getOrElse(
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

        (spi, actor, flexOptions, dataTick == tick)
    }

    val flexAnswersReceived = !flexOptions.exists {
      case (_, _, correspondence, true) if !correspondence.hasOptions => true
      case _                                                          => false
    }

    if (flexAnswersReceived) {
      // All flex options and all results have been received.

      val flexStratInput = flexOptions.flatMap {
        case (spi, _, flexOptionsOpt, _) =>
          flexOptionsOpt.receivedFlexOptions.map(spi -> _)
      }

      // TODO sanity checks before strat calculation

      val issueCtrlMsgs = baseStateData.model
        .determineDeviceControl(
          flexStratInput.collect {
            case (spi, flexOption: ProvideMinMaxFlexOptions) =>
              (spi, flexOption)
          }.toSeq
        )

      // TODO sanity checks after strat calculation

      val issueCtrlMsgsComplete = flexOptions.flatMap {
        case (spi, actor, _, activatedBefore) =>
          issueCtrlMsgs
            .find { case (uuid, _) =>
              uuid.equals(spi.getUuid)
            }
            .orElse {
              // if a response is expected for this tick and flexibility
              // should not be used, send a no-control-msg instead
              Option.when(activatedBefore)(spi.getUuid -> IssueNoCtrl)
            }
            .map { case (uuid, flexCtrl) =>
              (uuid, actor, flexCtrl, activatedBefore)
            }
      }

      issueCtrlMsgsComplete.foreach {
        case (_, actor, issueCtrlMsg, activatedBefore) =>
          // activate the participants that have not been activated before
          if (!activatedBefore)
            actor ! ActivityStartTrigger(tick)

          // send out flex control messages
          actor ! issueCtrlMsg
      }

      // create updated value stores for participants that are received control msgs
      val updatedValueStores = issueCtrlMsgsComplete.flatMap {
        case (uuid, _, issueFlex, _) =>
          baseStateData.flexCorrespondences.get(uuid).flatMap { store =>
            store.last().map { case (dataTick, correspondence) =>
              val updatedCorrespondence =
                correspondence.copy(issuedCtrlMsgs = Some(issueFlex))

              uuid -> ValueStore.updateValueStore(
                store,
                dataTick,
                updatedCorrespondence
              )
            }
          }
      }.toMap

      // actually update the value store map
      val updatedCorrespondences = baseStateData.flexCorrespondences.map {
        case (uuid, store) =>
          uuid -> updatedValueStores.getOrElse(uuid, store)
      }

      stay() using baseStateData.copy(
        flexCorrespondences = updatedCorrespondences
      )

      // TODO total power calculation
      self ! StartCalculationTrigger

      goto(Calculate) using baseStateData

    } else
      stay() using baseStateData
  }
}
