/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import akka.actor.{ActorRef, Props}
import edu.ie3.datamodel.models.input.system.{EmInput, SystemParticipantInput}
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.ParticipantAgent.getAndCheckNodalVoltage
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.em.EmAgent.{
  EmAgentInitializeStateData,
  EmModelBaseStateData,
  FlexCorrespondence
}
import edu.ie3.simona.agent.participant.em.EmSchedulerStateData.TriggerData
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantUninitializedStateData
import edu.ie3.simona.agent.participant.statedata.{
  BaseStateData,
  InitializeStateData
}
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.exceptions.agent.InconsistentStateException
import edu.ie3.simona.model.participant.EmModel
import edu.ie3.simona.model.participant.EmModel.EmRelevantData
import edu.ie3.simona.ontology.messages.FlexibilityMessage._
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
import edu.ie3.simona.util.TickUtil.RichZonedDateTime
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
  ) extends InitializeStateData[ApparentPower]

  final case class EmModelBaseStateData(
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
      resultValueStore: ValueStore[ApparentPower],
      requestValueStore: ValueStore[ApparentPower],
      calcRelevantDateStore: ValueStore[EmRelevantData],
      flexCorrespondences: Map[UUID, ValueStore[
        FlexCorrespondence
      ]], // TODO enhanced todo: MultiValueStore
      actorRefToUuid: Map[ActorRef, UUID],
      connectedAgents: Map[UUID, (SystemParticipantInput, ActorRef)],
      schedulerStateData: EmSchedulerStateData
  ) extends ModelBaseStateData[ApparentPower, EmRelevantData, EmModel] {
    override val modelUuid: UUID = model.getUuid
  }

  final case class FlexCorrespondence(
      receivedFlexOptions: Option[ProvideFlexOptions] = None,
      issuedCtrlMsg: Option[IssueFlexControl] = None,
      participantResult: Option[SystemParticipantResult] = None
  ) {
    def hasOptions: Boolean =
      receivedFlexOptions.nonEmpty

    def hasResults: Boolean =
      participantResult.nonEmpty
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
      ApparentPower,
      EmRelevantData,
      EmModelBaseStateData,
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
          _: ParticipantUninitializedStateData[ApparentPower]
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

      val model = buildModel(
        inputModel,
        modelConfig,
        simulationStartDate,
        simulationEndDate
      )

      val baseStateData = EmModelBaseStateData(
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
          baseStateData: EmModelBaseStateData
        ) =>
      stay() using
        baseStateData.copy(schedulerStateData =
          sendEligibleTrigger(
            scheduleTrigger(triggerToSchedule, baseStateData.schedulerStateData)
          )
        )

    case Event(
          completionMessage: CompletionMessage,
          baseStateData: EmModelBaseStateData
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
          baseStateData: EmModelBaseStateData
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

      // send out all ActivityStartTriggers
      val nextState =
        setActiveTickAndSendTriggers(updatedBaseStateData, newTick, triggerId)

      // send out flex requests for all expected agents
      expectedActors.foreach { case (actor, _) =>
        actor ! RequestFlexOptions
      }

      nextState

    case Event(
          flexOptions: ProvideFlexOptions,
          baseStateData: EmModelBaseStateData
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

    case Event(
          ParticipantResultEvent(result),
          baseStateData: EmModelBaseStateData
        ) =>
      implicit val startDate: ZonedDateTime = baseStateData.startDate
      val tick = baseStateData.schedulerStateData.nowInTicks

      val receivedFlexOptions =
        baseStateData.flexCorrespondences.getOrElse(
          result.getInputModel,
          throw new RuntimeException(
            s"No received flex options store found for ${result.getInputModel}"
          )
        )

      val resultTick = result.getTime.toTick
      val flexCorrespondence = receivedFlexOptions
        .get(resultTick)
        .getOrElse(
          throw new RuntimeException(
            s"No flex correspondence found for model ${result.getInputModel} and tick $resultTick"
          )
        )

      val updatedValueStore = ValueStore.updateValueStore(
        receivedFlexOptions,
        tick,
        flexCorrespondence.copy(participantResult = Some(result))
      )

      val updatedReceivedFlexOptions =
        baseStateData.flexCorrespondences.updated(
          result.getInputModel,
          updatedValueStore
        )

      val updatedBaseStateData = baseStateData.copy(
        flexCorrespondences = updatedReceivedFlexOptions
      )

      // check if all results received
      val flexData = extractFlexData(updatedBaseStateData)
      val resultsReceived = !flexData.exists {
        case (_, _, correspondence, dataTick)
            if dataTick == tick && !correspondence.hasResults =>
          true
        case _ => false
      }

      if (resultsReceived)
        calculatePower(
          updatedBaseStateData,
          scheduler
        )
      else
        stay() using updatedBaseStateData
  }

  when(Uninitialized) { handleUnitializedEm orElse handleUnitialized }

  when(Idle) { handleIdleEm orElse handleIdle }

  private def setActiveTickAndSendTriggers(
      baseStateData: EmModelBaseStateData,
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
      baseStateData: EmModelBaseStateData
  ): State = {
    val tick = baseStateData.schedulerStateData.nowInTicks

    val flexData = extractFlexData(baseStateData)

    // check if expected flex answers for this tick arrived
    val flexAnswersReceived = !flexData.exists {
      case (_, _, correspondence, dataTick)
          if dataTick == tick && !correspondence.hasOptions =>
        true
      case _ => false
    }

    if (flexAnswersReceived) {
      // All flex options and all results have been received.

      val flexStratInput = flexData.flatMap {
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

      val issueCtrlMsgsComplete = flexData.flatMap {
        case (spi, actor, correspondence, dataTick) =>
          issueCtrlMsgs
            .find { case (uuid, _) =>
              uuid.equals(spi.getUuid)
            }
            .orElse {
              // no power ctrl message has been set for this participant.
              // still send a no-control-msg instead, if...

              // ... a response is expected for this tick
              val currentlyActivated = dataTick == tick

              // ... flex control has been issued for this participant at
              // an earlier tick
              val flexControlCancelled =
                dataTick < tick && (correspondence.issuedCtrlMsg match {
                  case Some(_: IssuePowerCtrl) => true
                  case _                       => false
                })

              Option.when(currentlyActivated || flexControlCancelled)(
                spi.getUuid -> IssueNoCtrl
              )
            }
            .map { case (uuid, flexCtrl) =>
              (uuid, actor, flexCtrl, dataTick)
            }
      }

      issueCtrlMsgsComplete.foreach { case (_, actor, issueCtrlMsg, dataTick) =>
        // activate the participants that have not been activated before
        if (dataTick != tick)
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
                correspondence.copy(issuedCtrlMsg = Some(issueFlex))

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

      val updatedBaseStateData = baseStateData.copy(
        flexCorrespondences = updatedCorrespondences
      )

      stay() using updatedBaseStateData

    } else
      stay() using baseStateData
  }

  private def extractFlexData(baseStateData: EmModelBaseStateData): Iterable[
    (SystemParticipantInput, ActorRef, FlexCorrespondence, Long)
  ] =
    baseStateData.flexCorrespondences.map { case (uuid, store) =>
      val (spi, actor) = baseStateData.connectedAgents.getOrElse(
        uuid,
        throw new RuntimeException(
          s"There's no flex options store for $uuid whatsoever"
        )
      )
      val (dataTick, correspondence) = store
        .last()
        .getOrElse(
          throw new RuntimeException(
            s"There's no expected flex options for $spi whatsoever"
          )
        )

      (spi, actor, correspondence, dataTick)
    }

  private def calculatePower(
      baseStateData: EmModelBaseStateData,
      scheduler: ActorRef
  ): State = {
    val correspondences = baseStateData.flexCorrespondences
      .flatMap { case (_, store) =>
        store.last().map { case (_, correspondence) =>
          correspondence
        }
      }

    val voltage =
      getAndCheckNodalVoltage(
        baseStateData,
        baseStateData.schedulerStateData.nowInTicks
      )

    val relevantData = EmRelevantData(correspondences)

    val result = baseStateData.model.calculatePower(
      baseStateData.schedulerStateData.nowInTicks,
      voltage,
      relevantData
    )

    updateValueStoresInformListeners(
      scheduler,
      baseStateData,
      result,
      relevantData
    )
  }

  /** TODO consolidate with
    * ParticipantAgentFundamentals#updateValueStoresInformListenersAndGoToIdleWithUpdatedBaseStateData(akka.actor.ActorRef,
    * edu.ie3.simona.agent.participant.statedata.BaseStateData,
    * java.lang.Object, java.lang.Object)
    *
    * Update the result and calc relevant data value stores, inform all
    * registered listeners and go to Idle using the updated base state data
    *
    * @param scheduler
    *   Actor reference of the scheduler
    * @param baseStateData
    *   The base state data of the collection state
    * @param result
    *   Result of simulation
    * @param relevantData
    *   Data, that have been relevant to this calculation
    * @return
    *   Desired state change
    */
  final def updateValueStoresInformListeners(
      scheduler: ActorRef,
      baseStateData: EmModelBaseStateData,
      result: ApparentPower,
      relevantData: EmRelevantData
  ): State = {
    /* Update the value stores */
    val updatedValueStore =
      ValueStore.updateValueStore(
        baseStateData.resultValueStore,
        baseStateData.schedulerStateData.nowInTicks,
        result
      )
    val updatedRelevantDataStore =
      baseStateData match {
        case data: BaseStateData.ModelBaseStateData[_, _, _] =>
          ValueStore.updateValueStore(
            data.calcRelevantDateStore,
            baseStateData.schedulerStateData.nowInTicks,
            relevantData
          )
        case _ =>
          throw new InconsistentStateException(
            "Cannot find calculation relevant data to update."
          )
      }

    /* Update the base state data */
    val baseStateDateWithUpdatedResults =
      baseStateData match {
        case data: EmModelBaseStateData =>
          data.copy(
            resultValueStore = updatedValueStore,
            calcRelevantDateStore = updatedRelevantDataStore
          )
        case _ =>
          throw new InconsistentStateException(
            "Wrong base state data"
          )
      }

    /* Inform the listeners about current result
     * (IN CONTRAST TO REGULAR SPA BEHAVIOR, WHERE WE WAIT FOR POTENTIALLY CHANGED VOLTAGE) */
    announceSimulationResult(
      baseStateDateWithUpdatedResults,
      baseStateDateWithUpdatedResults.schedulerStateData.nowInTicks,
      result
    )(baseStateDateWithUpdatedResults.outputConfig)

    // we don't send the completion message here, as this is part
    // of the EmScheduler
    unstashAll()
    goto(Idle) using baseStateDateWithUpdatedResults
  }
}
