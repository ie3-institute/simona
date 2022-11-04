/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import akka.actor.{ActorRef, Props, ReceiveTimeout}
import edu.ie3.datamodel.models.input.system.{EmInput, SystemParticipantInput}
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.ParticipantAgent.getAndCheckNodalVoltage
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ApparentPower,
  PrimaryDataWithApparentPower
}
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.em.EmAgent.{
  EmAgentInitializeStateData,
  EmModelBaseStateData,
  FlexCorrespondence
}
import edu.ie3.simona.agent.participant.em.EmSchedulerStateData.TriggerData
import edu.ie3.simona.agent.participant.statedata.BaseStateData.{
  FlexStateData,
  ModelBaseStateData,
  ParticipantModelBaseStateData
}
import edu.ie3.simona.agent.participant.statedata.{
  BaseStateData,
  InitializeStateData,
  ParticipantStateData
}
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.{
  ParticipantUninitializedStateData,
  SimpleInputContainer
}
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.agent.InconsistentStateException
import edu.ie3.simona.io.result.AccompaniedSimulationResult
import edu.ie3.simona.model.participant.{CalcRelevantData, EmModel}
import edu.ie3.simona.model.participant.EmModel.EmRelevantData
import edu.ie3.simona.model.participant.ModelState.ConstantState
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
import javax.measure.quantity.{Dimensionless, Power}
import scala.concurrent.duration.{Duration, DurationInt}
import scala.language.postfixOps

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
      outputConfig: NotifierConfig,
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
      outputConfig: NotifierConfig,
      additionalActivationTicks: Array[Long],
      foreseenDataTicks: Map[ActorRef, Option[Long]],
      requestVoltageDeviationThreshold: Double,
      calcRelevantDateStore: ValueStore[CalcRelevantData],
      voltageValueStore: ValueStore[
        ComparableQuantity[Dimensionless]
      ],
      resultValueStore: ValueStore[ApparentPower],
      requestValueStore: ValueStore[ApparentPower],
      receivedSecondaryDataStore: ValueStore[Map[ActorRef, _ <: SecondaryData]],
      flexCorrespondences: Map[UUID, ValueStore[
        FlexCorrespondence
      ]], // TODO enhanced todo: MultiValueStore
      participantInput: Map[UUID, SystemParticipantInput],
      schedulerStateData: EmSchedulerStateData,
      stateDataStore: ValueStore[ConstantState.type],
      flexStateData: Option[FlexStateData]
  ) extends ModelBaseStateData[
        ApparentPower,
        EmRelevantData,
        ConstantState.type,
        EmModel
      ] {
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
      ConstantState.type,
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
      context.setReceiveTimeout(2 minutes)

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
        SimpleInputContainer(inputModel),
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
        ValueStore(resolution * 10),
        ValueStore.forVoltage(
          resolution * 10,
          inputModel.getNode
            .getvTarget()
            .to(PU)
        ),
        ValueStore.forResult(resolution, 10),
        ValueStore(resolution * 10),
        ValueStore(0),
        connectedAgents.map { case (_, _, sp) =>
          sp.getUuid -> ValueStore(resolution * 10)
        }.toMap,
        connectedAgents.map { case (_, _, sp) =>
          sp.getUuid -> sp
        }.toMap,
        EmSchedulerStateData(
          triggerData,
          connectedAgents.map { case (actor, _, sp) =>
            sp.getUuid -> actor
          }.toMap
        ),
        ValueStore(0),
        None
      )

      val updatedBaseStateData = setActiveTickAndSendTriggers(
        baseStateData,
        SimonaConstants.INIT_SIM_TICK,
        triggerId
      )

      goto(Idle) using updatedBaseStateData
  }

  private val handleIdleEm: StateFunction = {
    case Event(
          scheduleTriggerMessage: ScheduleTriggerMessage,
          baseStateData: EmModelBaseStateData
        ) =>
      createNextTriggerIfApplicable(
        baseStateData.schedulerStateData,
        scheduleTriggerMessage.trigger.tick
      ) foreach (scheduler ! _)

      stay() using
        baseStateData.copy(schedulerStateData =
          sendEligibleTrigger(
            scheduleTrigger(
              scheduleTriggerMessage,
              baseStateData.schedulerStateData
            )
          )
        )

    case Event(
          TriggerWithIdMessage(
            scheduleTriggerMessage: ScheduleTriggerMessage,
            triggerId,
            _
          ),
          baseStateData: EmModelBaseStateData
        ) =>
      val maybeNextTrigger = createNextTriggerIfApplicable(
        baseStateData.schedulerStateData,
        scheduleTriggerMessage.trigger.tick
      )

      // since we've been sent a trigger, we need to complete it as well
      scheduler ! CompletionMessage(triggerId, maybeNextTrigger.map(Seq(_)))

      stay() using
        baseStateData.copy(schedulerStateData =
          sendEligibleTrigger(
            scheduleTrigger(
              scheduleTriggerMessage,
              baseStateData.schedulerStateData
            )
          )
        )

    case Event(
          completionMessage: CompletionMessage,
          baseStateData: EmModelBaseStateData
        ) =>
      // there can be new triggers for the current tick, which need to be sent out immediately
      val updatedSchedulerData =
        maybeTicksCompleted(
          sendEligibleTrigger(
            handleCompletionMessage(
              completionMessage,
              baseStateData.schedulerStateData
            )
          )
        )

      stay() using baseStateData.copy(schedulerStateData = updatedSchedulerData)

    case Event(
          flexCompletion: FlexCtrlCompletion,
          baseStateData: EmModelBaseStateData
        ) =>
      val updatedSchedulerData = maybeTicksCompleted(
        handleFlexCompletionMessage(
          flexCompletion,
          baseStateData.schedulerStateData
        )
      )
      stay() using baseStateData.copy(schedulerStateData = updatedSchedulerData)

    case Event(
          TriggerWithIdMessage(ActivityStartTrigger(newTick), triggerId, _),
          baseStateData: EmModelBaseStateData
        ) =>
      // here, participants that are changing their flex options at the current
      // tick are activated and are sent flex options requests

      if (baseStateData.schedulerStateData.mainTriggerId.nonEmpty) {
        log.error(
          s"EmAgent $self is already active at $newTick with ${baseStateData.schedulerStateData.mainTriggerId} (new triggerId $triggerId)"
        )

        scheduler ! CompletionMessage(triggerId, None)
        stay() using baseStateData

      } else {

        // schedule flex options request for those agents that need to be activated at the next activated tick
        val schedulerDataWithNext =
          scheduleFlexRequestAtNextTick(
            baseStateData.schedulerStateData,
            newTick
          )

        // participants that have to be activated at this specific tick
        val expectedActivations =
          schedulerDataWithNext.trigger.triggerQueue
            .get(newTick)
            .map {
              _.map(_.agent)
                .flatMap(actor =>
                  baseStateData.schedulerStateData.flexTrigger.actorRefToUuid
                    .get(actor)
                )
            }
            .getOrElse(Seq.empty)

        // schedule flex options request for those agents that have just scheduled activations so far
        val updatedFlexTrigger = scheduleFlexRequestsOnce(
          schedulerDataWithNext.flexTrigger,
          expectedActivations.toSet,
          newTick
        )

        val expectedRequests = updatedFlexTrigger.triggerQueue
          .get(newTick)
          .map {
            _.map(_.modelUuid)
          }
          .getOrElse(Seq.empty)

        // prepare map for expected flex options and expected results for this tick
        val updatedFlexCorrespondences = expectedRequests.foldLeft(
          baseStateData.flexCorrespondences
        ) { case (correspondences, uuid) =>
          val participantValueStore = correspondences.getOrElse(
            uuid,
            throw new RuntimeException(s"ValueStore for UUID $uuid not found")
          )

          // add a fresh flex correspondence for the new tick
          val updatedFlexOptionsStore =
            ValueStore.updateValueStore(
              participantValueStore,
              newTick,
              FlexCorrespondence()
            )

          correspondences.updated(uuid, updatedFlexOptionsStore)
        }

        val updatedBaseStateData =
          baseStateData.copy(
            flexCorrespondences = updatedFlexCorrespondences,
            schedulerStateData =
              schedulerDataWithNext.copy(flexTrigger = updatedFlexTrigger)
          )

        // we should not get triggered without any scheduled triggers for the new tick
        if (expectedRequests.isEmpty)
          log.error(s"No requests for $self at $newTick")

        // send out all ActivityStartTriggers and RequestFlexOptions
        goto(Idle) using setActiveTickAndSendTriggers(
          updatedBaseStateData,
          newTick,
          triggerId
        )
      }

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
      val (_, flexCorrespondence) = receivedFlexOptions
        .last(resultTick)
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
        case (_, correspondence, dataTick)
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

    case Event(ReceiveTimeout, stateData) =>
      // disable timeout again
      context.setReceiveTimeout(Duration.Undefined)
      log.warning(
        "No messages received for two minutes. Current state data: " + stateData
      )
      stay()
  }

  when(Uninitialized) { handleUnitializedEm orElse handleUnitialized }

  when(Idle) { handleIdleEm orElse handleIdle }

  private def setActiveTickAndSendTriggers(
      baseStateData: EmModelBaseStateData,
      newTick: Long,
      triggerId: Long
  ): EmModelBaseStateData = {
    val updatedStateData = baseStateData.schedulerStateData.copy(
      nowInTicks = newTick,
      mainTriggerId = Some(triggerId)
    )

    baseStateData.copy(schedulerStateData =
      sendEligibleTrigger(updatedStateData)
    )
  }

  protected def createNextTriggerIfApplicable(
      schedulerStateData: EmSchedulerStateData,
      newTick: Long
  ): Option[ScheduleTriggerMessage] = {
    val isCurrentlyInactive = schedulerStateData.mainTriggerId.isEmpty

    val maybeNextScheduledTick = getNextScheduledTick(
      schedulerStateData
    )

    // only revoke next scheduled tick if it exists and is later than new tick
    val maybeTickToRevoke = maybeNextScheduledTick.filter { nextScheduledTick =>
      newTick < nextScheduledTick
    }

    // schedule new tick if we're inactive and
    //   - there is no scheduled next tick or
    //   - the new tick is earlier than the scheduled next tick
    val scheduleNewTick =
      isCurrentlyInactive && (maybeNextScheduledTick.isEmpty || maybeTickToRevoke.nonEmpty)

    Option.when(scheduleNewTick) {
      val maybeRevokeTrigger =
        maybeTickToRevoke.map(revokeTick =>
          (ActivityStartTrigger(revokeTick), self)
        )

      ScheduleTriggerMessage(
        ActivityStartTrigger(newTick),
        self,
        maybeRevokeTrigger
      )
    }

  }

  private def maybeIssueFlexControl(
      baseStateData: EmModelBaseStateData
  ): State = {
    val tick = baseStateData.schedulerStateData.nowInTicks

    val flexData = extractFlexData(baseStateData)

    // check if expected flex answers for this tick arrived
    val flexAnswersReceived = !flexData.exists {
      case (_, correspondence, dataTick)
          if dataTick == tick && !correspondence.hasOptions =>
        true
      case _ => false
    }

    if (flexAnswersReceived) {
      // All flex options and all results have been received.

      val flexStratInput = flexData.flatMap { case (spi, correspondence, _) =>
        correspondence.receivedFlexOptions.map(spi -> _)
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
        case (spi, correspondence, dataTick) =>
          issueCtrlMsgs
            .find { case (uuid, _) =>
              uuid.equals(spi.getUuid)
            }
            .map { case (uuid, power) =>
              uuid -> IssuePowerCtrl(tick, power)
            }
            .orElse {
              // no power ctrl message has been set for this participant.
              // still send a no-control-msg instead, if...

              // ... a response is expected for this tick
              val currentlyRequested = dataTick == tick

              // ... flex control has been issued for this participant at
              // an earlier tick
              val flexControlCancelled =
                dataTick < tick && (correspondence.issuedCtrlMsg match {
                  case Some(_: IssuePowerCtrl) => true
                  case _                       => false
                })

              Option.when(currentlyRequested || flexControlCancelled)(
                spi.getUuid -> IssueNoCtrl(tick)
              )
            }
            .map { case (uuid, flexCtrl) =>
              (uuid, flexCtrl, dataTick)
            }
      }

      val updatedFlexTrigger = issueCtrlMsgsComplete.foldLeft(
        baseStateData.schedulerStateData.flexTrigger
      ) { case (flexTrigger, (uuid, issueCtrlMsg, _)) =>
        // send out flex control messages
        scheduleFlexTrigger(flexTrigger, issueCtrlMsg, uuid)
      }

      val updatedScheduledStateData = sendEligibleTrigger(
        baseStateData.schedulerStateData.copy(
          flexTrigger = updatedFlexTrigger
        )
      )

      // create updated value stores for participants that are receiving control msgs
      val updatedValueStores = issueCtrlMsgsComplete.flatMap {
        case (uuid, issueFlex, _) =>
          baseStateData.flexCorrespondences.get(uuid).flatMap { store =>
            store.last().map { case (_, correspondence) =>
              // since we expect a new result with potentially changed reactive power, empty the last result
              val updatedCorrespondence =
                correspondence.copy(
                  issuedCtrlMsg = Some(issueFlex),
                  participantResult = None
                )

              // save for current tick
              uuid -> ValueStore.updateValueStore(
                store,
                tick,
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
        flexCorrespondences = updatedCorrespondences,
        schedulerStateData = updatedScheduledStateData
      )

      stay() using updatedBaseStateData

    } else
      stay() using baseStateData
  }

  private def extractFlexData(baseStateData: EmModelBaseStateData): Iterable[
    (SystemParticipantInput, FlexCorrespondence, Long)
  ] =
    baseStateData.flexCorrespondences.map { case (uuid, store) =>
      val spi = baseStateData.participantInput.getOrElse(
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

      (spi, correspondence, dataTick)
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

    val tick = baseStateData.schedulerStateData.nowInTicks

    val result = baseStateData.model.calculatePower(
      tick,
      voltage,
      ConstantState,
      relevantData
    )

    updateValueStoresInformListeners(
      scheduler,
      baseStateData,
      result
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
    * @return
    *   Desired state change
    */
  final def updateValueStoresInformListeners(
      scheduler: ActorRef,
      baseStateData: EmModelBaseStateData,
      result: ApparentPower
  ): State = {
    /* Update the value stores */
    val updatedValueStore =
      ValueStore.updateValueStore(
        baseStateData.resultValueStore,
        baseStateData.schedulerStateData.nowInTicks,
        result
      )

    /* Update the base state data */
    val baseStateDateWithUpdatedResults =
      baseStateData match {
        case data: EmModelBaseStateData =>
          data.copy(
            resultValueStore = updatedValueStore
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
      AccompaniedSimulationResult(result)
    )(baseStateDateWithUpdatedResults.outputConfig)

    // we don't send the completion message here, as this is part
    // of the EmScheduler
    unstashAll()
    goto(Idle) using baseStateDateWithUpdatedResults
  }
}