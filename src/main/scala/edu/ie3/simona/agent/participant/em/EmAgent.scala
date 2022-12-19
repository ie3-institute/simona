/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import akka.actor.{ActorRef, Props, ReceiveTimeout}
import edu.ie3.datamodel.models.input.system.{EmInput, SystemParticipantInput}
import edu.ie3.datamodel.models.result.system.FlexOptionsResult
import edu.ie3.datamodel.models.timeseries.individual.IndividualTimeSeries
import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.ParticipantAgent.getAndCheckNodalVoltage
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.em.EmAgent._
import edu.ie3.simona.agent.participant.em.EmSchedulerStateData.TriggerData
import edu.ie3.simona.agent.participant.em.FlexCorrespondenceStore.FlexCorrespondence
import edu.ie3.simona.agent.participant.statedata.BaseStateData.{
  FlexStateData,
  ModelBaseStateData
}
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantUninitializedStateData
import edu.ie3.simona.agent.participant.statedata.{
  InitializeStateData,
  ParticipantStateData
}
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.event.ResultEvent.{
  FlexOptionsResultEvent,
  ParticipantResultEvent
}
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.io.result.AccompaniedSimulationResult
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.em.EmModel.EmRelevantData
import edu.ie3.simona.model.participant.em.{
  EmAggregateFlex,
  EmModel,
  EmModelStrat
}
import edu.ie3.simona.ontology.messages.FlexibilityMessage._
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.trigger.Trigger
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeParticipantAgentTrigger
}
import edu.ie3.simona.util.SimonaConstants
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.PowerSystemUnits.PU
import edu.ie3.util.scala.io.FlexSignalFromExcel
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}
import scala.collection.SortedSet
import scala.compat.java8.OptionConverters.RichOptionalGeneric
import scala.concurrent.duration.{Duration, DurationInt}
import scala.jdk.CollectionConverters._
import scala.language.postfixOps
import scala.util.{Failure, Success}

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
      modelStrategy: EmModelStrat,
      connectedAgents: Seq[
        (
            ActorRef,
            InitializeParticipantAgentTrigger[_, _],
            SystemParticipantInput
        )
      ],
      maybeParentEmAgent: Option[ActorRef] = None,
      maybeRootEmConfig: Option[SimonaConfig.Simona.Runtime.RootEm] = None,
      aggregateFlex: EmAggregateFlex
  ) extends InitializeStateData[ApparentPower]

  final case class EmModelBaseStateData(
      startDate: ZonedDateTime,
      endDate: ZonedDateTime,
      model: EmModel,
      services: Option[
        Vector[SecondaryDataService[_ <: SecondaryData]]
      ],
      outputConfig: NotifierConfig,
      additionalActivationTicks: SortedSet[Long],
      foreseenDataTicks: Map[ActorRef, Option[Long]],
      requestVoltageDeviationThreshold: Double,
      voltageValueStore: ValueStore[
        ComparableQuantity[Dimensionless]
      ],
      resultValueStore: ValueStore[ApparentPower],
      requestValueStore: ValueStore[ApparentPower],
      receivedSecondaryDataStore: ValueStore[Map[ActorRef, _ <: SecondaryData]],
      flexCorrespondences: FlexCorrespondenceStore,
      participantInput: Map[UUID, SystemParticipantInput],
      schedulerStateData: EmSchedulerStateData,
      stateDataStore: ValueStore[ConstantState.type],
      flexStateData: Option[FlexStateData],
      flexTimeSeries: Option[FlexTimeSeries],
      aggregateFlex: EmAggregateFlex
  ) extends ModelBaseStateData[
        ApparentPower,
        EmRelevantData,
        ConstantState.type,
        EmModel
      ] {
    override val modelUuid: UUID = model.getUuid
  }

  case class FlexTimeSeries(
      timeSeries: IndividualTimeSeries[PValue],
      resolutionHours: Int,
      minValue: ComparableQuantity[Power],
      maxValue: ComparableQuantity[Power],
      threshold: Double
  )

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
                modelStrategy,
                connectedAgents,
                maybeParentEmAgent,
                maybeRootEmConfig,
                aggregateFlex
              )
            ),
            triggerId,
            _
          ),
          _: ParticipantUninitializedStateData[ApparentPower]
        ) =>
      context.setReceiveTimeout(10 minutes)

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
        simulationEndDate,
        modelStrategy
      )

      val maybeFlexTimeseries = maybeRootEmConfig.map { config =>
        val timeSeriesType =
          FlexSignalFromExcel.TimeSeriesType(config.timeSeriesType)
        val timeSeries = FlexSignalFromExcel
          .flexSignals(config.filePath, config.nodeId, timeSeriesType) match {
          case Success(timeSeries) => timeSeries
          case Failure(exception)  => throw exception
        }

        val resolutionHours =
          if (timeSeries.getEntries.size() < 2)
            throw new RuntimeException(
              s"Less than two entries for flex time series ${config.nodeId}"
            )
          else {
            val valueIt = timeSeries.getEntries.iterator()
            val entry1 = valueIt.next().getTime
            val entry2 = valueIt.next().getTime

            ChronoUnit.HOURS.between(entry1, entry2).intValue
          }

        val allValues =
          timeSeries.getEntries.asScala.flatMap(_.getValue.getP.asScala)
        val maybeMinValue = allValues.minByOption(
          _.to(PowerSystemUnits.MEGAWATT).getValue.doubleValue
        )
        val maybeMaxValue = allValues.maxByOption(
          _.to(PowerSystemUnits.MEGAWATT).getValue.doubleValue
        )

        val (minValue, maxValue) = maybeMinValue
          .zip(maybeMaxValue)
          .getOrElse(
            throw new RuntimeException(s"Time series for $config is empty")
          )

        FlexTimeSeries(
          timeSeries,
          resolutionHours,
          minValue,
          maxValue,
          config.threshold
        )
      }

      val baseStateData = EmModelBaseStateData(
        simulationStartDate,
        simulationEndDate,
        model,
        services,
        outputConfig,
        SortedSet.empty,
        Map.empty,
        requestVoltageDeviationThreshold,
        ValueStore.forVoltage(
          resolution,
          inputModel.getNode
            .getvTarget()
            .to(PU)
        ),
        ValueStore(resolution),
        ValueStore(resolution),
        ValueStore(0),
        FlexCorrespondenceStore(
          connectedAgents.map { case (_, _, sp) =>
            sp.getUuid
          }.toSet,
          resolution,
          simulationStartDate
        ),
        connectedAgents.map { case (_, _, sp) =>
          sp.getUuid -> sp
        }.toMap,
        EmSchedulerStateData(
          triggerData,
          connectedAgents.map { case (actor, _, sp) =>
            sp.getUuid -> actor
          }.toMap,
          tick => createActivationTrigger(tick, maybeParentEmAgent.isDefined)
        ),
        ValueStore(0),
        maybeParentEmAgent.map(FlexStateData(_, ValueStore(resolution))),
        maybeFlexTimeseries,
        aggregateFlex
      )

      val updatedBaseStateData = setActiveTickAndSendTriggers(
        baseStateData,
        SimonaConstants.INIT_SIM_TICK,
        triggerId
      )

      goto(Idle) using updatedBaseStateData

    case Event(ReceiveTimeout, stateData) =>
      handleReceiveTimeout(stateData)
  }

  private val handleIdleEm: StateFunction = {
    case Event(
          scheduleTriggerMessage: ScheduleTriggerMessage,
          baseStateData: EmModelBaseStateData
        ) =>
      if (baseStateData.flexStateData.isEmpty) {
        // we only need this if we're not EM-controlled
        createNextTriggerIfApplicable(
          baseStateData.schedulerStateData,
          scheduleTriggerMessage.trigger.tick
        ) foreach (scheduler ! _)
      }

      stay() using
        baseStateData.copy(
          schedulerStateData = sendEligibleTrigger(
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
      val maybeNextTrigger = if (baseStateData.flexStateData.isEmpty) {
        createNextTriggerIfApplicable(
          baseStateData.schedulerStateData,
          scheduleTriggerMessage.trigger.tick
        )
      } else None

      // since we've been sent a trigger, we need to complete it as well
      scheduler ! CompletionMessage(triggerId, maybeNextTrigger.map(Seq(_)))

      stay() using
        baseStateData.copy(
          schedulerStateData = sendEligibleTrigger(
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
      val (updatedSchedulerData, scheduledTick) =
        maybeTicksCompleted(
          sendEligibleTrigger(
            handleCompletionMessage(
              completionMessage,
              baseStateData.schedulerStateData
            )
          ),
          baseStateData.modelUuid,
          baseStateData.flexStateData.flatMap(_.scheduledRequest)
        )

      stay() using baseStateData.copy(
        schedulerStateData = updatedSchedulerData,
        flexStateData = baseStateData.flexStateData.map(
          _.copy(scheduledRequest = scheduledTick)
        )
      )

    case Event(
          flexCompletion: FlexCtrlCompletion,
          baseStateData: EmModelBaseStateData
        ) =>
      val (updatedSchedulerData, scheduledTick) = maybeTicksCompleted(
        handleFlexCompletionMessage(
          flexCompletion,
          baseStateData.schedulerStateData
        ),
        baseStateData.modelUuid,
        baseStateData.flexStateData.flatMap(_.scheduledRequest)
      )

      stay() using baseStateData.copy(
        schedulerStateData = updatedSchedulerData,
        flexStateData = baseStateData.flexStateData.map(
          _.copy(scheduledRequest = scheduledTick)
        )
      )

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

        val updatedBaseStateData = handleActivation(newTick, baseStateData)

        // send out all ActivityStartTriggers and RequestFlexOptions
        stay() using setActiveTickAndSendTriggers(
          updatedBaseStateData,
          newTick,
          triggerId
        )
      }

    case Event(
          RequestFlexOptions(newTick),
          baseStateData: EmModelBaseStateData
        ) =>
      // sent by parent EmAgent

      val updatedBaseStateData = handleActivation(newTick, baseStateData)

      stay() using updatedBaseStateData.copy(
        schedulerStateData = sendEligibleTrigger(
          updatedBaseStateData.schedulerStateData.copy(
            nowInTicks = newTick
          )
        )
      )

    case Event(
          flexCtrl: IssueFlexControl,
          baseStateData: EmModelBaseStateData
        ) =>
      // update tick since we could be activated without prior request for flex options
      val updatedBaseStateData = baseStateData.copy(
        schedulerStateData = baseStateData.schedulerStateData.copy(
          nowInTicks = flexCtrl.tick
        )
      )

      val flexParticipantData = updatedBaseStateData.flexStateData.getOrElse(
        throw new RuntimeException(
          s"EmAgent ${updatedBaseStateData.modelUuid} is not EM-controlled."
        )
      )

      val (_, flexOptions) = flexParticipantData.flexOptionsStore
        .last()
        .getOrElse(
          throw new RuntimeException(
            s"Flex options have not been calculated by agent ${updatedBaseStateData.modelUuid}"
          )
        )

      val resultingFlexPower = determineResultingFlexPower(
        flexOptions,
        flexCtrl
      )

      val flexData = updatedBaseStateData.flexCorrespondences.latestFlexData(
        updatedBaseStateData.participantInput
      )

      // calc power control per connected agent
      val finalStateData = determineAndSchedulePowerControl(
        updatedBaseStateData,
        flexData,
        resultingFlexPower
      )

      stay() using finalStateData

    case Event(
          flexOptions: ProvideFlexOptions,
          baseStateData: EmModelBaseStateData
        ) =>
      val tick = baseStateData.schedulerStateData.nowInTicks

      val updatedCorrespondences =
        baseStateData.flexCorrespondences.addReceivedFlexOptions(
          tick,
          flexOptions
        )

      val updatedBaseStateData = baseStateData.copy(
        flexCorrespondences = updatedCorrespondences
      )

      handleFlexProvision(updatedBaseStateData)

    case Event(
          ParticipantResultEvent(result),
          baseStateData: EmModelBaseStateData
        ) =>
      val updatedCorrespondences =
        baseStateData.flexCorrespondences.addReceivedResult(result)

      val updatedBaseStateData = baseStateData.copy(
        flexCorrespondences = updatedCorrespondences
      )

      // check if all results received
      val resultsReceived = updatedBaseStateData.flexCorrespondences.isComplete

      stay() using {
        if (resultsReceived)
          calculatePower(
            updatedBaseStateData.copy(
              flexCorrespondences =
                updatedBaseStateData.flexCorrespondences.setReceiveComplete()
            ),
            scheduler
          )
        else
          updatedBaseStateData
      }

    case Event(ReceiveTimeout, stateData) =>
      handleReceiveTimeout(stateData)
  }

  when(Uninitialized) { handleUnitializedEm orElse handleUnitialized }

  when(Idle) { handleIdleEm orElse handleIdle }

  private def handleActivation(
      newTick: Long,
      baseStateData: EmModelBaseStateData
  ): EmModelBaseStateData = {

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

    // prepare correspondences for this tick
    val updatedCorrespondences =
      baseStateData.flexCorrespondences.setExpectingFlexOptions(
        expectedRequests.toSet,
        newTick
      )

    // we should not get triggered without any scheduled triggers for the new tick
    if (expectedRequests.isEmpty)
      throw new RuntimeException(
        s"No requests at tick $newTick for ${baseStateData.modelUuid}."
      )

    baseStateData.copy(
      flexCorrespondences = updatedCorrespondences,
      schedulerStateData =
        schedulerDataWithNext.copy(flexTrigger = updatedFlexTrigger)
    )
  }

  private def setActiveTickAndSendTriggers(
      baseStateData: EmModelBaseStateData,
      newTick: Long,
      triggerId: Long
  ): EmModelBaseStateData = {
    val updatedStateData = baseStateData.schedulerStateData.copy(
      nowInTicks = newTick,
      mainTriggerId = Some(triggerId)
    )

    baseStateData.copy(
      schedulerStateData = sendEligibleTrigger(updatedStateData)
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

  private def handleFlexProvision(
      baseStateData: EmModelBaseStateData
  ): State = {
    val tick = baseStateData.schedulerStateData.nowInTicks

    // check if expected flex answers for this tick arrived
    val flexAnswersReceived = baseStateData.flexCorrespondences.isComplete

    if (flexAnswersReceived) {
      // All flex options and all results have been received.

      val flexData = baseStateData.flexCorrespondences.latestFlexData(
        baseStateData.participantInput
      )

      val updatedBaseStateData =
        baseStateData.flexStateData match {
          case Some(flexStateData) =>
            // this EmAgent is itself EM-controlled

            val flexOptionsInput = flexData
              .flatMap { case (spi, correspondence, _) =>
                correspondence.receivedFlexOptions.map((spi, _))
              }
              .collect { case (spi, flexOption: ProvideMinMaxFlexOptions) =>
                // adapt flex options, e.g. of devices that are
                // not controllable by the strategy of this EM
                spi -> baseStateData.model.modelStrategy
                  .adaptFlexOptions(spi, flexOption)
              }

            val (ref, min, max) =
              baseStateData.aggregateFlex.aggregateFlexOptions(
                flexOptionsInput
              )

            val flexMessage = ProvideMinMaxFlexOptions(
              baseStateData.modelUuid,
              ref,
              min,
              max
            )

            flexStateData.emAgent ! flexMessage

            baseStateData.copy(
              flexStateData = Some(
                flexStateData.copy(
                  flexOptionsStore = ValueStore.updateValueStore(
                    flexStateData.flexOptionsStore,
                    tick,
                    flexMessage
                  )
                )
              )
            )

          case None =>
            // if we're not EM-controlled ourselves, we're determining the set points
            // either via flex time series or as 0 kW
            val setPower = baseStateData.flexTimeSeries match {
              case Some(flexTimeSeries) =>
                // round current time to flexTimeSeries.resolutionHours hrs
                val currentDateTime = tick.toDateTime(baseStateData.startDate)
                val currentHour = currentDateTime.getHour
                val roundedHour =
                  currentHour - currentHour % flexTimeSeries.resolutionHours
                val roundedDateTime = currentDateTime
                  .withHour(roundedHour)
                  .withMinute(0)
                  .withSecond(0)
                  .withNano(0)

                val flexSetPower = flexTimeSeries.timeSeries
                  .getTimeBasedValue(roundedDateTime)
                  .asScala
                  .getOrElse(
                    throw new RuntimeException(
                      s"Could not retrieve value for $roundedDateTime"
                    )
                  )
                  .getValue
                  .getP
                  .asScala
                  .getOrElse(
                    throw new RuntimeException(
                      s"No value set for $roundedDateTime"
                    )
                  )

                val flexOptionsInput = flexData
                  .flatMap { case (spi, correspondence, _) =>
                    correspondence.receivedFlexOptions.map((spi, _))
                  }
                  .collect { case (spi, flexOption: ProvideMinMaxFlexOptions) =>
                    // adapt flex options, e.g. of devices that are
                    // not controllable by the strategy of this EM
                    spi -> baseStateData.model.modelStrategy
                      .adaptFlexOptions(spi, flexOption)
                  }

                // sum up ref power
                val (refSum, minSum, maxSum) =
                  baseStateData.aggregateFlex.aggregateFlexOptions(
                    flexOptionsInput
                  )

                // announce flex options (we can do this right away, since this
                // does not include reactive power which could change later
                if (baseStateData.outputConfig.flexResult) {
                  val flexResult = new FlexOptionsResult(
                    tick.toDateTime(baseStateData.startDate),
                    baseStateData.modelUuid,
                    refSum,
                    minSum,
                    maxSum
                  )

                  notifyListener(FlexOptionsResultEvent(flexResult))
                }

                val combinedPower = flexSetPower.add(refSum)

                val minThreshold =
                  flexTimeSeries.minValue.multiply(flexTimeSeries.threshold)
                val maxThreshold =
                  flexTimeSeries.maxValue.multiply(flexTimeSeries.threshold)

                // if target power is below threshold, issue no control
                if (
                  combinedPower.isGreaterThanOrEqualTo(minThreshold)
                  && combinedPower.isLessThanOrEqualTo(maxThreshold)
                ) {
                  refSum
                } else {
                  if (combinedPower.isLessThan(minThreshold)) {
                    // flex that is needed to reach threshold
                    val neededFlex = minThreshold.subtract(combinedPower)
                    // as we undershoot the lower threshold we need to increase load / decrease generation
                    val targetSetPoint = refSum.add(neededFlex)
                    if (targetSetPoint.isGreaterThan(maxSum)) maxSum else targetSetPoint
                  } else {
                    // flex that is needed to reach threshold
                    val neededFlex = maxThreshold.subtract(combinedPower)
                    // as we overshoot the upper threshold we need to decrease load / increase generation
                    val targetSetPoint = refSum.add(neededFlex)
                    if (targetSetPoint.isLessThan(minSum)) minSum else targetSetPoint
                  }
                }

              case None => zeroKW
            }

            determineAndSchedulePowerControl(
              baseStateData,
              flexData,
              targetPower = setPower
            )

        }

      stay() using updatedBaseStateData

    } else
      stay() using baseStateData
  }

  private def determineAndSchedulePowerControl(
      baseStateData: EmModelBaseStateData,
      flexData: Iterable[(SystemParticipantInput, FlexCorrespondence, Long)],
      targetPower: ComparableQuantity[Power] = zeroKW
  ): EmModelBaseStateData = {
    val tick = baseStateData.schedulerStateData.nowInTicks

    val flexStratInput = flexData.flatMap { case (spi, correspondence, _) =>
      correspondence.receivedFlexOptions.map(spi -> _)
    }

    // TODO sanity checks before strat calculation

    val issueCtrlMsgs = baseStateData.model
      .determineDeviceControl(
        flexStratInput.collect {
          case (spi, flexOption: ProvideMinMaxFlexOptions) =>
            (spi, flexOption)
        }.toSeq,
        targetPower
      )
      .toMap

    val issueCtrlMsgsComplete = flexData.flatMap {
      case (spi, correspondence, dataTick) =>
        issueCtrlMsgs
          .get(spi.getUuid)
          .map { power =>
            correspondence.receivedFlexOptions.getOrElse(
              throw new RuntimeException(
                s"FlexOptions not found for ${spi.getUuid}"
              )
            ) match {
              case flexOptions: ProvideMinMaxFlexOptions =>
                // sanity checks after strat calculation
                checkSetPower(flexOptions, power)
            }

            spi.getUuid -> IssuePowerCtrl(tick, power)
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
            (uuid, flexCtrl)
          }
    }

    if (baseStateData.modelUuid.toString == "15bdf719-68fd-47e2-8c33-f4fcb4322b00") {
      issueCtrlMsgs.foreach{case (uuid, setpoint) =>
        val actorRef = baseStateData.schedulerStateData.flexTrigger.uuidToActorRef(uuid)
        log.info(s"Ctrl msg setpoint = $setpoint, actor ref = $actorRef")
    }}

    val updatedScheduledStateData = issueCtrlMsgsComplete.foldLeft(
      baseStateData.schedulerStateData
    ) { case (schedulerData, (uuid, issueCtrlMsg)) =>
      // send out flex control messages
      val updatedFlexTrigger =
        scheduleFlexTriggerOnce(schedulerData.flexTrigger, issueCtrlMsg, uuid)

      sendEligibleTrigger(
        schedulerData.copy(
          flexTrigger = updatedFlexTrigger
        )
      )
    }

    val issueFlexParticipants = issueCtrlMsgsComplete.map { case (uuid, _) =>
      uuid
    }

    // create updated value stores for participants that are receiving control msgs
    val updatedCorrespondences = issueCtrlMsgsComplete
      .foldLeft(baseStateData.flexCorrespondences) {
        case (correspondences, (uuid, issueFlex)) =>
          correspondences.addIssuedFlexControl(
            uuid,
            tick,
            issueFlex
          )
      }
      .setExpectingResults(issueFlexParticipants.toSet)

    val updatedBaseStateData = baseStateData.copy(
      flexCorrespondences = updatedCorrespondences,
      schedulerStateData = updatedScheduledStateData
    )

    if (issueCtrlMsgsComplete.isEmpty) {
      // if we're EM-controlled and we only received an IssueFlexControl message,
      // it is possible that the result is empty

      // thus send a result to the parent EM (same one as before)...
      val stateDataWithResults = calculatePower(
        updatedBaseStateData,
        scheduler
      )

      // ... and send a flex completion message
      val (updatedSchedulerData, scheduledTick) = maybeTicksCompleted(
        stateDataWithResults.schedulerStateData,
        stateDataWithResults.modelUuid,
        stateDataWithResults.flexStateData.flatMap(_.scheduledRequest)
      )

      stateDataWithResults.copy(
        schedulerStateData = updatedSchedulerData,
        flexStateData = stateDataWithResults.flexStateData.map(
          _.copy(scheduledRequest = scheduledTick)
        )
      )
    } else
      updatedBaseStateData

  }

  private def calculatePower(
      baseStateData: EmModelBaseStateData,
      scheduler: ActorRef
  ): EmModelBaseStateData = {
    val correspondences =
      baseStateData.flexCorrespondences.latestCorrespondences

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
    * registered listeners and return updated base state data
    *
    * @param scheduler
    *   Actor reference of the scheduler
    * @param baseStateData
    *   The base state data of the collection state
    * @param result
    *   Result of simulation
    * @return
    *   Updated state data
    */
  final def updateValueStoresInformListeners(
      scheduler: ActorRef,
      baseStateData: EmModelBaseStateData,
      result: ApparentPower
  ): EmModelBaseStateData = {
    /* Update the value stores */
    val updatedValueStore =
      ValueStore.updateValueStore(
        baseStateData.resultValueStore,
        baseStateData.schedulerStateData.nowInTicks,
        result
      )

    /* Update the base state data */
    val baseStateDateWithUpdatedResults = baseStateData.copy(
      resultValueStore = updatedValueStore
    )

    /* Inform the listeners about current result
     * (IN CONTRAST TO REGULAR SPA BEHAVIOR, WHERE WE WAIT FOR POTENTIALLY CHANGED VOLTAGE) */
    announceSimulationResult(
      baseStateDateWithUpdatedResults,
      baseStateDateWithUpdatedResults.schedulerStateData.nowInTicks,
      AccompaniedSimulationResult(result)
    )(baseStateDateWithUpdatedResults.outputConfig)

    // FIXME this can probably be integrated into existing result processing
    // announce current result to parent EmAgent, if applicable
    baseStateData.flexStateData.foreach(
      _.emAgent ! buildResultEvent(
        baseStateDateWithUpdatedResults,
        baseStateDateWithUpdatedResults.schedulerStateData.nowInTicks,
        result
      )
    )

    unstashAll()

    baseStateDateWithUpdatedResults
  }

  /** Create trigger for this agent depending on whether it is controlled by a
    * parent EmAgent
    * @param tick
    *   the tick to create a trigger for
    * @param hasParentEm
    *   whether this EmAgent has a parent EmAgent
    * @return
    *   A trigger that this EmAgent would like to be activated with
    */
  private def createActivationTrigger(
      tick: Long,
      hasParentEm: Boolean
  ): Trigger = Option
    .when(hasParentEm)(RequestFlexOptions(tick))
    .getOrElse(ActivityStartTrigger(tick))

  private def handleReceiveTimeout(
      stateData: ParticipantStateData[ApparentPower]
  ): State = {
    // disable timeout again
    context.setReceiveTimeout(Duration.Undefined)

    log.warning(
      "No messages received for ten minutes. Current state data: " + stateData
    )
    stay()
  }
}
