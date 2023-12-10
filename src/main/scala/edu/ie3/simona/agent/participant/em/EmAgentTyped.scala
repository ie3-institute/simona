/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.em.EmAgent.{
  EmAgentInitializeStateData,
  EmModelBaseStateData,
  FlexTimeSeries
}
import edu.ie3.simona.agent.participant.em.EmSchedulerStateData.TriggerData
import edu.ie3.simona.agent.participant.em.FlexCorrespondenceStore2.FlexCorrespondence
import edu.ie3.simona.agent.participant.statedata.BaseStateData.FlexStateData
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.model.participant.em.{EmAggregateFlex, EmModel}
import edu.ie3.simona.ontology.messages.FlexibilityMessage.{
  FlexCtrlCompletion,
  FlexRequest,
  IssueFlexControl,
  IssueNoCtrl,
  IssuePowerCtrl,
  ProvideFlexOptions,
  ProvideMinMaxFlexOptions
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.ontology.messages.SchedulerMessage.ScheduleActivation
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import squants.Power
import squants.energy.Kilowatts
import edu.ie3.simona.model.participant.em.EmModelShell

import java.time.ZonedDateTime
import java.util.UUID
import scala.util.{Failure, Success, Try}

object EmAgentTyped {

  type Actor = ActorRef[FlexRequest]

  trait EmMessage

  private final case class EmActivation(tick: Long) extends EmMessage

  def apply(
      scheduler: ActorRef[SchedulerMessage],
      initStateData: EmAgentInitializeStateData
  ): Behavior[EmMessage] = Behaviors.setup { ctx =>
    val adapter = ctx.messageAdapter[Activation] { msg =>
      EmActivation(msg.tick)
    }
    scheduler ! ScheduleActivation(adapter, INIT_SIM_TICK)

    initializing(scheduler, initStateData, adapter)
  }

  private def initializing(
      scheduler: ActorRef[SchedulerMessage],
      initStateData: EmAgentInitializeStateData,
      adapter: ActorRef[Activation]
  ): Behaviors.Receive[EmMessage] = Behaviors.receive {
    case (ctx, EmActivation(INIT_SIM_TICK)) =>
      // sending init triggers
      val triggerData = initStateData.connectedAgents.foldLeft(TriggerData()) {
        case (triggerData, (actor, _)) =>
          ctx.watch(actor)
          scheduleTrigger(
            initTrigger,
            actor,
            triggerData,
            INIT_SIM_TICK
          )
      }

      val model = EmModel(
        initStateData.inputModel,
        initStateData.modelConfig,
        initStateData.simulationStartDate,
        initStateData.simulationEndDate,
        initStateData.modelStrategy
      )

      val maybeFlexTimeseries = initStateData.maybeRootEmConfig.map { config =>
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

        // in case of resLoad we use totalResload (considering Simona participants) for min max setting
        val (minValue, maxValue) =
          FlexSignalFromExcel.getCorrespondingMinMaxValues(
            timeSeriesType,
            timeSeries,
            config
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
        initStateData.simulationStartDate,
        initStateData.simulationEndDate,
        model,
        initStateData.outputConfig,
        FlexCorrespondenceStore2(
          initStateData.connectedAgents.map { case (_, sp) =>
            sp.getUuid
          }.toSet
        )(initStateData.simulationStartDate),
        initStateData.connectedAgents.map { case (_, sp) =>
          sp.getUuid -> sp
        }.toMap,
        EmSchedulerStateData(
          triggerData,
          initStateData.connectedAgents.map { case (actor, sp) =>
            sp.getUuid -> actor
          }.toMap,
          tick =>
            createActivationTrigger(
              tick,
              initStateData.maybeParentEmAgent.isDefined
            )
        ),
        initStateData.maybeParentEmAgent.map(
          FlexStateData(_, ValueStore(initStateData.resolution))
        ),
        maybeFlexTimeseries,
        initStateData.aggregateFlex
      )

      val updatedBaseStateData = setActiveTickAndSendTriggers(
        baseStateData,
        SimonaConstants.INIT_SIM_TICK,
        triggerId
      )

      inactive(scheduler, updatedBaseStateData, adapter)
  }

  private def inactive(
      scheduler: ActorRef[SchedulerMessage],
      stateData: EmModelBaseStateData,
      adapter: ActorRef[Activation]
  ): Behavior[EmMessage] = Behaviors.receive {
    // Activation or RequestFlexOptions

    case (ctx, EmActivation(tick)) =>

    case (ctx, issueCtrl: IssueFlexControl) =>
      // we receive issueFlexCtrl by the parent without being asked for flex options before
      ctx.self ! issueCtrl
      awaitingFlexCtrl()
  }

  private def awaitingFlexOptions(
      stateData: EmModelBaseStateData,
      modelShell: EmModelShell,
      flexOptionsCore: EmDataCore.AwaitingFlexOptions
  ): Behavior[EmMessage] = Behaviors.receive {
    case (ctx, flexOptions: ProvideFlexOptions) =>
      val updatedCore = flexOptionsCore.handleFlexOptions(flexOptions)

      if (updatedCore.isComplete) {

        val allFlexOptions = updatedCore.getFlexOptions

        stateData.flexStateData match {
          case Some(flexStateData) =>
            // aggregate flex options and provide to parent
            val (ref, min, max) =
              modelShell.aggregateFlexOptions(allFlexOptions)

            val flexMessage = ProvideMinMaxFlexOptions(
              ctx.self,
              ref,
              min,
              max
            )

            flexStateData.emAgent ! flexMessage

            val updatedStateData = stateData.copy(
              flexStateData = Some(
                flexStateData.copy(
                  lastFlexOptions = Some(flexMessage)
                )
              )
            )

            awaitingFlexCtrl(updatedStateData, updatedCore, activeTick)

          case None =>
            // no parent, target set point is 0 kW
            val ctrlSetPoints =
              modelShell.determineDeviceControl(allFlexOptions, Kilowatts(0))

            val (allFlexMsgs, awaitingResultsCore) = flexOptionsCore
              .handleFlexCtrl(ctrlSetPoints)
              .fillInMissingIssueCtrl()
              .complete()

            allFlexMsgs.foreach { case (actor, msg) =>
              actor ! msg
            }

            awaitingResults(stateData, awaitingResultsCore)

        }

      } else {
        // more flex options expected
        awaitingFlexOptions(
          updatedFlexStore,
          updatedAwaitedFlexOptions,
          activeTick
        )
      }

    case _ =>
      Behaviors.unhandled
  }

  /** If this EmAgent is itself controlled by a parent EmAgent, we wait for flex
    * control here
    *
    * TODO move to trait or object?
    */
  private def awaitingFlexCtrl(
      stateData: EmModelBaseStateData,
      flexOptionsCore: EmDataCore.AwaitingFlexOptions
  ): Behavior[EmMessage] = Behaviors.receive {
    case (ctx, flexCtrl: IssueFlexControl) =>
      // update tick since we could be activated without prior request for flex options
      val updatedStateData = stateData.copy(
        schedulerStateData = stateData.schedulerStateData.copy(
          nowInTicks = flexCtrl.tick
        )
      )

      val flexParticipantData = updatedStateData.flexStateData.getOrElse(
        throw new RuntimeException(
          s"EmAgent ${updatedStateData.model.uuid} is not EM-controlled."
        )
      )

      val flexOptions = flexParticipantData.lastFlexOptions
        .getOrElse(
          throw new RuntimeException(
            s"Flex options have not been calculated by agent ${updatedStateData.model.uuid}"
          )
        )

      determineResultingFlexPower(
        flexOptions,
        flexCtrl
      ).map { setPointActivePower =>
        val flexOptions = flexOptionsCore.getFlexOptions

        val ctrlSetPoints =
          modelShell.determineDeviceControl(flexOptions, setPointActivePower)

        val (allFlexMsgs, awaitingResults) = flexOptionsCore
          .handleFlexCtrl(ctrlSetPoints)
          .fillInMissingIssueCtrl()
          .complete()

        allFlexMsgs.foreach { case (actor, msg) =>
          actor ! msg
        }

        if (allFlexMsgs.isEmpty) {
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

        awaitingResults(finalStateData)
      }

    case _ =>
      Behaviors.unhandled

  }

  private def awaitingResults(
      stateData: EmModelBaseStateData,
      activeCore: EmDataCore.AwaitingResults
  ): Behavior[EmMessage] = {
    // Completions and results
    case (_, completion: FlexCtrlCompletion) =>
      val updatedFlexStore =
        stateData.flexStore.updateResult(
          completion.participant,
          completion.result,
          activeCore.activeTick
        )

      val updatedAwaitedResults = awaitedResults.excl(completion.participant)

      if (updatedAwaitedResults.isEmpty) {
        // send out em result

      }

      activeCore
        .maybeComplete()
        .map { _ =>
          inactive()
        }
        .getOrElse {
          // more flex options expected
          awaitingResults(updatedFlexStore, updatedAwaitedResults, activeTick)
        }

    case _ =>
      Behaviors.unhandled
  }

  protected def determineResultingFlexPower(
      flexOptionsMsg: ProvideFlexOptions,
      flexCtrl: IssueFlexControl
  ): Try[Power] =
    flexOptionsMsg match {
      case flexOptions: ProvideMinMaxFlexOptions =>
        flexCtrl match {
          case IssuePowerCtrl(_, setPower) =>
            // sanity check: setPower is in range of latest flex options
            checkSetPower(flexOptions, setPower).map { _ =>
              // override, take setPower
              setPower
            }

          case IssueNoCtrl(_) =>
            // no override, take reference power
            Success(flexOptions.referencePower)
        }

      case unknownFlexOpt =>
        Failure(
          new RuntimeException()(
            s"Unknown/unfitting flex messages $unknownFlexOpt"
          )
        )
    }

  protected def checkSetPower(
      flexOptions: ProvideMinMaxFlexOptions,
      setPower: squants.Power
  ): Try[Unit] = {
    if (setPower < flexOptions.minPower)
      Failure(
        new RuntimeException(
          s"The set power $setPower for ${flexOptions.participant} must not be lower than the minimum power ${flexOptions.minPower}!"
        )
      )
    else if (setPower > flexOptions.maxPower)
      Failure(
        new RuntimeException(
          s"The set power $setPower for ${flexOptions.participant} must not be greater than the maximum power ${flexOptions.maxPower}!"
        )
      )
    else
      Success(())
  }

  final case class EmModelBaseStateData(
      startDate: ZonedDateTime,
      endDate: ZonedDateTime,
      model: EmModel,
      outputConfig: NotifierConfig,
      flexStore: FlexCorrespondenceStore2,
      participantInput: Map[UUID, SystemParticipantInput],
      schedulerStateData: EmSchedulerStateData,
      flexStateData: Option[FlexStateData],
      flexTimeSeries: Option[FlexTimeSeries],
      aggregateFlex: EmAggregateFlex
  )
}
