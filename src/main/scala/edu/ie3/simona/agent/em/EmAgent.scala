/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.em

import edu.ie3.datamodel.models.input.system.EmInput
import edu.ie3.datamodel.models.result.system.{EmResult, FlexOptionsResult}
import edu.ie3.simona.actor.ActorUtil.stopOnError
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.statedata.BaseStateData.FlexControlledData
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.{
  FlexOptionsResultEvent,
  ParticipantResultEvent
}
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.model.participant.em.{EmModelShell, FlexTimeSeries}
import edu.ie3.simona.ontology.messages.FlexibilityMessage._
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.Megavars
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import squants.energy.{Kilowatts, Megawatts}

import java.time.ZonedDateTime

/** Energy management agent that receives flex options from and issues control
  * messages to connected agents
  * ([[edu.ie3.simona.agent.participant.ParticipantAgent]]s and subordinate
  * [[EmAgent]]s)
  */
object EmAgent {

  type Actor = ActorRef[FlexRequest]

  trait EmMessage

  /** Wrapper for an [[Activation]] for usage by an adapter
    * @param tick
    *   The tick to activate
    */
  private final case class EmActivation(tick: Long) extends EmMessage

  /** Wrapper for [[FlexRequest]] messages for usage by an adapter (if this
    * [[EmAgent]] is EM-controlled itself)
    * @param msg
    *   The wrapped flex request
    */
  private final case class Flex(msg: FlexRequest) extends EmMessage

  /** Creates the initial [[Behavior]] for an [[EmAgent]] in an inactive state
    *
    * @param inputModel
    *   Model for simulation
    * @param modelConfig
    *   Configuration for this type of model
    * @param modelStrategy
    *   The model strategy to use
    * @param outputConfig
    *   Config for the output behaviour of simulation results
    * @param simulationStartDate
    *   Date of the very first tick in the simulation
    * @param parent
    *   Either a [[Right]] with a reference to the parent [[EmAgent]] if this
    *   agent is em-controlled, or a [[Left]] with a reference to the scheduler
    *   that is activating this agent
    * @param maybeRootEmConfig
    *   Config for the root EM agent, if applicable
    * @param listener
    *   A collection of result event listeners
    */
  def apply(
      inputModel: EmInput,
      modelConfig: EmRuntimeConfig,
      outputConfig: NotifierConfig,
      modelStrategy: String,
      simulationStartDate: ZonedDateTime,
      parent: Either[ActorRef[SchedulerMessage], ActorRef[FlexResponse]],
      maybeRootEmConfig: Option[SimonaConfig.Simona.Runtime.RootEm] = None,
      listener: Iterable[ActorRef[ResultEvent]]
  ): Behavior[EmMessage] = Behaviors.setup { ctx =>
    val constantData = ConstantEmData(
      outputConfig,
      simulationStartDate,
      parent
        .map { parentEm =>
          val flexAdapter = ctx.messageAdapter[FlexRequest](Flex)

          parentEm ! RegisterParticipant(
            inputModel.getUuid,
            flexAdapter,
            inputModel
          )

          FlexControlledData(parentEm, flexAdapter)
        }
        .left
        .map { scheduler =>
          {
            val activationAdapter = ctx.messageAdapter[Activation] { msg =>
              EmActivation(msg.tick)
            }
            SchedulerData(scheduler, activationAdapter)
          }
        },
      maybeRootEmConfig.map(
        FlexTimeSeries(_)(simulationStartDate)
      ),
      listener
    )

    val modelShell = EmModelShell(
      inputModel.getUuid,
      inputModel.getId,
      modelStrategy,
      modelConfig
    )

    inactive(
      constantData,
      modelShell,
      EmDataCore.create(simulationStartDate)
    )
  }

  private def inactive(
      constantData: ConstantEmData,
      modelShell: EmModelShell,
      core: EmDataCore.Inactive
  ): Behavior[EmMessage] = Behaviors.receivePartial {

    case (_, RegisterParticipant(model, actor, spi)) =>
      val updatedModelShell = modelShell.addParticipant(model, spi)
      val updatedCore = core.addParticipant(actor, model)
      inactive(constantData, updatedModelShell, updatedCore)

    case (ctx, ScheduleFlexRequest(participant, newTick, scheduleKey)) =>
      if (core.checkScheduleRequest(newTick)) {
        val (maybeSchedule, newCore) =
          core.handleScheduleRequest(participant, newTick)

        maybeSchedule match {
          case Some(scheduleTick) =>
            // also potentially schedule with parent if the new earliest tick is
            // different from the old earliest tick (including if nothing had
            // been scheduled before)

            constantData.parentData.fold(
              schedulerData =>
                schedulerData.scheduler ! ScheduleActivation(
                  schedulerData.activationAdapter,
                  scheduleTick,
                  scheduleKey
                ),
              _.emAgent ! ScheduleFlexRequest(
                modelShell.uuid,
                scheduleTick,
                scheduleKey
              )
            )
          case None =>
            // we don't need to escalate to the parent, this means that we can release the lock (if applicable)
            scheduleKey.foreach {
              _.unlock()
            }
        }
        inactive(constantData, modelShell, newCore)
      } else {
        stopOnError(ctx, s"Cannot schedule an event at tick $newTick")
      }

    case (ctx, EmActivation(tick)) =>
      activate(constantData, modelShell, core, tick, ctx)

    case (ctx, Flex(RequestFlexOptions(tick))) =>
      activate(constantData, modelShell, core, tick, ctx)

    case (ctx, issueCtrl: IssueFlexControl) =>
      // we receive issueFlexCtrl by the parent without being asked for flex options before

      // there should be no scheduled flex requests at all,
      // because we would have received a flex request before
      if (core.checkActivation(issueCtrl.tick)) {
        // forward message to proper behavior
        ctx.self ! issueCtrl

        core
          .activate()
          .fold(
            stopOnError(ctx, _),
            awaitingFlexCtrl(constantData, modelShell, _)
          )
      } else {
        stopOnError(ctx, s"There should be no flex requests scheduled at all.")
      }
  }

  private def activate(
      constantData: ConstantEmData,
      modelShell: EmModelShell,
      core: EmDataCore.Inactive,
      tick: Long,
      ctx: ActorContext[EmMessage]
  ): Behavior[EmMessage] =
    if (core.checkActivation(tick)) {
      core
        .activate()
        .flatMap(_.takeNewFlexRequests())
        .map { case (toActivate, flexOptionsCore) =>
          toActivate.foreach {
            _ ! RequestFlexOptions(tick)
          }

          awaitingFlexOptions(constantData, modelShell, flexOptionsCore)
        }
        .fold(
          stopOnError(ctx, _),
          identity
        )
    } else {
      stopOnError(ctx, s"Cannot activate with new tick $tick")
    }

  private def awaitingFlexOptions(
      constantData: ConstantEmData,
      modelShell: EmModelShell,
      flexOptionsCore: EmDataCore.AwaitingFlexOptions
  ): Behavior[EmMessage] = Behaviors.receivePartial {
    case (ctx, flexOptions: ProvideFlexOptions) =>
      val updatedCore = flexOptionsCore.handleFlexOptions(flexOptions)

      if (updatedCore.isComplete) {

        val allFlexOptions = updatedCore.getFlexOptions

        constantData.parentData match {
          case Right(flexStateData) =>
            // aggregate flex options and provide to parent
            val (ref, min, max) =
              modelShell.aggregateFlexOptions(allFlexOptions)

            if (constantData.outputConfig.flexResult) {
              val flexResult = new FlexOptionsResult(
                flexOptionsCore.activeTick.toDateTime(
                  constantData.simulationStartDate
                ),
                modelShell.uuid,
                ref.toMegawatts.asMegaWatt,
                min.toMegawatts.asMegaWatt,
                max.toMegawatts.asMegaWatt
              )

              constantData.listener.foreach {
                _ ! FlexOptionsResultEvent(flexResult)
              }
            }

            val flexMessage = ProvideMinMaxFlexOptions(
              modelShell.uuid,
              ref,
              min,
              max
            )

            flexStateData.emAgent ! flexMessage

            val updatedStateData = constantData.copy(
              parentData = Right(
                flexStateData.copy(
                  lastFlexOptions = Some(flexMessage)
                )
              )
            )

            awaitingFlexCtrl(updatedStateData, modelShell, updatedCore)

          case Left(_) =>
            // if we're not EM-controlled ourselves, we're determining the set points
            // either via flex time series or as 0 kW
            val setPower = constantData.flexTimeSeries match {
              case Some(_) =>
                throw new NotImplementedError(
                  "Flex time series are currently not implemented"
                )

              case None => Kilowatts(0)
            }

            // no parent, target set point is 0 kW
            val ctrlSetPoints =
              modelShell.determineDeviceControl(allFlexOptions, setPower)

            updatedCore
              .handleFlexCtrl(ctrlSetPoints)
              .fillInMissingIssueCtrl()
              .complete()
              .map { case (allFlexMsgs, newCore) =>
                allFlexMsgs.foreach { case (actor, msg) =>
                  actor ! msg
                }

                awaitingCompletions(constantData, modelShell, newCore)
              }
              .fold(
                stopOnError(ctx, _),
                identity
              )
        }

      } else {
        // more flex options expected
        awaitingFlexOptions(
          constantData,
          modelShell,
          updatedCore
        )
      }

    /* We do not need to handle ScheduleFlexRequests here, since active agents
       can schedule themselves with there completions and inactive agents should
       be sleeping right now
     */
  }

  /** If this EmAgent is itself controlled by a parent EmAgent, we wait for flex
    * control here
    */
  private def awaitingFlexCtrl(
      stateData: ConstantEmData,
      modelShell: EmModelShell,
      flexOptionsCore: EmDataCore.AwaitingFlexOptions
  ): Behavior[EmMessage] = Behaviors.receivePartial {
    case (ctx, Flex(flexCtrl: IssueFlexControl)) =>
      stateData.parentData.left
        .map(_ => s"EmAgent is not EM-controlled.")
        .flatMap(
          // flex options calculated by this EmAgent
          _.lastFlexOptions.toRight(
            s"Flex options have not been calculated by EmAgent."
          )
        )
        .flatMap(flexOptions =>
          EmModelShell.determineResultingFlexPower(
            flexOptions,
            flexCtrl
          )
        )
        .flatMap { setPointActivePower =>
          // flex options calculated by connected agents
          val receivedFlexOptions = flexOptionsCore.getFlexOptions

          val ctrlSetPoints =
            modelShell.determineDeviceControl(
              receivedFlexOptions,
              setPointActivePower
            )

          flexOptionsCore
            .handleFlexCtrl(ctrlSetPoints)
            .fillInMissingIssueCtrl()
            .complete()
            .map { case (allFlexMsgs, newCore) =>
              allFlexMsgs.foreach { case (actor, msg) =>
                actor ! msg
              }

              awaitingCompletions(stateData, modelShell, newCore)
            }
        }
        .fold(
          stopOnError(ctx, _),
          identity
        )

  }

  private def awaitingCompletions(
      constantData: ConstantEmData,
      modelShell: EmModelShell,
      core: EmDataCore.AwaitingCompletions
  ): Behavior[EmMessage] = Behaviors.receivePartial {
    // Completions and results
    case (ctx, completion: FlexCtrlCompletion) =>
      Either
        .cond(
          core.checkCompletion(completion.modelUuid),
          core.handleCompletion(completion),
          s"Participant ${completion.modelUuid} is not part of the expected completing participants"
        )
        .map { updatedCore =>
          updatedCore
            .maybeComplete()
            .map { case (maybeScheduleTick, inactiveCore) =>
              // calc result
              val result = updatedCore.getResults
                .reduceOption { (power1, power2) =>
                  ApparentPower(power1.p + power2.p, power1.q + power2.q)
                }
                .getOrElse(
                  ApparentPower(
                    Megawatts(0d),
                    Megavars(0d)
                  )
                )

              constantData.listener.foreach {
                _ ! ParticipantResultEvent(
                  new EmResult(
                    updatedCore.activeTick
                      .toDateTime(constantData.simulationStartDate),
                    modelShell.uuid,
                    result.p.toMegawatts.asMegaWatt,
                    result.q.toMegavars.asMegaVar
                  )
                )
              }

              constantData.parentData.fold(
                schedulerData =>
                  schedulerData.scheduler ! Completion(
                    schedulerData.activationAdapter,
                    maybeScheduleTick
                  ),
                _.emAgent ! FlexCtrlCompletion(
                  modelShell.uuid,
                  result,
                  requestAtNextActivation = false,
                  maybeScheduleTick
                )
              )

              inactive(constantData, modelShell, inactiveCore)
            }
            .getOrElse {
              // more flex options expected
              awaitingCompletions(
                constantData,
                modelShell,
                updatedCore
              )
            }
        }
        .fold(
          stopOnError(ctx, _),
          identity
        )

  }

  /** Data that is supposed to stay constant during simulation
    *
    * @param outputConfig
    *   Config for the output behaviour of simulation results
    * @param simulationStartDate
    *   Date of the very first tick in the simulation
    * @param parentData
    *   Either a [[Right]] with [[FlexControlledData]] if this agent is
    *   em-controlled, or a [[Left]] with [[SchedulerData]]
    * @param flexTimeSeries
    *   Flex time series if this is the root EM
    * @param listener
    *   A collection of result event listeners
    */
  private final case class ConstantEmData(
      outputConfig: NotifierConfig,
      simulationStartDate: ZonedDateTime,
      parentData: Either[SchedulerData, FlexControlledData],
      flexTimeSeries: Option[FlexTimeSeries],
      listener: Iterable[ActorRef[ResultEvent]]
  )

  /** The existence of this data object indicates that the corresponding agent
    * is not EM-controlled, but activated by a
    * [[edu.ie3.simona.scheduler.Scheduler]]
    *
    * @param scheduler
    *   The scheduler that is activating this agent
    * @param activationAdapter
    *   The activation adapter handling [[Activation]] messages
    */
  final case class SchedulerData(
      scheduler: ActorRef[SchedulerMessage],
      activationAdapter: ActorRef[Activation]
  )
}
