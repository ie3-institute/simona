/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import edu.ie3.datamodel.models.input.system.EmInput
import edu.ie3.datamodel.models.result.system.EmResult
import edu.ie3.simona.actor.ActorUtil.stopOnError
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.statedata.BaseStateData.FlexStateData
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
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
import squants.Power
import squants.energy.{Kilowatts, Megawatts}

import java.time.ZonedDateTime
import scala.util.{Failure, Success, Try}

// TODO move package em out of participant
object EmAgent {

  type Actor = ActorRef[FlexRequest]

  trait EmMessage

  private final case class EmActivation(tick: Long) extends EmMessage

  private final case class Flex(msg: FlexRequest) extends EmMessage

  def apply(
      inputModel: EmInput,
      modelConfig: EmRuntimeConfig,
      outputConfig: NotifierConfig,
      modelStrategy: String,
      simulationStartDate: ZonedDateTime,
      maybeParentEmAgent: Option[ActorRef[FlexResponse]] = None,
      maybeRootEmConfig: Option[SimonaConfig.Simona.Runtime.RootEm] = None,
      scheduler: ActorRef[SchedulerMessage],
      listener: Iterable[ActorRef[ResultEvent]]
  ): Behavior[EmMessage] = Behaviors.setup { ctx =>
    val activationAdapter = ctx.messageAdapter[Activation] { msg =>
      EmActivation(msg.tick)
    }

    val stateData = EmData(
      scheduler,
      listener,
      activationAdapter,
      outputConfig,
      simulationStartDate,
      maybeParentEmAgent.map { parentEm =>
        val flexAdapter = ctx.messageAdapter[FlexRequest](Flex)

        parentEm ! RegisterParticipant(
          inputModel.getUuid,
          flexAdapter,
          inputModel
        )

        FlexStateData(parentEm, flexAdapter)
      },
      maybeRootEmConfig.map(
        FlexTimeSeries(_)(simulationStartDate)
      )
    )

    val modelShell = EmModelShell(
      inputModel.getUuid,
      inputModel.getId,
      modelStrategy,
      modelConfig
    )

    inactive(
      stateData,
      modelShell,
      EmDataCore.create(simulationStartDate)
    )
  }

  private def inactive(
      stateData: EmData,
      modelShell: EmModelShell,
      core: EmDataCore.Inactive
  ): Behavior[EmMessage] = Behaviors.receive {

    case (_, RegisterParticipant(model, actor, spi)) =>
      val updatedModelShell = modelShell.addParticipant(model, spi)
      val updatedCore = core.addParticipant(actor, model)
      inactive(stateData, updatedModelShell, updatedCore)

    case (ctx, ScheduleFlexRequest(participant, newTick, scheduleKey)) =>
      if (core.checkSchedule(newTick)) {
        val (maybeSchedule, newCore) = core.handleSchedule(participant, newTick)

        maybeSchedule match {
          case Some(scheduleTick) =>
            // also potentially schedule with parent if the new earliest tick is
            // different from the old earliest tick (including if nothing had
            // been scheduled before)

            stateData.flexStateData
              .map { flexData =>
                flexData.emAgent ! ScheduleFlexRequest(
                  modelShell.uuid,
                  scheduleTick,
                  scheduleKey
                )
              }
              .getOrElse {
                stateData.scheduler ! ScheduleActivation(
                  stateData.activationAdapter,
                  scheduleTick,
                  scheduleKey
                )
              }
          case None =>
            // we don't need to escalate to the parent, this means that we can release the lock (if applicable)
            scheduleKey.foreach {
              _.unlock()
            }
        }
        inactive(stateData, modelShell, newCore)
      } else {
        stopOnError(ctx, s"Cannot schedule an event at tick $newTick")
      }

    case (ctx, EmActivation(tick)) =>
      activate(stateData, modelShell, core, tick, ctx)

    case (ctx, Flex(RequestFlexOptions(tick))) =>
      activate(stateData, modelShell, core, tick, ctx)

    case (ctx, issueCtrl: IssueFlexControl) =>
      // we receive issueFlexCtrl by the parent without being asked for flex options before

      // there should be no scheduled flex requests at all,
      // because we would have received a flex request before
      if (core.checkActivation(issueCtrl.tick)) {
        // forward message to proper behavior
        ctx.self ! issueCtrl

        awaitingFlexCtrl(stateData, modelShell, core.activate())
      } else {
        stopOnError(ctx, s"There should be no flex requests scheduled at all.")
      }
  }

  private def activate(
      stateData: EmData,
      modelShell: EmModelShell,
      core: EmDataCore.Inactive,
      tick: Long,
      ctx: ActorContext[EmMessage]
  ): Behavior[EmMessage] =
    if (core.checkActivation(tick)) {
      val (toActivate, flexOptionsCore) = core.activate().takeNewActivations()

      toActivate.foreach {
        _ ! RequestFlexOptions(tick)
      }

      awaitingFlexOptions(stateData, modelShell, flexOptionsCore)
    } else {
      stopOnError(ctx, s"Cannot activate with new tick $tick")
    }

  private def awaitingFlexOptions(
      stateData: EmData,
      modelShell: EmModelShell,
      flexOptionsCore: EmDataCore.AwaitingFlexOptions
  ): Behavior[EmMessage] = Behaviors.receive {
    case (_, flexOptions: ProvideFlexOptions) =>
      val updatedCore = flexOptionsCore.handleFlexOptions(flexOptions)

      if (updatedCore.isComplete) {

        val allFlexOptions = updatedCore.getFlexOptions

        stateData.flexStateData match {
          case Some(flexStateData) =>
            // aggregate flex options and provide to parent
            val (ref, min, max) =
              modelShell.aggregateFlexOptions(allFlexOptions)

            val flexMessage = ProvideMinMaxFlexOptions(
              modelShell.uuid,
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

            awaitingFlexCtrl(updatedStateData, modelShell, updatedCore)

          case None =>
            // if we're not EM-controlled ourselves, we're determining the set points
            // either via flex time series or as 0 kW
            val setPower = stateData.flexTimeSeries match {
              case Some(_) =>
                throw new NotImplementedError(
                  "Flex time series are currently not implemented"
                )

              case None => Kilowatts(0)
            }

            // no parent, target set point is 0 kW
            val ctrlSetPoints =
              modelShell.determineDeviceControl(allFlexOptions, setPower)

            val (allFlexMsgs, newCore) = updatedCore
              .handleFlexCtrl(ctrlSetPoints)
              .fillInMissingIssueCtrl()
              .complete()

            allFlexMsgs.foreach { case (actor, msg) =>
              actor ! msg
            }

            awaitingCompletions(stateData, modelShell, newCore)

        }

      } else {
        // more flex options expected
        awaitingFlexOptions(
          stateData,
          modelShell,
          updatedCore
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
      stateData: EmData,
      modelShell: EmModelShell,
      flexOptionsCore: EmDataCore.AwaitingFlexOptions
  ): Behavior[EmMessage] = Behaviors.receive {
    case (ctx, Flex(flexCtrl: IssueFlexControl)) =>
      val flexParticipantData = stateData.flexStateData.getOrElse(
        throw new RuntimeException(
          s"EmAgent is not EM-controlled."
        )
      )

      val flexOptions = flexParticipantData.lastFlexOptions
        .getOrElse(
          throw new RuntimeException(
            s"Flex options have not been calculated by EmAgent."
          )
        )

      determineResultingFlexPower(
        flexOptions,
        flexCtrl
      ).map { setPointActivePower =>
        val flexOptions = flexOptionsCore.getFlexOptions

        val ctrlSetPoints =
          modelShell.determineDeviceControl(flexOptions, setPointActivePower)

        val (allFlexMsgs, newCore) = flexOptionsCore
          .handleFlexCtrl(ctrlSetPoints)
          .fillInMissingIssueCtrl()
          .complete()

        allFlexMsgs.foreach { case (actor, msg) =>
          actor ! msg
        }

        awaitingCompletions(stateData, modelShell, newCore)
      }.getOrElse {
        stopOnError(ctx, "") // TODO
      }

    case _ =>
      Behaviors.unhandled

  }

  private def awaitingCompletions(
      stateData: EmData,
      modelShell: EmModelShell,
      core: EmDataCore.AwaitingCompletions
  ): Behavior[EmMessage] = Behaviors.receive {
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

              stateData.listener.foreach {
                _ ! ParticipantResultEvent(
                  new EmResult(
                    updatedCore.activeTick
                      .toDateTime(stateData.simulationStartDate),
                    modelShell.uuid,
                    result.p.toMegawatts.asMegaWatt,
                    result.q.toMegavars.asMegaVar
                  )
                )
              }

              stateData.flexStateData
                .map { flexData =>
                  flexData.emAgent ! FlexCtrlCompletion(
                    modelShell.uuid,
                    result,
                    requestAtNextActivation = false,
                    maybeScheduleTick
                  )
                }
                .getOrElse {
                  stateData.scheduler ! Completion(
                    stateData.activationAdapter,
                    maybeScheduleTick
                  )

                }

              inactive(stateData, modelShell, inactiveCore)
            }
            .getOrElse {
              // more flex options expected
              awaitingCompletions(
                stateData,
                modelShell,
                updatedCore
              )
            }
        }
        .fold(
          stopOnError(ctx, _),
          identity
        )

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
          new RuntimeException(
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
          s"The set power $setPower for ${flexOptions.modelUuid} must not be lower than the minimum power ${flexOptions.minPower}!"
        )
      )
    else if (setPower > flexOptions.maxPower)
      Failure(
        new RuntimeException(
          s"The set power $setPower for ${flexOptions.modelUuid} must not be greater than the maximum power ${flexOptions.maxPower}!"
        )
      )
    else
      Success(())
  }

  /** Data that is supposed to stay constant during simulation
    */
  private final case class EmData(
      scheduler: ActorRef[SchedulerMessage],
      listener: Iterable[ActorRef[ResultEvent]],
      activationAdapter: ActorRef[Activation],
      outputConfig: NotifierConfig,
      simulationStartDate: ZonedDateTime,
      flexStateData: Option[FlexStateData],
      flexTimeSeries: Option[FlexTimeSeries]
  )
}
