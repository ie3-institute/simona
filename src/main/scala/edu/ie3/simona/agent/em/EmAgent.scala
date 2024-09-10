/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.em

import edu.ie3.datamodel.models.input.EmInput
import edu.ie3.datamodel.models.result.system.{EmResult, FlexOptionsResult}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{ApparentPower, ZERO_POWER}
import edu.ie3.simona.agent.participant.statedata.BaseStateData.FlexControlledData
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.{FlexOptionsResultEvent, ParticipantResultEvent}
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.em.{EmModelShell, EmTools}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{Completion, ScheduleActivation}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{IssueFlexControl, _}
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ExtEmDataServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.WrappedRegistrationSuccessfulMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationSuccessfulMessage
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities._
import org.apache.pekko.actor.typed.scaladsl.{Behaviors, StashBuffer}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.{ActorRef => ClassicRef}
import org.apache.pekko.actor.typed.scaladsl.adapter._
import squants.Power

import java.time.ZonedDateTime

/** Energy management agent that receives flex options from and issues control
  * messages to connected agents
  * ([[edu.ie3.simona.agent.participant.ParticipantAgent]]s and subordinate
  * [[EmAgent]]s)
  */
object EmAgent {

  type Actor = ActorRef[FlexRequest]

  /** Extended by all messages that an [[EmAgent]] can receive
    */
  trait Request

  /** Extended by all requests that activate an [[EmAgent]], i.e. activations,
    * flex requests and control messages
    */
  private sealed trait ActivationRequest extends Request {
    val tick: Long
  }

  /** Wrapper for an [[Activation]] for usage by an adapter. Activations can
    * only be received if this EM agent is not EM-controlled.
    *
    * @param tick
    *   The tick to activate
    */
  private final case class EmActivation(override val tick: Long)
      extends ActivationRequest

  /** Wrapper for [[FlexRequest]] messages for usage by an adapter (if this
    * [[EmAgent]] is EM-controlled itself)
    *
    * @param msg
    *   The wrapped flex request
    */
  private final case class Flex(msg: FlexRequest) extends ActivationRequest {
    override val tick: Long = msg.tick
  }

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
      listener: Iterable[ActorRef[ResultEvent]],
      extEmDataService: Option[ClassicRef]
  ): Behavior[Request] = Behaviors.setup[Request] { ctx =>
    val flexAdapterEmDataService = ctx.messageAdapter[FlexRequest](Flex)
    var extInitTick = Option.empty[Long]
    if (extEmDataService.isDefined) {
      extEmDataService.getOrElse(throw new RuntimeException("No Service")) ! ExtEmDataServiceRegistrationMessage(
        inputModel.getUuid,
        ctx.self,
        flexAdapterEmDataService
      )
      extInitTick = Some(0L)
    }

    val constantData = EmData(
      outputConfig,
      simulationStartDate,
      parent
        .map { parentEm =>
          val flexAdapter = ctx.messageAdapter[FlexRequest](Flex)

          parentEm ! RegisterParticipant(
            inputModel.getUuid,
            flexAdapter,
            inputModel,
          )

          FlexControlledData(parentEm, flexAdapter)
        }
        .left
        .map { scheduler => {
          val activationAdapter = ctx.messageAdapter[Activation] { msg =>
            EmActivation(msg.tick)
          }
          SchedulerData(scheduler, activationAdapter)
        }
        },
      listener,
      ExternalEmDataServiceData(extEmDataService)
    )


    val modelShell = EmModelShell(
      inputModel.getUuid,
      inputModel.getId,
      inputModel.getControlStrategy,
      modelConfig,
    )

    //ctx.log.info(s"EMAgent ${modelShell.uuid} with $modelShell")

    inactive(
      constantData,
      modelShell,
      EmDataCore.create(simulationStartDate, extInitTick)
    )
  }

  /** Behavior of an inactive [[EmAgent]], which waits for an activation or flex
    * request to be activated.
    */
  private def inactive(
                        emData: EmData,
                        modelShell: EmModelShell,
                        core: EmDataCore.Inactive,
  ): Behavior[Request] = Behaviors.receivePartial {

    case (ctx, RegisterParticipant(model, actor, spi)) =>
      //ctx.log.info(s"EM Agent ${modelShell.uuid} RegisterParticipant model $model")
      val updatedModelShell = modelShell.addParticipant(model, spi)
      val updatedCore = core.addParticipant(actor, model)
      inactive(emData, updatedModelShell, updatedCore)

    case (ctx, WrappedRegistrationSuccessfulMessage(RegistrationSuccessfulMessage(serviceRef, nextDataTick))) =>
      //ctx.log.info(s"EM Agent ${ctx.self} will use external set points!")
      /*
      val flexAdapter = ctx.messageAdapter[FlexRequest](Flex)
      val updatedEmData = emData.copy(
        parentData = Right(FlexControlledData(emData.extEmDataService.getOrElse(throw new RuntimeException("")).toTyped, flexAdapter))
      )
      */
      inactive(emData, modelShell, core)


    case (ctx, ScheduleFlexRequest(participant, newTick, scheduleKey)) =>
      //ctx.log.info(s"EM Agent ${modelShell.uuid} got ScheduleFlexRequest!")
      val (maybeSchedule, newCore) = core
        .handleSchedule(participant, newTick)

      maybeSchedule match {
        case Some(scheduleTick) =>
          //ctx.log.info(s"EM Agent ${modelShell.uuid} -> parentData = ${emData.parentData}")
          // also potentially schedule with parent if the new earliest tick is
          // different from the old earliest tick (including if nothing had
          // been scheduled before)
          emData.parentData.fold(
            schedulerData =>
              schedulerData.scheduler ! ScheduleActivation(
                schedulerData.activationAdapter,
                scheduleTick,
                scheduleKey,
              ),
            _.emAgent ! ScheduleFlexRequest(
              modelShell.uuid,
              scheduleTick,
              scheduleKey,
            ),
          )
        case None =>
          // we don't need to escalate to the parent, this means that we can
          // release the lock (if applicable)
          scheduleKey.foreach {
            _.unlock()
          }
      }
      inactive(emData, modelShell, newCore)

    case (ctx, msg: ActivationRequest) =>
      //ctx.log.info(s"\u001b[0;34m[${msg.tick}] ${ctx.self}.inactive got ActivationRequest = $msg, dataProvisionMessage = ${core.nextSetPointMessage}, nextSetPointTick = ${core.nextSetPointTick}\u001b[0;0m")

      msg match {
        case Flex(_: RequestFlexOptions) | EmActivation(_) =>           // Activation by another EMAgent or by the scheduler
          val flexOptionsCore = core.activate(msg.tick)

          // Check if there will be a new set point for this tick -> We can't start processing flex options before we know what's the set point for this tick
          if (core.nextSetPointTick.contains(msg.tick)) {
            // We expect a new set point for this tick
            core.nextSetPointMessage match {
              case Some(setPointMsg) => // We already got a set point, check if the set point is for the right tick
                if (setPointMsg.tick == msg.tick) { // yes, it's for the right tick -> we can activate our connected agents and do the normal stuff
                  val (toActivate, newCore) = flexOptionsCore.handleSetPoint(setPointMsg).takeNewFlexRequests()
                  ctx.log.info(s"\u001b[0;34m[${flexOptionsCore.activeTick}] ${ctx.self}.inactive expects and received set point for this tick\n -> activate connected agents $toActivate\n -> send IssuePowerControl to myself with the new set point ${setPointMsg.setPower} \u001b[0;0m")
                  toActivate.foreach {
                    _ ! RequestFlexOptions(msg.tick)
                  }
                  ctx.self ! Flex(IssuePowerControl(flexOptionsCore.activeTick, setPointMsg.setPower))
                  awaitingFlexOptions(emData, modelShell, newCore)
                } else {
                  throw new RuntimeException("Set point for wrong tick arrived!")
                }
              case _ => // We still have to wait for a set point
                val (toActivate, newCore) = flexOptionsCore.takeNewFlexRequests()
                //ctx.log.info(s"\u001b[0;34m[${flexOptionsCore.activeTick}] ${ctx.self}.inactive expects set point for this tick, but I have to wait..., toActivate = $toActivate\u001b[0;0m")
                toActivate.foreach {
                  _ ! RequestFlexOptions(msg.tick)
                }
                awaitingFlexOptions(emData, modelShell, newCore)
            }
          } else { // We don't expect a new set point -> we can do our normal stuff, because we are activated because at least one connected agent should provide flex options
            val (toActivate, newCore) = flexOptionsCore.updateSetPoint().takeNewFlexRequests()
            //ctx.log.info(s"\u001b[0;34m[${flexOptionsCore.activeTick}] EM Agent ${ctx.self} doesn't expect set point for this tick, toActivate = $toActivate\u001b[0;0m")
            toActivate.foreach {
              _ ! RequestFlexOptions(msg.tick)
            }
            awaitingFlexOptions(emData, modelShell, newCore)
          }

        case Flex(_: IssueFlexControl) =>
          // We got sent a flex control message instead of a flex request,
          // this means that flex options must have not changed since
          // they were last calculated

          // Thus, we just jump to the appropriate place and forward the
          // control message there
          val flexOptionsCore = core.activate(msg.tick)
          ctx.self ! msg

          awaitingFlexCtrl(emData, modelShell, flexOptionsCore)

        case Flex(msg: SetPointFlexRequest) =>
          // We didn't get an activation yet, but a set point arrived -> save message and wait for an activation
          //ctx.log.info(s"(${core.getLastActiveTick}) ${ctx.self}.inactive got external set point = $msg before activation -> save message and wait...")
          val newCore = core.handleSetPointMessage(msg)

          inactive(emData, modelShell, newCore)
      }

  }

  /** Behavior of an [[EmAgent]] waiting for flex options to be received in
    * order to transition to the next behavior.
    */
  private def awaitingFlexOptions(
      emData: EmData,
      modelShell: EmModelShell,
      flexOptionsCore: EmDataCore.AwaitingFlexOptions
  ): Behavior[Request] = Behaviors.receivePartial {
    case (ctx, flexOptions: ProvideFlexOptions) =>
      val updatedCore = flexOptionsCore.handleFlexOptions(flexOptions)

      if (updatedCore.isComplete) {

        val allFlexOptions = updatedCore.getFlexOptions

        //ctx.log.info(s"EM Agent ${ctx.self} allFlexOptions = $allFlexOptions")

        emData.parentData match {
          case Right(flexStateData) =>
            // aggregate flex options and provide to parent
            val (ref, min, max) =
              modelShell.aggregateFlexOptions(allFlexOptions)

            if (emData.outputConfig.flexResult) {
              val flexResult = new FlexOptionsResult(
                flexOptionsCore.activeTick.toDateTime(
                  emData.simulationStartDate
                ),
                modelShell.uuid,
                ref.toMegawatts.asMegaWatt,
                min.toMegawatts.asMegaWatt,
                max.toMegawatts.asMegaWatt,
              )

              emData.listener.foreach {
                _ ! FlexOptionsResultEvent(flexResult)
              }
            }

            val flexMessage = ProvideMinMaxFlexOptions(
              modelShell.uuid,
              ref,
              min,
              max,
            )

            flexStateData.emAgent ! flexMessage

            val updatedEmData = emData.copy(
              parentData = Right(
                flexStateData.copy(
                  lastFlexOptions = Some(flexMessage)
                )
              )
            )

            awaitingFlexCtrl(updatedEmData, modelShell, updatedCore)

          case Left(_) =>
            // We're not em-controlled ourselves,
            // always desire to come as close as possible to 0 kW -> maybe overwrite it if we get a set point
            val setPower = updatedCore.currentSetPower.getOrElse(
              throw new CriticalFailureException(
                "Uncontrolled agent received ProvideFlexOptions without a set point!"
              ))
            val flexControl =
              modelShell.determineFlexControl(allFlexOptions, setPower)

            val (allFlexMsgs, newCore) = updatedCore
              .handleFlexCtrl(flexControl)
              .fillInMissingIssueCtrl()
              .complete()

            allFlexMsgs.foreach { case (actor, msg) =>
              actor ! msg
            }

            awaitingCompletions(emData, modelShell, newCore)
        }
      } else {
        // more flex options expected
        awaitingFlexOptions(
          emData,
          modelShell,
          updatedCore,
        )
      }

    /* We do not need to handle ScheduleFlexRequests here, since active agents
       can schedule themselves with there completions and inactive agents should
       be sleeping right now
     */
    case (ctx, Flex(setPointMsg: SetPointFlexRequest)) =>
      // We got a set point after Activation -> Check, if setPower changed (yes) we have to calculate new set points for our connected agents (no) activate core and do the updates
      ctx.log.info(s"\u001b[0;36m[${flexOptionsCore.activeTick}] ${ctx.self}.awaitingFlexOptions got external set point = $setPointMsg\u001b[0;0m")
      val updatedCore = flexOptionsCore.handleSetPoint(setPointMsg)
      ctx.self ! Flex(IssuePowerControl(flexOptionsCore.activeTick, setPointMsg.setPower))
      awaitingFlexOptions(emData, modelShell, updatedCore)

    case (ctx, Flex(flexCtrl: IssuePowerControl)) =>
      //ctx.log.info(s"[${flexOptionsCore.activeTick}] ${ctx.self}.awaitingFlexOptions.IssuePowerControl received IssuePowerControl $flexCtrl")
      if (flexOptionsCore.isComplete) {
        //ctx.log.info(s"[${flexOptionsCore.activeTick}] ${ctx.self}.awaitingFlexOptions.IssuePowerControl core is already complete")
        val allFlexOptions = flexOptionsCore.getFlexOptions
        // We're not em-controlled ourselves,
        // always desire to come as close as possible to 0 kW -> maybe overwrite it if we get a set point
        val setPower = flexCtrl.setPower

        val flexControl =
          modelShell.determineFlexControl(allFlexOptions, setPower)

        val (allFlexMsgs, newCore) = flexOptionsCore
          .handleFlexCtrl(flexControl)
          .fillInMissingIssueCtrl()
          .complete()

        if(allFlexMsgs.isEmpty) {
          newCore
            .maybeComplete()
            .map { inactiveCore =>
              sendCompletionCommunication(
                emData,
                modelShell,
                inactiveCore,
                flexOptionsCore.activeTick,
                flexOptionsCore.nextSetPointTick
              )
              inactive(emData, modelShell, inactiveCore)
            }
            .getOrElse {
              // more flex options expected
              awaitingCompletions(
                emData,
                modelShell,
                newCore,
              )
            }
        } else {
          allFlexMsgs.foreach { case (actor, msg) =>
            actor ! msg
          }

          awaitingCompletions(emData, modelShell, newCore)
        }

      } else {
        //ctx.log.info(s"[${flexOptionsCore.activeTick}] ${ctx.self}.awaitingFlexOptions.IssuePowerControl there are still missing ProvideFlexOptions -> we have to wait...")
        awaitingFlexOptions(
          emData,
          modelShell,
          flexOptionsCore,
        )
      }
  }

  /** Behavior of an [[EmAgent]] waiting for a flex control message to be
    * received in order to transition to the next behavior. This behavior should
    * only be used by EmAgents that are themselves EM-controlled.
    */
  private def awaitingFlexCtrl(
      emData: EmData,
      modelShell: EmModelShell,
      flexOptionsCore: EmDataCore.AwaitingFlexOptions,
  ): Behavior[Request] = Behaviors.receivePartial {
    case (ctx, Flex(flexCtrl: IssueFlexControl)) =>
      //ctx.log.info(s"Received $flexCtrl")
      val flexData = emData.parentData.getOrElse(
        throw new CriticalFailureException(s"EmAgent is not EM-controlled.")
      )

      // flex options calculated by this EmAgent
      val ownFlexOptions = flexData.lastFlexOptions.getOrElse(
        throw new CriticalFailureException(
          s"Flex options have not been calculated by EmAgent."
        )
      )

      val setPointActivePower = EmTools.determineFlexPower(
        ownFlexOptions,
        flexCtrl,
      )

      // flex options calculated by connected agents
      val receivedFlexOptions = flexOptionsCore.getFlexOptions

      val ctrlSetPoints =
        modelShell.determineFlexControl(
          receivedFlexOptions,
          setPointActivePower,
        )

      val (allFlexMsgs, newCore) = flexOptionsCore
        .handleFlexCtrl(ctrlSetPoints)
        .fillInMissingIssueCtrl()
        .complete()

      allFlexMsgs.foreach { case (actor, msg) =>
        actor ! msg
      }

      awaitingCompletions(emData, modelShell, newCore)
  }

  /** Behavior of an [[EmAgent]] waiting for completions messages to be received
    * in order to transition to the inactive behavior.
    */
  private def awaitingCompletions(
      emData: EmData,
      modelShell: EmModelShell,
      core: EmDataCore.AwaitingCompletions,
  ): Behavior[Request] = Behaviors.receiveMessagePartial {
    // Completions and results
    case completion: FlexCtrlCompletion =>
      val updatedCore = core.handleCompletion(completion)

      updatedCore
        .maybeComplete()
        .map { inactiveCore =>
          sendCompletionCommunication(
            emData,
            modelShell,
            inactiveCore,
            updatedCore.activeTick,
            updatedCore.nextSetPointTick
          )
          inactive(emData, modelShell, inactiveCore)
        }
        .getOrElse {
          // more flex options expected
          awaitingCompletions(
            emData,
            modelShell,
            updatedCore,
          )
        }

  }

  private def sendCompletionCommunication(
      emData: EmData,
      modelShell: EmModelShell,
      inactiveCore: EmDataCore.Inactive,
      lastActiveTick: Long,
      nextSetPointTick: Option[Long],
  ): Unit = {
    // calc result
    val result = inactiveCore.getResults
      .reduceOption { (power1, power2) =>
        ApparentPower(power1.p + power2.p, power1.q + power2.q)
      }
      .getOrElse(
        ApparentPower(
          zeroMW,
          zeroMVAr,
        )
      )

    val nextActiveTick = EmTools.minOptionTicks(
      inactiveCore.nextActiveTick,
      nextSetPointTick
    )

    emData.listener.foreach {
      _ ! ParticipantResultEvent(
        new EmResult(
          lastActiveTick
            .toDateTime(emData.simulationStartDate),
          modelShell.uuid,
          result.p.toMegawatts.asMegaWatt,
          result.q.toMegavars.asMegaVar,
        ),
        tick = lastActiveTick,
        nextTick = nextActiveTick
      )
    }


    emData.parentData.fold(
      schedulerData =>
        schedulerData.scheduler ! Completion(
          schedulerData.activationAdapter,
          nextActiveTick,
        ),
      _.emAgent ! FlexCtrlCompletion(
        modelShell.uuid,
        result,
        inactiveCore.hasFlexWithNext,
        inactiveCore.nextActiveTick,
      ),
    )
  }

  /** Data that is supposed to stay (mostly) constant during simulation
    *
    * @param outputConfig
    *   Config for the output behaviour of simulation results
    * @param simulationStartDate
    *   Date of the very first tick in the simulation
    * @param parentData
    *   Either a [[Right]] with [[FlexControlledData]] if this agent is
    *   em-controlled, or a [[Left]] with [[SchedulerData]]
    * @param listener
    *   A collection of result event listeners
    */
  private final case class EmData(
      outputConfig: NotifierConfig,
      simulationStartDate: ZonedDateTime,
      parentData: Either[SchedulerData, FlexControlledData],
      listener: Iterable[ActorRef[ResultEvent]],
      extEmDataServiceData: ExternalEmDataServiceData
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
      activationAdapter: ActorRef[Activation],
      lastFlexOptions: Option[ProvideFlexOptions] = None,
  )

  final case class ExternalEmDataServiceData(
                                              extEmDataService: Option[ClassicRef]
                                            )
}
