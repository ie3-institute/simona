/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.em

import edu.ie3.datamodel.models.result.system.EmResult
import edu.ie3.simona.agent.DataInputHandler
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.{FlexOptionsResultEvent, ParticipantResultEvent}
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.em.EmModelShell
import edu.ie3.simona.ontology.messages.AgentMessage.{ActivationRequest, force, tick}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{Completion, ScheduleActivation}
import edu.ie3.simona.ontology.messages.ServiceMessage.{DataMessage, EmFlexMessage}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage, ServiceMessage}
import edu.ie3.simona.service.Data.PrimaryData.ComplexPower
import edu.ie3.simona.service.em.ExtEmDataService
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.simona.util.TickUtil.toDateTime
import edu.ie3.util.quantities.QuantityUtils.*
import edu.ie3.util.scala.quantities.DefaultQuantities.*
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import squants.Power

import java.time.ZonedDateTime
import java.util.UUID
import scala.util.{Failure, Try}

/** Energy management agent that receives flex options from and issues control
  * messages to connected agents
  * ([[edu.ie3.simona.agent.participant.ParticipantAgent]]s and subordinate
  * [[EmAgent]]s).
  */
object EmAgent {

  type Message = Activation | FlexRequest | FlexResponse |
    ServiceMessage.Response

  /** Data that is supposed to stay (mostly) constant during simulation.
    *
    * @param outputConfig
    *   Config for the output behavior of simulation results.
    * @param simulationStartDate
    *   Date of the very first tick in the simulation.
    * @param parent
    *   Either a [[Right]] with a reference to the parent [[EmAgent]] if this
    *   agent is em-controlled, or a [[Left]] with a reference to the scheduler
    *   that is activating this agent.
    * @param listener
    *   A listener for result events.
    */
  final case class EmData(
      outputConfig: NotifierConfig,
      simulationStartDate: ZonedDateTime,
      parent: Either[ActorRef[SchedulerMessage], ActorRef[FlexResponse]],
      listener: ActorRef[ResultEvent],
      emService: Option[ActorRef[ExtEmDataService.Message]]
  )

  /** Behavior of an inactive [[EmAgent]], which waits for an activation or flex
    * request to be activated.
    */
  def inactive(
      emData: EmData,
      modelShell: EmModelShell[?],
      inputHandler: DataInputHandler,
      core: EmDataCore.Inactive,
  ): Behavior[Message] = Behaviors.receivePartial {

    case (_, RegisterControlledAsset(actor, assetInput)) =>
      val updatedModelShell =
        modelShell.addControlledAsset(assetInput.getUuid, assetInput)
      val updatedCore = core.addControlledAsset(actor, assetInput.getUuid)
      inactive(emData, updatedModelShell, inputHandler, updatedCore)

    case (ctx, ScheduleFlexActivation(participant, newTick, scheduleKey)) =>
      val (maybeSchedule, newCore) = core
        .handleSchedule(participant, newTick)

      maybeSchedule match {
        case Some(scheduleTick) =>
          // also potentially schedule with parent if the new earliest tick is
          // different from the old earliest tick (including if nothing had
          // been scheduled before)
          emData.parent.fold(
            _ ! ScheduleActivation(
              ctx.self,
              scheduleTick,
              scheduleKey,
            ),
            _ ! ScheduleFlexActivation(
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
      inactive(emData, modelShell, inputHandler, newCore)

    case (ctx, msg: IssueFlexControl) =>
      val flexOptionsCore = core.activate(msg.tick)

      // We got sent a flex control message instead of a flex request,
      // this means that flex options must have not changed since
      // they were last calculated

      // Thus, we just jump to the appropriate place and forward the
      // control message there
      ctx.self ! msg

      awaitingFlexCtrl(emData, modelShell, inputHandler, flexOptionsCore)

    // other activations besides IssueFlexControl
    case (_, msg: ActivationRequest) =>
      activate(emData, modelShell, inputHandler, core, msg)

    case (_, msg: DataMessage) =>
      inactive(emData, modelShell, inputHandler.handleDataMessage(msg), core)

    case (ctx, unhandled) =>
      ctx.log.warn(s"Unhandled (inactive): $unhandled")
      Behaviors.same
  }

  private def activate(
      emData: EmData,
      modelShell: EmModelShell[?],
      inputHandler: DataInputHandler,
      core: EmDataCore.Inactive,
      msg: ActivationRequest,
  ): Behavior[Message] = {
    val tick = msg.tick
    val force = msg.force

    val flexOptionsCore = if force then {
      core.gotoTick(tick).activateAll(tick)
    } else core.activate(tick)

    val (toActivate, newCore) = flexOptionsCore.takeNewFlexRequests()

    msg match {
      case flexInit: FlexInit =>
        // validate initialization message
        modelShell.validateInit(flexInit)
      case _ =>
      // no validation to do
    }

    val activationMsg = msg.tick match {
      case INIT_SIM_TICK =>
        FlexInit(modelShell.getFlexType, modelShell.getDataTimeType)
      case _ => FlexActivation(msg.tick, force)
    }
    toActivate.foreach(_ ! activationMsg)

    newCore.fold(
      awaitingFlexOptions(emData, modelShell, inputHandler, _),
      awaitingCompletions(emData, modelShell, inputHandler, _),
    )
  }

  /** Behavior of an [[EmAgent]] waiting for flex options to be received in
    * order to transition to the next behavior.
    */
  private def awaitingFlexOptions(
      emData: EmData,
      modelShell: EmModelShell[?],
      inputHandler: DataInputHandler,
      flexOptionsCore: EmDataCore.AwaitingFlexOptions,
  ): Behavior[Message] = Behaviors.receivePartial {
    case (ctx, provideFlex: ProvideFlexOptions) =>
      val updatedCore = flexOptionsCore.handleFlexOptions(
        provideFlex.modelUuid,
        provideFlex.flexOptions,
      )

      maybeDetermineFlex(emData, modelShell, inputHandler, updatedCore)

    case (ctx, msg: DataMessage) =>
      val updatedInputHandler = inputHandler.handleDataMessage(msg)

      maybeDetermineFlex(
        emData,
        modelShell,
        updatedInputHandler,
        flexOptionsCore,
      )

    /* We do not need to handle ScheduleFlexRequests here, since active agents
       can schedule themselves with their completions and inactive agents should
       be sleeping right now
     */
  }

  private def maybeDetermineFlex(
      emData: EmData,
      modelShell: EmModelShell[?],
      inputHandler: DataInputHandler,
      flexOptionsCore: EmDataCore.AwaitingFlexOptions,
  ): Behavior[Message] = {
    // we need the expected secondary data to be received here
    // even if we're em-controlled in order to make things not too complicated
    if flexOptionsCore.isComplete && inputHandler.allMessagesReceived(
        flexOptionsCore.activeTick
      )
    then {

      val allFlexOptions = flexOptionsCore.getFlexOptions

      val updatedModelShell =
        modelShell.updateAggregatedFlexOptions(allFlexOptions)

      if emData.outputConfig.flexResult then {
        val flexResult = updatedModelShell.determineResults(
          flexOptionsCore.activeTick.toDateTime(using
            emData.simulationStartDate
          )
        )

        emData.listener ! FlexOptionsResultEvent(flexResult)
      }

      emData.parent match {
        case Right(parentEm) =>
          // provide aggregate flex options to parent
          parentEm ! ProvideFlexOptions(
            updatedModelShell.uuid,
            updatedModelShell.getFlexOptions,
          )

          awaitingFlexCtrl(
            emData,
            updatedModelShell,
            inputHandler,
            flexOptionsCore,
          )

        case Left(_) =>
          // We're not em-controlled ourselves,
          // always desire to come as close as possible to 0 kW
          val setPower = zeroKW

          val flexControl =
            updatedModelShell.determineFlexControl(
              allFlexOptions,
              setPower,
              flexOptionsCore.activeTick,
              inputHandler.getSecondaryData,
            )

          val (allFlexMsgs, newCore) = flexOptionsCore
            .handleFlexCtrl(flexControl)
            .fillInMissingIssueCtrl()
            .complete()

          allFlexMsgs.foreach { case (actor, msg) =>
            actor ! msg
          }

          awaitingCompletions(
            emData,
            updatedModelShell,
            inputHandler,
            newCore,
          )
      }

    } else {
      emData.emService.foreach(_ ! EmFlexMessage(WaitingForData(modelShell.uuid), modelShell.uuid))

      // more flex options expected
      awaitingFlexOptions(
        emData,
        modelShell,
        inputHandler,
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
      modelShell: EmModelShell[?],
      inputHandler: DataInputHandler,
      flexOptionsCore: EmDataCore.AwaitingFlexOptions,
  ): Behavior[Message] = Behaviors.receivePartial {
    case (_, IssueDisaggregatedControl(_, setPowers)) =>
      handleFlexControl(
        emData,
        modelShell,
        inputHandler,
        flexOptionsCore,
        setPowers,
      )

    case (_, flexCtrl: IssueFlexControl) =>
      val setPointActivePower =
        Try(modelShell.determineFlexPower(flexCtrl))
          .recoverWith(exception =>
            Failure(
              new CriticalFailureException(
                s"${modelShell.identifier}: Determining flex power failed",
                exception,
              )
            )
          )
          .get

      // flex options calculated by connected agents
      val receivedFlexOptions = flexOptionsCore.getFlexOptions

      val ctrlSetPoints =
        modelShell.determineFlexControl(
          receivedFlexOptions,
          setPointActivePower,
          flexCtrl.tick,
          inputHandler.getSecondaryData,
        )

      handleFlexControl(
        emData,
        modelShell,
        inputHandler,
        flexOptionsCore,
        ctrlSetPoints,
      )

    case (ctx, unhandled) =>
      ctx.log.warn(s"Unhandled (awaiting control): $unhandled")
      Behaviors.same
  }

  private def handleFlexControl(
      emData: EmData,
      modelShell: EmModelShell[?],
      inputHandler: DataInputHandler,
      flexOptionsCore: EmDataCore.AwaitingFlexOptions,
      ctrlSetPoints: Iterable[(UUID, Power)],
  ): Behavior[Message] = {
    val (allFlexMsgs, newCore) = flexOptionsCore
      .handleFlexCtrl(ctrlSetPoints)
      .fillInMissingIssueCtrl()
      .complete()

    allFlexMsgs.foreach { case (actor, msg) =>
      actor ! msg
    }

    awaitingCompletions(emData, modelShell, inputHandler, newCore)
  }

  /** Behavior of an [[EmAgent]] waiting for completions messages to be received
    * in order to transition to the inactive behavior.
    */
  private def awaitingCompletions(
      emData: EmData,
      modelShell: EmModelShell[?],
      inputHandler: DataInputHandler,
      core: EmDataCore.AwaitingCompletions,
  ): Behavior[Message] = Behaviors.receivePartial {
    case (_, result: FlexResult) =>
      val updatedCore = core.handleResult(result)

      awaitingCompletions(
        emData,
        modelShell,
        inputHandler,
        updatedCore,
      )

    case (ctx, completion: FlexCompletion) =>
      val updatedCore = core.handleCompletion(completion)

      updatedCore
        .maybeComplete()
        .map { inactiveCore =>
          sendCompletionCommunication(
            emData,
            modelShell,
            inactiveCore,
            lastActiveTick = updatedCore.activeTick,
          )(using ctx.self)
          inactive(emData, modelShell, inputHandler, inactiveCore)
        }
        .getOrElse {
          // more flex options expected
          awaitingCompletions(
            emData,
            modelShell,
            inputHandler,
            updatedCore,
          )
        }

    case (ctx, x) =>
      ctx.log.warn(
        s"AwaitingCompletion: (${modelShell.id}, tick: ${core.activeTick}) Could not handle $x"
      )
      Behaviors.same
  }

  /** Completions have all been received, possibly send results and report to
    * parent.
    */
  private def sendCompletionCommunication(
      emData: EmData,
      modelShell: EmModelShell[?],
      inactiveCore: EmDataCore.Inactive,
      lastActiveTick: Long,
  )(using self: ActorRef[Message]): Unit = {
    // Sum up resulting power, if applicable.
    // After initialization, there are no results yet.
    val maybeResult = inactiveCore.getResults
      .reduceOption { (power1, power2) =>
        ComplexPower(power1.p + power2.p, power1.q + power2.q)
      }

    maybeResult.foreach { result =>
      emData.listener ! ParticipantResultEvent(
        new EmResult(
          lastActiveTick
            .toDateTime(using emData.simulationStartDate),
          modelShell.uuid,
          result.p.toMegawatts.asMegaWatt,
          result.q.toMegavars.asMegaVar,
        )
      )

      emData.parent.foreach {
        _ ! FlexResult(modelShell.uuid, result)
      }
    }

    // we do not take possible next data ticks into consideration,
    // as we don't need to be activated only for secondary data
    emData.parent.fold(
      _ ! Completion(
        self,
        inactiveCore.nextActiveTick,
      ),
      _ ! FlexCompletion(
        modelShell.uuid,
        inactiveCore.hasFlexWithNext,
        inactiveCore.nextActiveTick,
      ),
    )
  }

}
