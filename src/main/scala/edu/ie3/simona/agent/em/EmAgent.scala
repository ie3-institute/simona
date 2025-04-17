/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.em

import edu.ie3.datamodel.models.input.EmInput
import edu.ie3.datamodel.models.result.system.{EmResult, FlexOptionsResult}
import edu.ie3.simona.config.RuntimeConfig.EmRuntimeConfig
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.{
  FlexOptionsResultEvent,
  ParticipantResultEvent,
}
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.em.{EmModelShell, EmTools}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage._
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.service.Data.PrimaryData.ComplexPower
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities._
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

import java.time.ZonedDateTime
import scala.util.{Failure, Try}

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
  ): Behavior[Request] = Behaviors.setup[Request] { ctx =>
    val constantData = EmData(
      outputConfig,
      simulationStartDate,
      parent
        .map { parentEm =>
          val flexAdapter = ctx.messageAdapter[FlexRequest](Flex)

          parentEm ! RegisterControlledAsset(
            flexAdapter,
            inputModel,
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
      listener,
    )

    val modelShell = EmModelShell(
      inputModel.getUuid,
      inputModel.getId,
      modelStrategy,
      modelConfig,
    )

    inactive(
      constantData,
      modelShell,
      EmDataCore.create(simulationStartDate),
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

    case (_, RegisterControlledAsset(actor, spi)) =>
      val updatedModelShell = modelShell.addParticipant(spi.getUuid, spi)
      val updatedCore = core.addParticipant(actor, spi.getUuid)
      inactive(emData, updatedModelShell, updatedCore)

    case (_, ScheduleFlexActivation(participant, newTick, scheduleKey)) =>
      val (maybeSchedule, newCore) = core
        .handleSchedule(participant, newTick)

      maybeSchedule match {
        case Some(scheduleTick) =>
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
            _.emAgent ! ScheduleFlexActivation(
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
      val flexOptionsCore = core.activate(msg.tick)

      msg match {
        case Flex(_: FlexActivation) | EmActivation(_) =>
          val (toActivate, newCore) = flexOptionsCore.takeNewFlexRequests()
          toActivate.foreach {
            _ ! FlexActivation(msg.tick)
          }

          newCore.fold(
            awaitingFlexOptions(emData, modelShell, _),
            awaitingCompletions(emData, modelShell, _),
          )

        case Flex(_: IssueFlexControl) =>
          // We got sent a flex control message instead of a flex request,
          // this means that flex options must have not changed since
          // they were last calculated

          // Thus, we just jump to the appropriate place and forward the
          // control message there
          ctx.self ! msg

          awaitingFlexCtrl(emData, modelShell, flexOptionsCore)
      }

  }

  /** Behavior of an [[EmAgent]] waiting for flex options to be received in
    * order to transition to the next behavior.
    */
  private def awaitingFlexOptions(
      emData: EmData,
      modelShell: EmModelShell,
      flexOptionsCore: EmDataCore.AwaitingFlexOptions,
  ): Behavior[Request] = Behaviors.receiveMessagePartial {
    case provideFlex: ProvideFlexOptions =>
      val updatedCore = flexOptionsCore.handleFlexOptions(
        provideFlex.modelUuid,
        provideFlex.flexOptions,
      )

      if (updatedCore.isComplete) {

        val allFlexOptions = updatedCore.getFlexOptions

        val emFlexOptions =
          modelShell.aggregateFlexOptions(allFlexOptions)

        if (emData.outputConfig.flexResult) {
          val flexResult = new FlexOptionsResult(
            flexOptionsCore.activeTick.toDateTime(
              emData.simulationStartDate
            ),
            modelShell.uuid,
            emFlexOptions.ref.toMegawatts.asMegaWatt,
            emFlexOptions.min.toMegawatts.asMegaWatt,
            emFlexOptions.max.toMegawatts.asMegaWatt,
          )

          emData.listener.foreach {
            _ ! FlexOptionsResultEvent(flexResult)
          }
        }

        emData.parentData match {
          case Right(flexStateData) =>
            // provide aggregate flex options to parent
            flexStateData.emAgent ! ProvideFlexOptions(
              modelShell.uuid,
              emFlexOptions,
            )

            val updatedEmData = emData.copy(
              parentData = Right(
                flexStateData.copy(
                  lastFlexOptions = Some(emFlexOptions)
                )
              )
            )

            awaitingFlexCtrl(updatedEmData, modelShell, updatedCore)

          case Left(_) =>
            // We're not em-controlled ourselves,
            // always desire to come as close as possible to 0 kW
            val setPower = zeroKW

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
       can schedule themselves with their completions and inactive agents should
       be sleeping right now
     */
  }

  /** Behavior of an [[EmAgent]] waiting for a flex control message to be
    * received in order to transition to the next behavior. This behavior should
    * only be used by EmAgents that are themselves EM-controlled.
    */
  private def awaitingFlexCtrl(
      emData: EmData,
      modelShell: EmModelShell,
      flexOptionsCore: EmDataCore.AwaitingFlexOptions,
  ): Behavior[Request] = Behaviors.receiveMessagePartial {
    case Flex(flexCtrl: IssueFlexControl) =>
      val flexData = emData.parentData.getOrElse(
        throw new CriticalFailureException(s"EmAgent is not EM-controlled.")
      )

      // flex options calculated by this EmAgent
      val ownFlexOptions = flexData.lastFlexOptions.getOrElse(
        throw new CriticalFailureException(
          s"Flex options have not been calculated by EmAgent."
        )
      )

      val setPointActivePower =
        Try(EmTools.determineFlexPower(ownFlexOptions, flexCtrl))
          .recoverWith(exception =>
            Failure(
              new CriticalFailureException(
                s"Determining flex power failed for EmAgent ${modelShell.uuid}",
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
    case result: FlexResult =>
      val updatedCore = core.handleResult(result)

      awaitingCompletions(
        emData,
        modelShell,
        updatedCore,
      )

    case completion: FlexCompletion =>
      val updatedCore = core.handleCompletion(completion)

      updatedCore
        .maybeComplete()
        .map { inactiveCore =>
          sendCompletionCommunication(
            emData,
            modelShell,
            inactiveCore,
            lastActiveTick = updatedCore.activeTick,
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

  /** Completions have all been received, possibly send results and report to
    * parent
    */
  private def sendCompletionCommunication(
      emData: EmData,
      modelShell: EmModelShell,
      inactiveCore: EmDataCore.Inactive,
      lastActiveTick: Long,
  ): Unit = {
    // Sum up resulting power, if applicable.
    // After initialization, there are no results yet.
    val maybeResult = inactiveCore.getResults
      .reduceOption { (power1, power2) =>
        ComplexPower(power1.p + power2.p, power1.q + power2.q)
      }

    maybeResult.foreach { result =>
      emData.listener.foreach {
        _ ! ParticipantResultEvent(
          new EmResult(
            lastActiveTick
              .toDateTime(emData.simulationStartDate),
            modelShell.uuid,
            result.p.toMegawatts.asMegaWatt,
            result.q.toMegavars.asMegaVar,
          )
        )
      }

      emData.parentData.foreach {
        _.emAgent ! FlexResult(modelShell.uuid, result)
      }
    }

    emData.parentData.fold(
      schedulerData =>
        schedulerData.scheduler ! Completion(
          schedulerData.activationAdapter,
          inactiveCore.nextActiveTick,
        ),
      _.emAgent ! FlexCompletion(
        modelShell.uuid,
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
  )
}
