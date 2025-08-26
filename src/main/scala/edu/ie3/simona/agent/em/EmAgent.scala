/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.em

import edu.ie3.datamodel.models.input.EmInput
import edu.ie3.datamodel.models.result.system.EmResult
import edu.ie3.simona.config.RuntimeConfig.EmRuntimeConfig
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.{
  FlexOptionsResultEvent,
  ParticipantResultEvent,
}
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.em.EmModelShell
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.EmServiceRegistration
import edu.ie3.simona.ontology.messages.flex.FlexOptions
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.service.Data.PrimaryData.ComplexPower
import edu.ie3.simona.service.em.ExtEmDataService
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.QuantityUtils.*
import edu.ie3.util.scala.quantities.DefaultQuantities.*
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

import java.time.ZonedDateTime
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Try}

/** Energy management agent that receives flex options from and issues control
  * messages to connected agents
  * ([[edu.ie3.simona.agent.participant.ParticipantAgent]]s and subordinate
  * [[EmAgent]]s).
  */
object EmAgent {

  type Message = Activation | FlexRequest | FlexResponse

  /** Creates the initial [[Behavior]] for an [[EmAgent]] in an inactive state.
    *
    * @param inputModel
    *   The model for this agent.
    * @param modelConfig
    *   Configuration for this type of model.
    * @param modelStrategy
    *   The model strategy to use.
    * @param outputConfig
    *   Config for the output behaviour of simulation results.
    * @param simulationStartDate
    *   Date of the very first tick in the simulation.
    * @param parent
    *   Either a [[Right]] with a reference to the parent [[EmAgent]] if this
    *   agent is em-controlled, or a [[Left]] with a reference to the scheduler
    *   that is activating this agent.
    * @param listener
    *   A listener for result events.
    * @param emDataService
    *   An energy management service.
    */
  def apply(
      inputModel: EmInput,
      modelConfig: EmRuntimeConfig,
      outputConfig: NotifierConfig,
      modelStrategy: String,
      simulationStartDate: ZonedDateTime,
      parent: Either[ActorRef[SchedulerMessage], ActorRef[FlexResponse]],
      listener: ActorRef[ResultEvent],
      emDataService: Option[ActorRef[ExtEmDataService.Message]],
  ): Behavior[Message] = Behaviors.setup[Message] { ctx =>

    val parentData: Either[ActorRef[SchedulerMessage], ActorRef[FlexResponse]] =
      emDataService match {
        case Some(service) =>
          // since we have a service, it will replace the default agent communication
          given ActorContext[Message] = ctx

          val uuid = inputModel.getUuid

          service ! EmServiceRegistration(
            ctx.self,
            uuid,
            parent.toOption,
            inputModel.getControllingEm.toScala.map(_.getUuid),
          )

          // given to the parent
          val requestAdapter = ExtEmDataService.emServiceRequestAdapter(
            service,
            ctx.self,
          )

          val adaptedParent = parent match {
            case Left(_) =>
              uuid
            case Right(value) =>
              value
          }

          // used by this agent
          val responseAdapter = ExtEmDataService.emServiceResponseAdapter(
            service,
            adaptedParent,
          )

          parent.map {
            _ ! RegisterControlledAsset(
              requestAdapter,
              inputModel,
            )
          }

          Right(responseAdapter)

        case None =>
          parent.map {
            _ ! RegisterControlledAsset(
              ctx.self,
              inputModel,
            )
          }

          parent
      }

    val constantData = EmData(
      outputConfig,
      simulationStartDate,
      parentData,
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
      EmDataCore.create(using simulationStartDate),
    )
  }

  /** Behavior of an inactive [[EmAgent]], which waits for an activation or flex
    * request to be activated.
    */
  private def inactive(
      emData: EmData,
      modelShell: EmModelShell[?],
      core: EmDataCore.Inactive,
  ): Behavior[Message] = Behaviors.receivePartial {

    case (_, RegisterControlledAsset(actor, assetInput)) =>
      val updatedModelShell =
        modelShell.addControlledAsset(assetInput.getUuid, assetInput)
      val updatedCore = core.addControlledAsset(actor, assetInput.getUuid)
      inactive(emData, updatedModelShell, updatedCore)

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
      inactive(emData, modelShell, newCore)

    case (_, msg: Activation) =>
      activate(emData, modelShell, core, msg.tick)

    case (_, msg: FlexActivation) =>
      activate(emData, modelShell, core, msg.tick)

    case (ctx, msg: IssueFlexControl) =>
      val flexOptionsCore = core.activate(msg.tick)

      // We got sent a flex control message instead of a flex request,
      // this means that flex options must have not changed since
      // they were last calculated

      // Thus, we just jump to the appropriate place and forward the
      // control message there
      ctx.self ! msg

      awaitingFlexCtrl(emData, modelShell, flexOptionsCore)

  }

  private def activate(
      emData: EmData,
      modelShell: EmModelShell[?],
      core: EmDataCore.Inactive,
      tick: Long,
  ) = {
    val flexOptionsCore = core.activate(tick)

    val (toActivate, newCore) = flexOptionsCore.takeNewFlexRequests()
    toActivate.foreach {
      _ ! FlexActivation(tick, modelShell.getFlexType)
    }

    newCore.fold(
      awaitingFlexOptions(emData, modelShell, _),
      awaitingCompletions(emData, modelShell, _),
    )
  }

  /** Behavior of an [[EmAgent]] waiting for flex options to be received in
    * order to transition to the next behavior.
    */
  private def awaitingFlexOptions(
      emData: EmData,
      modelShell: EmModelShell[?],
      flexOptionsCore: EmDataCore.AwaitingFlexOptions,
  ): Behavior[Message] = Behaviors.receiveMessagePartial {
    case provideFlex: ProvideFlexOptions =>
      val updatedCore = flexOptionsCore.handleFlexOptions(
        provideFlex.modelUuid,
        provideFlex.flexOptions,
      )

      if (updatedCore.isComplete) {

        val allFlexOptions = updatedCore.getFlexOptions

        val updatedModelShell =
          modelShell.updateAggregatedFlexOptions(allFlexOptions)

        if (emData.outputConfig.flexResult) {
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

            awaitingFlexCtrl(emData, updatedModelShell, updatedCore)

          case Left(_) =>
            // We're not em-controlled ourselves,
            // always desire to come as close as possible to 0 kW
            val setPower = zeroKW

            val flexControl =
              updatedModelShell.determineFlexControl(allFlexOptions, setPower)

            val (allFlexMsgs, newCore) = updatedCore
              .handleFlexCtrl(flexControl)
              .fillInMissingIssueCtrl()
              .complete()

            allFlexMsgs.foreach { case (actor, msg) =>
              actor ! msg
            }

            awaitingCompletions(emData, updatedModelShell, newCore)
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
      modelShell: EmModelShell[?],
      flexOptionsCore: EmDataCore.AwaitingFlexOptions,
  ): Behavior[Message] = Behaviors.receiveMessagePartial {
    case flexCtrl: IssueFlexControl =>
      val setPointActivePower =
        Try(modelShell.determineFlexPower(flexCtrl))
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
      modelShell: EmModelShell[?],
      core: EmDataCore.AwaitingCompletions,
  ): Behavior[Message] = Behaviors.receivePartial {
    case (_, result: FlexResult) =>
      val updatedCore = core.handleResult(result)

      awaitingCompletions(
        emData,
        modelShell,
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

  /** Data that is supposed to stay (mostly) constant during simulation.
    *
    * @param outputConfig
    *   Config for the output behaviour of simulation results.
    * @param simulationStartDate
    *   Date of the very first tick in the simulation.
    * @param parent
    *   Either a [[Right]] with a reference to the parent [[EmAgent]] if this
    *   agent is em-controlled, or a [[Left]] with a reference to the scheduler
    *   that is activating this agent.
    * @param listener
    *   A listener for result events.
    */
  private final case class EmData(
      outputConfig: NotifierConfig,
      simulationStartDate: ZonedDateTime,
      parent: Either[ActorRef[SchedulerMessage], ActorRef[FlexResponse]],
      listener: ActorRef[ResultEvent],
  )

  /** The existence of this data object indicates that the corresponding agent
    * is EM-controlled (by [[emAgent]]).
    *
    * @param emAgent
    *   The parent EmAgent that is controlling this agent.
    * @param lastFlexOptions
    *   Last flex options that have been calculated for this agent.
    */
  final case class FlexControlledData(
      emAgent: ActorRef[FlexResponse],
      lastFlexOptions: Option[FlexOptions] = None,
  )

}
