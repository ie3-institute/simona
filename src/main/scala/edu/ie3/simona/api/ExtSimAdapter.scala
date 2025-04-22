/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.api

import edu.ie3.simona.api.ExtSimAdapter.{Create, ExtSimAdapterStateData, Stop}
import edu.ie3.simona.api.data.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.simulation.ExtSimAdapterData
import edu.ie3.simona.api.simulation.ontology.{
  ActivationMessage,
  ControlResponseMessageFromExt,
  TerminationCompleted,
  TerminationMessage,
  CompletionMessage => ExtCompletionMessage,
}
import edu.ie3.simona.logging.SimonaActorLogging
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ScheduleServiceActivation
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.ScheduleServiceActivation
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.actor.{Actor, ActorRef, PoisonPill, Props}
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

import scala.jdk.OptionConverters._

object ExtSimAdapter {

  sealed trait AdapterMessage extends ControlResponseMessageFromExt

  /** The [[ExtSimAdapterData]] can only be constructed once the ExtSimAdapter
    * actor is created. Thus, we need an extra initialization message.
    *
    * @param extSimData
    *   The [[ExtSimAdapterData]] of the corresponding external simulation
    */
  final case class Create(extSimData: ExtSimAdapterData, unlockKey: ScheduleKey)
      extends AdapterMessage

  final case class Stop(simulationSuccessful: Boolean) extends AdapterMessage

  final case class ExtSimAdapterStateData(
      extSimData: ExtSimAdapterData,
      currentTick: Option[Long] = None,
  ) extends AdapterMessage

  final case class WrappedActivation(activation: Activation)
      extends AdapterMessage
  final case class WrappedScheduleDataServiceMessage(
      msg: ScheduleDataServiceMessage
  ) extends AdapterMessage

  def adapter(
      ref: ActorRef[ControlResponseMessageFromExt]
  ): Behavior[ScheduleDataServiceMessage] = Behaviors.receiveMessagePartial {
    extMsg =>
      ref ! WrappedScheduleDataServiceMessage(extMsg)
      Behaviors.same
  }

  def apply(
      scheduler: ActorRef[SchedulerMessage]
  ): Behavior[ControlResponseMessageFromExt] = Behaviors.setup { ctx =>
    val activationAdapter = ctx.messageAdapter(WrappedActivation)
    initialize(scheduler, activationAdapter)
  }

  private def initialize(implicit
      scheduler: ActorRef[SchedulerMessage],
      activationAdapter: ActorRef[Activation],
  ): Behavior[ControlResponseMessageFromExt] = Behaviors.receiveMessage {
    case Create(extSimData, unlockKey) =>
      // triggering first time at init tick
      scheduler ! ScheduleActivation(
        activationAdapter,
        INIT_SIM_TICK,
        Some(unlockKey),
      )

      receiveIdle(ExtSimAdapterStateData(extSimData))
  }

  private[api] def receiveIdle(stateData: ExtSimAdapterStateData)(implicit
      scheduler: ActorRef[SchedulerMessage],
      activationAdapter: ActorRef[Activation],
  ): Behavior[ControlResponseMessageFromExt] = Behaviors.receive {
    case (ctx, WrappedActivation(Activation(tick))) =>
      stateData.extSimData.queueExtMsg(
        new ActivationMessage(tick)
      )
      ctx.log.debug(
        "Tick {} has been activated in external simulation",
        tick,
      )

      receiveIdle(stateData.copy(currentTick = Some(tick)))

    case (ctx, extCompl: ExtCompletionMessage) =>
      // when multiple triggers have been sent, a completion message
      // always refers to the oldest tick

      val newTick = extCompl.nextActivation().toScala.map(Long2long)

      scheduler ! Completion(activationAdapter, newTick)
      ctx.log.debug(
        "Tick {} has been completed in external simulation",
        stateData.currentTick,
      )

      receiveIdle(stateData.copy(currentTick = None))

    case (
          ctx,
          WrappedScheduleDataServiceMessage(
            scheduleDataService: ScheduleDataServiceMessage
          ),
        ) =>
      val tick = stateData.currentTick.getOrElse(
        throw new RuntimeException("No tick has been triggered")
      )
      val key = ScheduleLock.singleKey(ctx, scheduler, tick)

      scheduleDataService.dataService ! ScheduleServiceActivation(
        tick,
        key,
      )

      Behaviors.same

    case (_, Stop(simulationSuccessful)) =>
      // let external sim know that we have terminated
      stateData.extSimData.queueExtMsg(
        new TerminationMessage(simulationSuccessful)
      )

      Behaviors.same

    case (_, _: TerminationCompleted) =>
      // external simulation has terminated as well, we can exit
      Behaviors.stopped
  }

}
