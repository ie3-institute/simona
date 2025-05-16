/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.api

import edu.ie3.simona.api.data.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.simulation.ExtSimAdapterData
import edu.ie3.simona.api.simulation.ontology.{
  ActivationMessage,
  ControlResponseMessageFromExt,
  TerminationCompleted,
  TerminationMessage,
  CompletionMessage => ExtCompletionMessage,
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ScheduleServiceActivation
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

import scala.jdk.OptionConverters._

object ExtSimAdapter {

  type Request = Create | Stop | Activation | ControlResponseMessageFromExt

  /** The [[ExtSimAdapterData]] can only be constructed once the ExtSimAdapter
    * actor is created. Thus, we need an extra initialization message.
    *
    * @param extSimData
    *   The [[ExtSimAdapterData]] of the corresponding external simulation
    */
  final case class Create(extSimData: ExtSimAdapterData, unlockKey: ScheduleKey)

  final case class Stop(simulationSuccessful: Boolean)

  final case class ExtSimAdapterStateData(
      extSimData: ExtSimAdapterData,
      currentTick: Option[Long] = None,
  )

  def apply(
      scheduler: ActorRef[SchedulerMessage]
  ): Behavior[Request] = initialize(using scheduler)

  private def initialize(using
      scheduler: ActorRef[SchedulerMessage]
  ): Behavior[Request] =
    Behaviors.receivePartial { case (ctx, Create(extSimData, unlockKey)) =>
      // triggering first time at init tick
      scheduler ! ScheduleActivation(
        ctx.self,
        INIT_SIM_TICK,
        Some(unlockKey),
      )

      receiveIdle(ExtSimAdapterStateData(extSimData))
    }

  private[api] def receiveIdle(stateData: ExtSimAdapterStateData)(using
      scheduler: ActorRef[SchedulerMessage]
  ): Behavior[Request] =
    Behaviors.receivePartial {
      case (ctx, Activation(tick)) =>
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

        scheduler ! Completion(ctx.self, newTick)
        ctx.log.debug(
          "Tick {} has been completed in external simulation",
          stateData.currentTick,
        )

        receiveIdle(stateData.copy(currentTick = None))

      case (
            ctx,
            scheduleDataService: ScheduleDataServiceMessage,
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
