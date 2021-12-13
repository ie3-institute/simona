/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.api

import akka.actor.{Actor, ActorSystem, Props}
import edu.ie3.simona.akka.SimonaActorRef
import edu.ie3.simona.akka.SimonaActorRef.selfSingleton
import edu.ie3.simona.api.ExtMessageUtils.{
  RichExtCompletion,
  RichExtScheduleTrigger
}
import edu.ie3.simona.api.ExtSimAdapter.{
  ExtSimAdapterStateData,
  InitExtSimAdapter
}
import edu.ie3.simona.api.data.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.simulation.ExtSimAdapterData
import edu.ie3.simona.api.simulation.ontology.{
  ActivityStartTrigger => ExtActivityStartTrigger,
  CompletionMessage => ExtCompletionMessage
}
import edu.ie3.simona.logging.SimonaActorLogging
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeExtSimAdapterTrigger
}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK

object ExtSimAdapter {

  def props(scheduler: SimonaActorRef): Props =
    Props(
      new ExtSimAdapter(scheduler)
    )

  final case class InitExtSimAdapter(
      extSimData: ExtSimAdapterData
  )

  final case class ExtSimAdapterStateData(
      extSimData: ExtSimAdapterData,
      triggeredTicks: Map[Long, Long] = Map.empty // tick -> id
  )
}

final case class ExtSimAdapter(scheduler: SimonaActorRef)
    extends Actor
    with SimonaActorLogging {

  protected implicit val system: ActorSystem = context.system

  override def receive: Receive = {
    case TriggerWithIdMessage(
          InitializeExtSimAdapterTrigger(
            InitExtSimAdapter(extSimData)
          ),
          triggerId,
          _
        ) =>
      // triggering first time at init tick
      sender() ! CompletionMessage(
        triggerId,
        selfSingleton,
        Some(
          Seq(
            ScheduleTriggerMessage(
              ActivityStartTrigger(INIT_SIM_TICK),
              selfSingleton
            )
          )
        )
      )
      context become receiveIdle(
        ExtSimAdapterStateData(extSimData)
      )
  }

  def receiveIdle(implicit stateData: ExtSimAdapterStateData): Receive = {
    case TriggerWithIdMessage(ActivityStartTrigger(tick), triggerId, _) =>
      stateData.extSimData.queueExtMsg(
        new ExtActivityStartTrigger(tick)
      )
      log.debug(
        "Tick {} (trigger id {}) has been scheduled in external simulation",
        tick,
        triggerId
      )

      context become receiveIdle(
        stateData.copy(
          triggeredTicks = stateData.triggeredTicks + (tick -> triggerId)
        )
      )

    case extCompl: ExtCompletionMessage =>
      // when multiple triggers have been sent, a completion message
      // always refers to the oldest tick
      val (oldestTick, oldestTriggerId) = getOldestTickAndTriggerId.getOrElse(
        throw new RuntimeException("No tick has been triggered")
      )

      scheduler ! extCompl.toSimona(
        oldestTriggerId,
        selfSingleton
      )
      log.debug(
        "Tick {} (trigger id {}) has been completed in external simulation",
        oldestTick,
        oldestTriggerId
      )

      context become receiveIdle(
        stateData.copy(
          triggeredTicks = stateData.triggeredTicks - oldestTick
        )
      )

    case scheduleDataService: ScheduleDataServiceMessage =>
      val (oldestTick, _) = getOldestTickAndTriggerId.getOrElse(
        throw new RuntimeException("No tick has been triggered")
      )
      scheduler ! scheduleDataService.toSimona(oldestTick)

  }

  private def getOldestTickAndTriggerId(implicit
      stateData: ExtSimAdapterStateData
  ): Option[(Long, Long)] =
    stateData.triggeredTicks
      .minByOption(_._1)
}
