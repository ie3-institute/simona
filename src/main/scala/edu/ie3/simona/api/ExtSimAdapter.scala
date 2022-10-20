/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.api

import akka.actor.{Actor, ActorRef, PoisonPill, Props}
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
  Terminate,
  TerminationCompleted,
  ActivityStartTrigger => ExtActivityStartTrigger,
  CompletionMessage => ExtCompletionMessage
}
import edu.ie3.simona.logging.SimonaActorLogging
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.messages.StopMessage
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeExtSimAdapterTrigger
}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK

object ExtSimAdapter {

  def props(scheduler: ActorRef): Props =
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

final case class ExtSimAdapter(scheduler: ActorRef)
    extends Actor
    with SimonaActorLogging {
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
        Some(
          Seq(
            ScheduleTriggerMessage(
              ActivityStartTrigger(INIT_SIM_TICK),
              self,
              priority = true
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
        self
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

    case StopMessage(simulationSuccessful) =>
      // let external sim know that we have terminated
      stateData.extSimData.queueExtMsg(new Terminate(simulationSuccessful))

    case _: TerminationCompleted =>
      // external simulation has terminated as well, we can exit
      self ! PoisonPill
  }

  private def getOldestTickAndTriggerId(implicit
      stateData: ExtSimAdapterStateData
  ): Option[(Long, Long)] =
    stateData.triggeredTicks
      .minByOption(_._1)
}
