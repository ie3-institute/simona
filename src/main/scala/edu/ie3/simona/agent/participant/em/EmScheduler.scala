/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import akka.actor.{Actor, ActorRef, Props}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.trigger.Trigger.ActivityStartTrigger

object EmScheduler {
  def props(
      parent: ActorRef,
      simonaTimeConfig: SimonaConfig.Simona.Time
  ): Props =
    Props(
      EmScheduler(
        parent,
        simonaTimeConfig
      )
    )
}

final case class EmScheduler(
    parent: ActorRef,
    simonaTimeConfig: SimonaConfig.Simona.Time
) extends Actor
    with EmSchedulerHelper {

  def receive: Receive = receiveIdle(EmSchedulerStateData())

  def receiveIdle(stateData: EmSchedulerStateData): Receive = {
    /* schedule new triggers */
    case triggerToSchedule: ScheduleTriggerMessage =>
      context become receiveIdle(
        sendEligibleTrigger(
          scheduleTrigger(triggerToSchedule, stateData)
        )
      )

    /* process completion messages */
    case completionMessage: CompletionMessage =>
      // there can be new triggers for the current tick, which need to be sent out immediately
      val updatedStateData = sendEligibleTrigger(
        handleCompletionMessage(completionMessage, stateData)
      )

      context become receiveIdle(
        maybeTicksCompleted(updatedStateData)
      )

    case TriggerWithIdMessage(ActivityStartTrigger(newTick), triggerId, _) =>
      val updatedStateData = stateData.copy(
        nowInTicks = newTick,
        mainTrigger = stateData.mainTrigger +
          (newTick -> Some(triggerId))
      )

      context become receiveIdle(
        sendEligibleTrigger(updatedStateData)
      )

  }

}
