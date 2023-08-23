/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.api

import akka.actor.ActorRef
import edu.ie3.simona.api.data.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.simulation.ontology.{
  CompletionMessage => ExtCompletionMessage
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage
}
import edu.ie3.simona.ontology.trigger.Trigger.ActivityStartTrigger

import scala.jdk.CollectionConverters.CollectionHasAsScala

object ExtMessageUtils {
  implicit class RichExtCompletion(
      private val extCompl: ExtCompletionMessage
  ) {
    def toSimona(triggerId: Long, triggerActor: ActorRef): CompletionMessage = {
      val newTrigger =
        // FIXME ext CompletionMessage should have only one next trigger
        extCompl.newTriggers.asScala.headOption.map { tick =>
          ScheduleTriggerMessage(ActivityStartTrigger(tick), triggerActor)
        }

      CompletionMessage(
        triggerId,
        newTrigger
      )
    }
  }

  implicit class RichExtScheduleTrigger(
      private val sched: ScheduleDataServiceMessage
  ) {
    def toSimona(tick: Long): ScheduleTriggerMessage =
      ScheduleTriggerMessage(
        ActivityStartTrigger(tick),
        sched.getDataService
      )
  }
}
