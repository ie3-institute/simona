/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.api

import edu.ie3.simona.akka.SimonaActorRef
import edu.ie3.simona.akka.SimonaActorRef.RichActorRef
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
    def toSimona(
        triggerId: Long,
        triggerActor: SimonaActorRef
    ): CompletionMessage = {
      val newTriggers =
        Option.when(!extCompl.getNewTriggers.isEmpty) {
          extCompl.getNewTriggers.asScala.map { tick =>
            ScheduleTriggerMessage(ActivityStartTrigger(tick), triggerActor)
          }.toSeq
        }

      CompletionMessage(
        triggerId,
        triggerActor,
        newTriggers
      )
    }
  }

  implicit class RichExtScheduleTrigger(
      private val sched: ScheduleDataServiceMessage
  ) {
    def toSimona(tick: Long): ScheduleTriggerMessage =
      ScheduleTriggerMessage(
        ActivityStartTrigger(tick),
        sched.getDataService.asLocal // TODO all ActorRefs have to be removed from SimonaAPI
      )
  }
}
