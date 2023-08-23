/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import edu.ie3.simona.actor.ActorUtil.stopOnError
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.RuntimeEvent.{Initializing, Simulating}
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage._
import edu.ie3.simona.ontology.trigger.Trigger.ActivityStartTrigger
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK

class TimeAdvancer {

  // notify listener as well

  def apply(
      scheduler: actor.typed.ActorRef[SchedulerMessage],
      eventListener: Option[ActorRef[RuntimeEvent]],
      endTick: Long
  ): Behavior[SchedulerMessage] = Behaviors.receive {
    case (ctx, InitTimeAdvancer(autoStart)) =>
      if (autoStart)
        ctx.self ! StartScheduleMessage()
      inactive(
        TimeAdvancerData(scheduler, eventListener, endTick),
        INIT_SIM_TICK,
        0L
      )
  }

  private def inactive(
      data: TimeAdvancerData,
      nextActiveTick: Long,
      nextTriggerId: Long
  ): Behavior[SchedulerMessage] = Behaviors.receive {
    case (_, StartScheduleMessage(pauseSimAtTick)) =>
      data.scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(nextActiveTick),
        nextTriggerId
      )
      // TODO continue
      data.eventListener.foreach {
        _ ! (nextActiveTick match {
          case INIT_SIM_TICK => Initializing
          case _ =>
            Simulating(nextActiveTick, pauseSimAtTick.getOrElse(data.endTick))
        })
      }

      active(
        data,
        nextActiveTick,
        nextTriggerId,
        pauseSimAtTick
      )
  }

  private def active(
      data: TimeAdvancerData,
      activeTick: Long,
      expectedTriggerId: Long,
      pauseSimAtTick: Option[Long]
  ): Behavior[SchedulerMessage] = Behaviors.receive {
    case (ctx, CompletionMessage(triggerId, nextTriggers)) =>
      nextTriggers
        .map { nextTrig =>
          val nextTriggerId = triggerId + 1L
          checkCompletion(activeTick, expectedTriggerId, nextTrig, triggerId)
            .map(stopOnError(ctx, _))
            .getOrElse {
              pauseSimAtTick
                .filter(_ > nextTrig.trigger.tick)
                .map { _ =>
                  // pause, inactivate
                  inactive(
                    data,
                    nextTrig.trigger.tick,
                    nextTriggerId
                  )
                }
                .getOrElse {
                  // activate next
                  data.scheduler ! TriggerWithIdMessage(
                    nextTrig.trigger,
                    nextTriggerId
                  )
                  active(
                    data,
                    nextTrig.trigger.tick,
                    nextTriggerId,
                    pauseSimAtTick
                  )
                }
            }
        }
        .getOrElse {
          stopOnError(
            ctx,
            s"No next tick returned after tick $activeTick completed."
          )
        }
  }

  private def checkCompletion(
      activeTick: Long,
      expectedTriggerId: Long,
      nextTrig: ScheduleTriggerMessage,
      triggerId: Long
  ): Option[String] =
    Option
      .when(triggerId != expectedTriggerId) {
        s"Received completion message with trigger id $triggerId, although $expectedTriggerId was expected."
      }
      .orElse {
        Option.when(nextTrig.trigger.tick <= activeTick) {
          s"The next trigger has tick ${nextTrig.trigger.tick}, although current active tick was $activeTick."
        }
      }
}
