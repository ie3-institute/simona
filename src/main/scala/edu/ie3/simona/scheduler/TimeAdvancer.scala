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
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage._
import edu.ie3.simona.ontology.trigger.Trigger.ActivityStartTrigger
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK

/** TODO */
object TimeAdvancer {

  def apply(
      scheduler: actor.typed.ActorRef[SchedulerMessage],
      eventListener: Option[ActorRef[RuntimeEvent]],
      checkWindow: Long,
      endTick: Long
  ): Behavior[SchedulerMessage] = inactive(
    TimeAdvancerData(scheduler, endTick),
    eventListener.map(RuntimeNotifier(_, checkWindow)),
    INIT_SIM_TICK,
    0L
  )

  private def inactive(
      data: TimeAdvancerData,
      notifier: Option[RuntimeNotifier],
      nextActiveTick: Long,
      nextTriggerId: Long
  ): Behavior[SchedulerMessage] = Behaviors.receive {
    case (_, StartScheduleMessage(pauseSimAtTick)) =>
      val updatedNotifier = notifier.map {
        _.starting(
          nextActiveTick,
          pauseSimAtTick.getOrElse(data.endTick)
        )
      }

      data.scheduler ! TriggerWithIdMessage(
        ActivityStartTrigger(nextActiveTick),
        nextTriggerId
      )

      active(
        data,
        updatedNotifier,
        nextActiveTick,
        nextTriggerId,
        pauseSimAtTick
      )

    // FIXME handle STM as well
    // theoretically, we could receive STMs while inactive, so let's just handle them as well
  }

  private def active(
      data: TimeAdvancerData,
      notifier: Option[RuntimeNotifier],
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
                  val updatedNotifier = notifier.map { _.pausing(activeTick) }

                  // pause, inactivate
                  inactive(
                    data,
                    updatedNotifier,
                    nextTrig.trigger.tick,
                    nextTriggerId
                  )
                }
                .getOrElse {
                  val updatedNotifier = notifier.map { notifier =>
                    val notifierCompleted =
                      notifier.completing(nextTrig.trigger.tick - 1)

                    if (activeTick == INIT_SIM_TICK)
                      notifierCompleted.starting(
                        nextTrig.trigger.tick,
                        pauseSimAtTick.getOrElse(data.endTick)
                      )
                    else
                      notifierCompleted
                  }

                  // activate next
                  data.scheduler ! TriggerWithIdMessage(
                    nextTrig.trigger,
                    nextTriggerId
                  )
                  active(
                    data,
                    updatedNotifier,
                    nextTrig.trigger.tick,
                    nextTriggerId,
                    pauseSimAtTick
                  )
                }
            }
        }
        .getOrElse {
          notifier.foreach { _.finishing(activeTick, error = false) }

          Behaviors.stopped
        }

    // FIXME handle STM as well
    // this should rarely happen, but just in case an STM arrives right after a completion
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
