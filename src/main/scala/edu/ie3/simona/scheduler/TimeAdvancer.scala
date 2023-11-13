/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.scaladsl.adapter.ClassicActorRefOps
import akka.actor.typed.{ActorRef, Behavior}
import edu.ie3.simona.actor.ActorUtil.stopOnError
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage._
import edu.ie3.simona.ontology.trigger.Trigger.ActivityStartTrigger
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK

/** Unit that is in control of time advancement within the simulation.
  * Represents the root entity of any scheduler hierarchy.
  */
object TimeAdvancer {

  /** @param simulation
    *   The root actor of the simulation
    * @param eventListener
    *   listener that receives runtime events
    * @param checkWindow
    *   interval in which check window messages are sent
    * @param endTick
    *   last tick of the simulation
    */
  def apply(
      simulation: akka.actor.ActorRef,
      eventListener: Option[ActorRef[RuntimeEvent]],
      checkWindow: Option[Int],
      endTick: Long
  ): Behavior[SchedulerMessage] = Behaviors.receivePartial {
    case (_, ScheduleTriggerMessage(trigger, actorToBeScheduled)) =>
      inactive(
        TimeAdvancerData(simulation, actorToBeScheduled.toTyped, endTick),
        eventListener.map(RuntimeNotifier(_, checkWindow)),
        trigger.tick,
        trigger.tick,
        0L
      )

    case (ctx, Stop(errorMsg: String)) =>
      endWithFailure(ctx, simulation, None, INIT_SIM_TICK, errorMsg)
  }

  /** TimeAdvancer is inactive and waiting for a StartScheduleMessage to start
    * or continue
    * @param data
    *   constant time advancer data
    * @param notifier
    *   notifier for runtime events
    * @param startingTick
    *   tick that the simulation continues with at the beginning/after the last
    *   pause
    * @param nextActiveTick
    *   tick that the schedulee wants to be activated for next
    * @param nextTriggerId
    *   next trigger id to use for activation
    */
  private def inactive(
      data: TimeAdvancerData,
      notifier: Option[RuntimeNotifier],
      startingTick: Long,
      nextActiveTick: Long,
      nextTriggerId: Long
  ): Behavior[SchedulerMessage] = Behaviors.receivePartial {
    case (_, StartScheduleMessage(pauseTick)) =>
      val updatedNotifier = notifier.map {
        _.starting(
          startingTick,
          pauseTick,
          data.endTick
        )
      }

      data.schedulee ! TriggerWithIdMessage(
        ActivityStartTrigger(nextActiveTick),
        nextTriggerId
      )

      active(
        data,
        updatedNotifier,
        nextActiveTick,
        nextTriggerId,
        pauseTick
      )

    case (ctx, Stop(errorMsg: String)) =>
      endWithFailure(ctx, data.simulation, notifier, startingTick, errorMsg)
  }

  /** TimeAdvancer is active and waiting for the current activation of the
    * schedulee to complete
    *
    * @param data
    *   constant time advancer data
    * @param notifier
    *   notifier for runtime events
    * @param activeTick
    *   tick that is currently active
    * @param expectedTriggerId
    *   the trigger id that we expect to receive with completion
    * @param pauseTick
    *   the tick that we should pause at (if applicable)
    */
  private def active(
      data: TimeAdvancerData,
      notifier: Option[RuntimeNotifier],
      activeTick: Long,
      expectedTriggerId: Long,
      pauseTick: Option[Long]
  ): Behavior[SchedulerMessage] = Behaviors.receivePartial {
    case (ctx, CompletionMessage(triggerId, nextTrigger)) =>
      checkCompletion(activeTick, expectedTriggerId, nextTrigger, triggerId)
        .map(endWithFailure(ctx, data.simulation, notifier, activeTick, _))
        .getOrElse {
          val nextTriggerId = triggerId + 1L

          (nextTrigger, pauseTick) match {
            case (Some(nextTrig), _) if nextTrig.trigger.tick > data.endTick =>
              // next tick is after endTick, finish simulation
              endSuccessfully(data, notifier)

            case (Some(nextTrig), Some(pauseTick))
                if nextTrig.trigger.tick > pauseTick =>
              // next tick is after pause tick, pause sim
              val updatedNotifier = notifier.map {
                _.completing(
                  math.min(nextTrig.trigger.tick - 1, pauseTick)
                )
                  .pausing(pauseTick)
              }

              // pause, inactivate
              inactive(
                data,
                updatedNotifier,
                pauseTick + 1,
                nextTrig.trigger.tick,
                nextTriggerId
              )

            case (Some(nextTrig), _) =>
              // next tick is ok, continue
              val updatedNotifier = notifier.map { notifier =>
                val notifierCompleted =
                  notifier.completing(nextTrig.trigger.tick - 1)

                if (activeTick == INIT_SIM_TICK)
                  notifierCompleted.starting(
                    nextTrig.trigger.tick,
                    pauseTick,
                    data.endTick
                  )
                else
                  notifierCompleted
              }

              // activate next
              data.schedulee ! TriggerWithIdMessage(
                nextTrig.trigger,
                nextTriggerId
              )
              active(
                data,
                updatedNotifier,
                nextTrig.trigger.tick,
                nextTriggerId,
                pauseTick
              )

            case (None, _) =>
              // there is no next tick, finish
              ctx.log.info("No next tick supplied, stopping simulation.")
              endSuccessfully(data, notifier)

          }
        }

    case (ctx, Stop(errorMsg: String)) =>
      endWithFailure(ctx, data.simulation, notifier, activeTick, errorMsg)

  }

  private def endSuccessfully(
      data: TimeAdvancerData,
      notifier: Option[RuntimeNotifier]
  ): Behavior[SchedulerMessage] = {
    data.simulation ! SimulationSuccessfulMessage

    notifier.foreach {
      // we do not want a check window message for the endTick
      _.completing(data.endTick - 1)
        .finishing(data.endTick)
    }

    // we do not stop here, but wait until we are terminated
    Behaviors.empty
  }

  private def endWithFailure(
      ctx: ActorContext[SchedulerMessage],
      simulation: akka.actor.ActorRef,
      notifier: Option[RuntimeNotifier],
      tick: Long,
      errorMsg: String
  ): Behavior[SchedulerMessage] = {
    simulation ! SimulationFailureMessage
    notifier.foreach(_.error(tick, errorMsg))

    stopOnError(ctx, errorMsg)
  }

  private def checkCompletion(
      activeTick: Long,
      expectedTriggerId: Long,
      nextTrigger: Option[ScheduleTriggerMessage],
      triggerId: Long
  ): Option[String] =
    Option
      .when(triggerId != expectedTriggerId) {
        s"Received completion message with trigger id $triggerId, although $expectedTriggerId was expected."
      }
      .orElse {
        nextTrigger.filter(_.trigger.tick <= activeTick).map { nextTrig =>
          s"The next trigger has tick ${nextTrig.trigger.tick}, although current active tick was $activeTick."
        }
      }

  /** This data container stores objects that are not supposed to change for a
    * [[TimeAdvancer]] during simulation
    *
    * @param simulation
    *   The root actor of the simulation
    * @param schedulee
    *   scheduler or other actor whose time advancement is controlled
    * @param endTick
    *   the last tick of the simulation
    */
  private final case class TimeAdvancerData(
      simulation: akka.actor.ActorRef,
      schedulee: ActorRef[SchedulerMessage],
      endTick: Long
  )
}
