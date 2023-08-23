/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor
import akka.actor.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.adapter.TypedActorRefOps
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import edu.ie3.simona.actor.ActorUtil.stopOnError
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.trigger.Trigger
import edu.ie3.simona.ontology.trigger.Trigger.ActivityStartTrigger
import edu.ie3.simona.scheduler.SchedulerData.ActivationData

object Scheduler {

  def apply(
      parent: actor.typed.ActorRef[SchedulerMessage]
  ): Behavior[SchedulerMessage] = inactive(
    SchedulerData(parent)
  )

  private def inactive(
      data: SchedulerData,
      lastActiveTick: Long = Long.MinValue
  ): Behavior[SchedulerMessage] =
    Behaviors.receive {
      case (ctx, TriggerWithIdMessage(ActivityStartTrigger(tick), triggerId)) =>
        checkActivation(data, tick).map(stopOnError(ctx, _)).getOrElse {
          sendCurrentTriggers(data, ActivationData(tick, triggerId)) match {
            case (newSchedulerData, newActivationData) =>
              active(newSchedulerData, newActivationData)
          }
        }

      case (ctx, ScheduleTriggerMessage(trigger, actorToBeScheduled)) =>
        checkTriggerSchedule(lastActiveTick + 1L, trigger)
          .map(stopOnError(ctx, _))
          .getOrElse {
            inactive(scheduleTrigger(data, trigger, actorToBeScheduled))
          }

      case (ctx, unexpected: SchedulerMessage) =>
        stopOnError(
          ctx,
          s"Received unexpected message $unexpected when inactive"
        )

    }

  private def active(
      data: SchedulerData,
      activationData: ActivationData
  ): Behavior[SchedulerMessage] = Behaviors.receive {

    case (ctx, ScheduleTriggerMessage(trigger, actorToBeScheduled)) =>
      checkTriggerSchedule(activationData.tick, trigger)
        .map(stopOnError(ctx, _))
        .getOrElse {
          sendCurrentTriggers(
            scheduleTrigger(data, trigger, actorToBeScheduled),
            activationData
          ) match {
            case (newSchedulerData, newActivationData) =>
              active(newSchedulerData, newActivationData)
          }
        }

    case (ctx, CompletionMessage(triggerId, newTrigger)) =>
      val tick = activationData.tick

      checkCompletion(activationData, triggerId)
        .toLeft(handleCompletion(activationData, triggerId))
        .flatMap { updatedActivationData =>
          val eitherData = Right(data).withLeft[String]

          // schedule new triggers, if present
          newTrigger
            .map { newTrigger =>
              eitherData.flatMap { currentData =>
                checkTriggerSchedule(tick, newTrigger.trigger)
                  .toLeft(
                    scheduleTrigger(
                      currentData,
                      newTrigger.trigger,
                      newTrigger.actorToBeScheduled
                    )
                  )
              }
            }
            .getOrElse(eitherData)
            .map((_, updatedActivationData))
        }
        .map { case (updatedData, updatedActivationData) =>
          if (isTickCompleted(updatedData, updatedActivationData)) {
            // send completion to parent, if all completed
            completeWithParent(updatedData, updatedActivationData, ctx)
            inactive(updatedData)
          } else {
            // there might be new triggers for current tick, send them out
            sendCurrentTriggers(updatedData, updatedActivationData) match {
              case (newSchedulerData, newActivationData) =>
                active(newSchedulerData, newActivationData)
            }
          }
        }
        .fold(stopOnError(ctx, _), identity)

    case (ctx, unexpected: SchedulerMessage) =>
      stopOnError(ctx, s"Received unexpected message $unexpected when active")
  }

  private def checkActivation(
      data: SchedulerData,
      newTick: Long
  ): Option[String] =
    data.triggerQueue.headKeyOption.flatMap { minScheduledKey =>
      Option.when(newTick != minScheduledKey) {
        s"The next tick to activate is $minScheduledKey, not $newTick"
      }
    }

  private def checkTriggerSchedule(
      minTick: Long,
      trigger: Trigger
  ): Option[String] = {
    Option.when(trigger.tick < minTick) {
      s"Cannot schedule an event $trigger at tick ${trigger.tick} when the last or currently activated tick is $minTick"
    }
  }

  private def checkCompletion(
      activationData: ActivationData,
      triggerId: Long
  ): Option[String] =
    Option
      .when(activationData.awaitingCompletions <= 0) {
        s"No completions expected, received completion for trigger id $triggerId"
      }
      .orElse {
        Option.unless(
          activationData.triggerIdToActiveTrigger.contains(triggerId)
        ) {
          s"Trigger id $triggerId is not part of expected trigger ids ${activationData.triggerIdToActiveTrigger.keys}"
        }
      }

  private def sendCurrentTriggers(
      data: SchedulerData,
      activationData: ActivationData
  ): (SchedulerData, ActivationData) = {
    val newActivationData =
      data.triggerQueue
        .getAndRemoveSet(activationData.tick)
        .foldLeft(activationData) {
          case (
                updatedActivationData,
                actor
              ) =>
            data.actorToTrigger
              .remove(actor)
              .map { triggerWithId =>
                // track the trigger id with the scheduled trigger
                updatedActivationData.triggerIdToActiveTrigger +=
                  triggerWithId.triggerId -> actor

                actor ! triggerWithId

                // track that we wait for a response for this tick
                updatedActivationData
                  .copy(awaitingCompletions =
                    updatedActivationData.awaitingCompletions + 1
                  )
              }
              .getOrElse(updatedActivationData)

        }

    (data, newActivationData)
  }

  private def scheduleTrigger(
      data: SchedulerData,
      trigger: Trigger,
      actorToBeScheduled: ActorRef
  ): SchedulerData = {
    val newTriggerId = data.lastTriggerId + 1L
    val triggerWithIdMessage =
      TriggerWithIdMessage(
        trigger,
        newTriggerId
      )

    /* update trigger queue */
    data.triggerQueue.set(
      trigger.tick,
      actorToBeScheduled
    )

    data.actorToTrigger += (
      actorToBeScheduled -> triggerWithIdMessage
    )

    data.copy(
      lastTriggerId = newTriggerId
    )
  }

  private def handleCompletion(
      data: ActivationData,
      triggerId: Long
  ): ActivationData = {
    data.triggerIdToActiveTrigger.remove(triggerId)
    data.copy(awaitingCompletions = data.awaitingCompletions - 1)
  }

  /** Returns true if current tick can be completed with parent.
    */
  private def isTickCompleted(
      data: SchedulerData,
      activationData: ActivationData
  ): Boolean =
    activationData.awaitingCompletions == 0 &&
      !data.triggerQueue.headKeyOption.contains(activationData.tick)

  private def completeWithParent(
      data: SchedulerData,
      activationData: ActivationData,
      ctx: ActorContext[SchedulerMessage]
  ): Unit = {
    val newTriggers = data.triggerQueue.headKeyOption.map(tick =>
      ScheduleTriggerMessage(ActivityStartTrigger(tick), ctx.self.toClassic)
    )
    data.parent ! CompletionMessage(
      activationData.activationTriggerId,
      newTriggers
    )
  }
}
