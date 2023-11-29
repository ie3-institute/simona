/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import edu.ie3.simona.actor.ActorUtil.stopOnError
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation
}
import edu.ie3.simona.scheduler.SchedulerData.ActivationData

/** Scheduler that activates actors at specific ticks and keeps them
  * synchronized by waiting for the completions of all activations. Can be
  * stacked into scheduler hierarchies.
  */
object Scheduler {

  trait Incoming

  private final case class WrappedActivation(activation: Activation)
      extends Incoming

  def apply(
      parent: ActorRef[SchedulerMessage]
  ): Behavior[Incoming] = Behaviors.setup { ctx =>
    val adapter =
      ctx.messageAdapter[Activation](msg => WrappedActivation(msg))

    inactive(
      SchedulerData(parent, adapter)
    )
  }

  private def inactive(
      data: SchedulerData,
      lastActiveTick: Long = Long.MinValue
  ): Behavior[Incoming] =
    Behaviors.receive {
      case (ctx, WrappedActivation(Activation(tick))) =>
        checkActivation(data, tick).map(stopOnError(ctx, _)).getOrElse {
          sendCurrentTriggers(data, ActivationData(tick)) match {
            case (newSchedulerData, newActivationData) =>
              active(newSchedulerData, newActivationData)
          }
        }

      case (
            ctx,
            ScheduleActivation(actor, newTick, unlockKey)
          ) =>
        checkTriggerSchedule(lastActiveTick + 1L, newTick)
          .map(stopOnError(ctx, _))
          .getOrElse {
            val oldEarliestTick = data.triggerQueue.headKeyOption

            val updatedData = scheduleTrigger(data, actor, newTick)
            val newEarliestTick = updatedData.triggerQueue.headKeyOption

            // also potentially schedule with parent if the new earliest tick is
            // different from the old earliest tick (including if nothing had
            // been scheduled before)
            if (newEarliestTick != oldEarliestTick)
              data.parent ! ScheduleActivation(
                data.activationAdapter,
                newTick,
                unlockKey
              )
            else {
              // we don't need to escalate to the parent, this means that we can release the lock (if applicable)
              unlockKey.foreach { _.unlock() }
            }
            inactive(updatedData, lastActiveTick)
          }

      case (ctx, unexpected) =>
        stopOnError(
          ctx,
          s"Received unexpected message $unexpected when inactive"
        )
    }

  private def active(
      data: SchedulerData,
      activationData: ActivationData
  ): Behavior[Incoming] = Behaviors.receive {

    case (
          ctx,
          ScheduleActivation(actor, newTick, unlockKey)
        ) =>
      checkTriggerSchedule(activationData.tick, newTick)
        .map(stopOnError(ctx, _))
        .getOrElse {
          // if there's a lock:
          // since we're active and any scheduled activation can still influence our next activation,
          // we can directly unlock the lock with the key
          unlockKey.foreach { _.unlock() }

          sendCurrentTriggers(
            scheduleTrigger(data, actor, newTick),
            activationData
          ) match {
            case (newSchedulerData, newActivationData) =>
              active(newSchedulerData, newActivationData)
          }
        }

    case (ctx, Completion(actor, maybeNewTick)) =>
      val currentTick = activationData.tick

      checkCompletion(activationData, actor)
        .toLeft(handleCompletion(activationData, actor))
        .flatMap { updatedActivationData =>
          // schedule new triggers, if present
          maybeNewTick
            .map { newTick =>
              checkTriggerSchedule(currentTick, newTick)
                .toLeft(
                  scheduleTrigger(data, actor, newTick)
                )
            }
            .getOrElse(Right(data))
            .map((_, updatedActivationData))
        }
        .map { case (updatedData, updatedActivationData) =>
          if (isTickCompleted(updatedData, updatedActivationData)) {
            // send completion to parent, if all completed
            completeWithParent(updatedData)
            inactive(updatedData, currentTick)
          } else {
            // there might be new triggers for current currentTick, send them out
            sendCurrentTriggers(updatedData, updatedActivationData) match {
              case (newSchedulerData, newActivationData) =>
                active(newSchedulerData, newActivationData)
            }
          }
        }
        .fold(stopOnError(ctx, _), identity)

    case (ctx, unexpected) =>
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
      newTick: Long
  ): Option[String] = {
    Option.when(newTick < minTick) {
      s"Cannot schedule an event at tick $newTick when the last or currently activated tick is $minTick"
    }
  }

  private def checkCompletion(
      activationData: ActivationData,
      actor: ActorRef[Activation]
  ): Option[String] =
    Option.unless(
      activationData.activeActors.contains(actor)
    ) {
      s"Actor $actor is not part of expected completing actors ${activationData.activeActors}"
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
            // track the trigger id with the scheduled trigger
            updatedActivationData.activeActors += actor

            actor ! Activation(activationData.tick)

            updatedActivationData

        }

    (data, newActivationData)
  }

  private def scheduleTrigger(
      data: SchedulerData,
      actorToBeScheduled: ActorRef[Activation],
      tick: Long
  ): SchedulerData = {
    /* update trigger queue */
    data.triggerQueue.set(
      tick,
      actorToBeScheduled
    )

    data
  }

  private def handleCompletion(
      data: ActivationData,
      actor: ActorRef[Activation]
  ): ActivationData = {
    data.activeActors.remove(actor)
    data
  }

  /** Returns true if current tick can be completed with parent.
    */
  private def isTickCompleted(
      data: SchedulerData,
      activationData: ActivationData
  ): Boolean =
    activationData.activeActors.isEmpty &&
      !data.triggerQueue.headKeyOption.contains(activationData.tick)

  private def completeWithParent(data: SchedulerData): Unit = {
    val newTick = data.triggerQueue.headKeyOption
    data.parent ! Completion(data.activationAdapter, newTick)
  }
}
