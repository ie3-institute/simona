/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import edu.ie3.simona.actor.ActorUtil.stopOnError
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.core.Core.{ActiveCore, InactiveCore}
import edu.ie3.simona.scheduler.core.SchedulerCore.SchedulerInactive

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
      SchedulerData(parent, adapter),
      SchedulerInactive.create()
    )
  }

  private def inactive(
      data: SchedulerData,
      core: InactiveCore
  ): Behavior[Incoming] =
    Behaviors.receive {
      case (ctx, WrappedActivation(Activation(tick))) =>
        if (core.checkActivation(tick)) {
          val (toActivate, activeCore) = core.activate().takeNewActivations()

          toActivate.foreach { _ ! Activation(tick) }

          active(data, activeCore)
        } else {
          stopOnError(ctx, s"Cannot activate with new tick $tick")
        }

      case (
            ctx,
            ScheduleActivation(actor, newTick, unlockKey)
          ) =>
        if (core.checkSchedule(newTick)) {
          val (maybeSchedule, newCore) = core.handleSchedule(actor, newTick)

          maybeSchedule match {
            case Some(scheduleTick) =>
              // also potentially schedule with parent if the new earliest tick is
              // different from the old earliest tick (including if nothing had
              // been scheduled before)
              data.parent ! ScheduleActivation(
                data.activationAdapter,
                scheduleTick,
                unlockKey
              )
            case None =>
              // we don't need to escalate to the parent, this means that we can release the lock (if applicable)
              unlockKey.foreach {
                _.unlock()
              }
          }

          inactive(data, newCore)
        } else {
          stopOnError(ctx, s"Cannot schedule an event at tick $newTick")
        }
      case (ctx, unexpected) =>
        stopOnError(
          ctx,
          s"Received unexpected message $unexpected when inactive"
        )
    }

  private def active(
      data: SchedulerData,
      core: ActiveCore
  ): Behavior[Incoming] = Behaviors.receive {

    case (
          ctx,
          ScheduleActivation(actor, newTick, unlockKey)
        ) =>
      if (core.checkSchedule(newTick)) {
        val (toActivate, newCore) =
          core.handleSchedule(actor, newTick).takeNewActivations()

        // if there's a lock:
        // since we're active and any scheduled activation can still influence our next activation,
        // we can directly unlock the lock with the key
        unlockKey.foreach {
          _.unlock()
        }

        toActivate.foreach {
          _ ! Activation(newCore.activeTick)
        }

        active(data, newCore)
      } else {
        stopOnError(ctx, s"Cannot schedule an event at tick $newTick")
      }

    case (ctx, Completion(actor, maybeNewTick)) =>
      Either
        .cond(
          core.checkCompletion(actor),
          core.handleCompletion(actor),
          s"Actor $actor is not part of the expected completing actors"
        )
        .flatMap { core =>
          // if successful
          maybeNewTick
            .map { newTick =>
              Either
                .cond(
                  core.checkSchedule(newTick),
                  core.handleSchedule(actor, newTick),
                  s"Cannot schedule an event at tick $newTick for completing actor $actor"
                )
                .map { newCore =>
                  val (toActivate, updatedCore) = newCore.takeNewActivations()
                  toActivate.foreach {
                    _ ! Activation(newCore.activeTick)
                  }

                  updatedCore
                }
            }
            .getOrElse(Right(core))
        }
        .map { core =>
          core
            .maybeComplete()
            .map { case (maybeScheduleTick, inactiveCore) =>
              data.parent ! Completion(
                data.activationAdapter,
                maybeScheduleTick
              )
              inactive(data, inactiveCore)
            }
            .getOrElse {
              active(data, core)
            }
        }
        .fold(
          stopOnError(ctx, _),
          identity
        )

    case (ctx, unexpected) =>
      stopOnError(ctx, s"Received unexpected message $unexpected when active")
  }

  private final case class SchedulerData(
      parent: ActorRef[
        SchedulerMessage
      ],
      activationAdapter: ActorRef[Activation]
  )
}
