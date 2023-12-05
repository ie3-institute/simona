/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import edu.ie3.simona.actor.ActorUtil.stopOnError
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.core.Core.{
  ActiveCore,
  CoreFactory,
  InactiveCore
}
import edu.ie3.simona.scheduler.core.RegularSchedulerCore

/** Scheduler that activates actors at specific ticks and keeps them
  * synchronized by waiting for the completions of all activations. Can be
  * stacked into scheduler hierarchies.
  */
object Scheduler {

  trait Incoming

  private final case class WrappedActivation(activation: Activation)
      extends Incoming

  /** Creates a new scheduler with given parent and core. The scheduler starts
    * in the inactive state.
    * @param parent
    *   The parent of this scheduler, which activates this scheduler and waits
    *   for its completion
    * @param coreFactory
    *   The factory that delivers the core to be used within this scheduler
    */
  def apply(
      parent: ActorRef[SchedulerMessage],
      coreFactory: CoreFactory = RegularSchedulerCore
  ): Behavior[Incoming] = Behaviors.setup { ctx =>
    val adapter =
      ctx.messageAdapter[Activation](msg => WrappedActivation(msg))

    inactive(
      SchedulerData(parent, adapter),
      coreFactory.create()
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
      if (core.checkSchedule(actor, newTick)) {
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
        .flatMap { newCore =>
          // if successful
          maybeNewTick
            .map { newTick =>
              Either
                .cond(
                  newCore.checkSchedule(actor, newTick),
                  newCore.handleSchedule(actor, newTick),
                  s"Cannot schedule an event at tick $newTick for completing actor $actor"
                )
            }
            .getOrElse(Right(newCore))
        }
        .map { newCore =>
          val (toActivate, updatedCore) = newCore.takeNewActivations()
          toActivate.foreach {
            _ ! Activation(updatedCore.activeTick)
          }

          updatedCore
        }
        .map { newCore =>
          newCore
            .maybeComplete()
            .map { case (maybeScheduleTick, inactiveCore) =>
              data.parent ! Completion(
                data.activationAdapter,
                maybeScheduleTick
              )
              inactive(data, inactiveCore)
            }
            .getOrElse {
              active(data, newCore)
            }
        }
        .fold(
          stopOnError(ctx, _),
          identity
        )

    case (ctx, unexpected) =>
      stopOnError(ctx, s"Received unexpected message $unexpected when active")
  }

  /** Data that is constant over the life time of a scheduler.
    * @param parent
    *   The parent of the scheduler
    * @param activationAdapter
    *   The activation adapter that is used to activate the scheduler
    */
  private final case class SchedulerData(
      parent: ActorRef[
        SchedulerMessage
      ],
      activationAdapter: ActorRef[Activation]
  )
}
