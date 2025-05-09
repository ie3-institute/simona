/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import edu.ie3.simona.actor.ActorUtil.stopOnError
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.core.Core.{
  ActiveCore,
  CoreFactory,
  InactiveCore,
}
import edu.ie3.simona.scheduler.core.RegularSchedulerCore
import edu.ie3.util.scala.Scope
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

/** Scheduler that activates actors at specific ticks and keeps them
  * synchronized by waiting for the completions of all activations. Can be
  * stacked into scheduler hierarchies.
  */
object Scheduler {

  trait Request

  private final case class WrappedActivation(activation: Activation)
      extends Request

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
      coreFactory: CoreFactory = RegularSchedulerCore,
  ): Behavior[Request] = Behaviors.setup { ctx =>
    val adapter =
      ctx.messageAdapter[Activation](WrappedActivation)

    inactive(
      SchedulerData(parent, adapter),
      coreFactory.create(),
    )
  }

  private def inactive(
      data: SchedulerData,
      core: InactiveCore,
  ): Behavior[Request] =
    Behaviors.receive {
      case (_, WrappedActivation(Activation(tick))) =>
        val (toActivate, activeCore) = core
          .activate(tick)
          .takeNewActivations()

        toActivate.foreach { _ ! Activation(tick) }

        active(data, activeCore)

      case (
            _,
            ScheduleActivation(actor, newTick, unlockKey),
          ) =>
        val (maybeSchedule, newCore) = core.handleSchedule(actor, newTick)
        maybeSchedule match {
          case Some(scheduleTick) =>
            // also potentially schedule with parent if the new earliest tick is
            // different from the old earliest tick (including if nothing had
            // been scheduled before)
            data.parent ! ScheduleActivation(
              data.activationAdapter,
              scheduleTick,
              unlockKey,
            )
          case None =>
            // we don't need to escalate to the parent, this means that we can release the lock (if applicable)
            unlockKey.foreach {
              _.unlock()
            }
        }

        inactive(data, newCore)

      case (ctx, unexpected) =>
        stopOnError(
          ctx,
          s"Received unexpected message $unexpected when inactive",
        )
    }

  private def active(
      data: SchedulerData,
      core: ActiveCore,
  ): Behavior[Request] = Behaviors.receive {

    case (
          _,
          ScheduleActivation(actor, newTick, unlockKey),
        ) =>
      val (toActivate, newCore) = core
        .handleSchedule(actor, newTick)
        .takeNewActivations()

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

    case (_, Completion(actor, maybeNewTick)) =>
      Scope(core.handleCompletion(actor))
        .map { newCore =>
          maybeNewTick
            .map(newCore.handleSchedule(actor, _))
            .getOrElse(newCore)
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
                maybeScheduleTick,
              )
              inactive(data, inactiveCore)
            }
            .getOrElse {
              active(data, newCore)
            }
        }
        .get

    case (ctx, unexpected) =>
      stopOnError(ctx, s"Received unexpected message $unexpected when active")
  }

  /** Data that is constant over the lifetime of a scheduler.
    * @param parent
    *   The parent of the scheduler
    * @param activationAdapter
    *   The activation adapter that is used to activate the scheduler
    */
  private final case class SchedulerData(
      parent: ActorRef[
        SchedulerMessage
      ],
      activationAdapter: ActorRef[Activation],
  )

}
