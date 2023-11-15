/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import edu.ie3.simona.ontology.messages.SchedulerMessageTyped.{
  Completion,
  ScheduleActivation
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessageTyped}
import edu.ie3.simona.scheduler.ScheduleLock.LockMsg

import java.util.UUID

/** Simple lock that can be placed on a [[Scheduler]], which means that the
  * scheduler cannot complete the the tick that the lock was scheduled for,
  * until the lock has been dissolved. For example, when scheduling activations
  * of some actors, we need to prevent simulation time from advancing until
  * scheduling all of them has finished.
  */
object ScheduleLock {
  trait LockMsg

  /** @param key
    *   the key that unlocks (part of) the lock
    */
  final case class Unlock(key: UUID) extends LockMsg

  private final case class WrappedActivation(tick: Long) extends LockMsg

  final case class ScheduleKey(lock: ActorRef[LockMsg], key: UUID) {
    def unlock(): Unit =
      lock ! Unlock(key)
  }

  /** @param scheduler
    *   The scheduler to lock
    * @param awaitedKeys
    *   The keys that have to be supplied in order to unlock this lock
    * @param tick
    *   The tick to lock
    */
  def apply(
      scheduler: ActorRef[SchedulerMessageTyped],
      awaitedKeys: Set[UUID],
      tick: Long
  ): Behavior[LockMsg] =
    Behaviors.setup { ctx =>
      val adapter =
        ctx.messageAdapter[Activation](msg => WrappedActivation(msg.tick))
      scheduler ! ScheduleActivation(adapter, tick)

      uninitialized(scheduler, awaitedKeys, adapter)
    }

  /** @param scheduler
    *   The scheduler to lock
    * @param awaitedKeys
    *   The keys that have to be supplied in order to unlock this lock
    * @return
    */
  def uninitialized(
      scheduler: ActorRef[SchedulerMessageTyped],
      awaitedKeys: Set[UUID],
      adapter: ActorRef[Activation]
  ): Behavior[LockMsg] =
    Behaviors.withStash(100) { buffer =>
      Behaviors.receiveMessage {
        case WrappedActivation(_) =>
          buffer.unstashAll(active(awaitedKeys, scheduler, adapter))

        case unlock: Unlock =>
          // stash unlock messages until we are initialized
          buffer.stash(unlock)
          Behaviors.same
      }
    }

  private def active(
      awaitedKeys: Set[UUID],
      parent: ActorRef[SchedulerMessageTyped],
      adapter: ActorRef[Activation]
  ): Behavior[LockMsg] = Behaviors.receiveMessage { case Unlock(key) =>
    val updatedKeys = awaitedKeys - key

    if (updatedKeys.nonEmpty)
      active(updatedKeys, parent, adapter)
    else {
      parent ! Completion(adapter)
      Behaviors.stopped
    }
  }
}
