/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor.typed.scaladsl.adapter.ClassicActorContextOps
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, Scheduler}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}

import java.util.UUID

/** Simple lock that can be placed on a [[Scheduler]], which means that the
  * scheduler cannot complete the the tick that the lock was scheduled for,
  * until the lock has been dissolved. For example, when scheduling activations
  * of some actors, we need to prevent simulation time from advancing until
  * scheduling all of them has finished.
  */
object ScheduleLock {
  trait LockMsg

  private final case class Init(adapter: ActorRef[Activation]) extends LockMsg

  /** @param key
    *   the key that unlocks (part of) the lock
    */
  final case class Unlock(key: UUID) extends LockMsg

  private final case class WrappedActivation(tick: Long) extends LockMsg

  private def lockAdapter(lock: ActorRef[LockMsg]): Behavior[Activation] =
    Behaviors.receiveMessage { case Activation(tick) =>
      lock ! WrappedActivation(tick)
      // We can stop after forwarding the activation (there will be only one)
      Behaviors.stopped
    }

  final case class ScheduleKey(lock: ActorRef[LockMsg], key: UUID) {
    def unlock(): Unit =
      lock ! Unlock(key)
  }

  trait Spawner {
    def spawn[T](behavior: Behavior[T]): ActorRef[T]
  }

  private final case class TypedSpawner(ctx: ActorContext[_]) extends Spawner {
    override def spawn[T](behavior: Behavior[T]): ActorRef[T] =
      ctx.spawnAnonymous(behavior)
  }

  private final case class ClassicSpawner(ctx: akka.actor.ActorContext)
      extends Spawner {
    override def spawn[T](behavior: Behavior[T]): ActorRef[T] =
      ctx.spawnAnonymous(behavior)
  }

  def singleKey(
      ctx: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      tick: Long
  ): ScheduleKey =
    singleKey(TypedSpawner(ctx), scheduler, tick)

  def singleKey(
      ctx: akka.actor.ActorContext,
      scheduler: ActorRef[SchedulerMessage],
      tick: Long
  ): ScheduleKey =
    singleKey(ClassicSpawner(ctx), scheduler, tick)

  def singleKey(
      spawner: Spawner,
      scheduler: ActorRef[SchedulerMessage],
      tick: Long
  ): ScheduleKey =
    multiKey(spawner, scheduler, tick, 1).headOption.getOrElse(
      throw new RuntimeException("Should not happen")
    )

  def multiKey(
      ctx: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      tick: Long,
      count: Int
  ): Iterable[ScheduleKey] =
    multiKey(TypedSpawner(ctx), scheduler, tick, count)

  def multiKey(
      ctx: akka.actor.ActorContext,
      scheduler: ActorRef[SchedulerMessage],
      tick: Long,
      count: Int
  ): Iterable[ScheduleKey] =
    multiKey(ClassicSpawner(ctx), scheduler, tick, count)

  def multiKey(
      spawner: Spawner,
      scheduler: ActorRef[SchedulerMessage],
      tick: Long,
      count: Int
  ): Iterable[ScheduleKey] = {
    val keys = (1 to count).map(_ => UUID.randomUUID())

    val lock = spawner.spawn(ScheduleLock(scheduler, keys.toSet, tick))

    val adapter = spawner.spawn(lockAdapter(lock))
    lock ! Init(adapter)

    // We have to schedule the activation right away. If there is any
    // possibility for delay via a third actor, the lock could be
    // placed too late.
    scheduler ! ScheduleActivation(adapter, tick)

    keys.map(ScheduleKey(lock, _))
  }

  /** @param scheduler
    *   The scheduler to lock
    * @param awaitedKeys
    *   The keys that have to be supplied in order to unlock this lock
    * @param tick
    *   The tick to lock
    */
  private def apply(
      scheduler: ActorRef[SchedulerMessage],
      awaitedKeys: Set[UUID],
      tick: Long
  ): Behavior[LockMsg] =
    Behaviors.withStash(100) { buffer =>
      Behaviors.receiveMessage {
        case Init(adapter) =>
          buffer.unstashAll(uninitialized(scheduler, awaitedKeys, adapter))

        case msg =>
          // stash all messages until we are initialized
          buffer.stash(msg)
          Behaviors.same
      }
    }

  /** @param scheduler
    *   The scheduler to lock
    * @param awaitedKeys
    *   The keys that have to be supplied in order to unlock this lock
    * @return
    */
  def uninitialized(
      scheduler: ActorRef[SchedulerMessage],
      awaitedKeys: Set[UUID],
      adapter: ActorRef[Activation]
  ): Behavior[LockMsg] =
    Behaviors.withStash(100) { buffer =>
      Behaviors.receiveMessage {
        case WrappedActivation(_) =>
          buffer.unstashAll(active(scheduler, awaitedKeys, adapter))

        case unlock: Unlock =>
          // stash unlock messages until we are initialized
          buffer.stash(unlock)
          Behaviors.same
      }
    }

  private def active(
      scheduler: ActorRef[SchedulerMessage],
      awaitedKeys: Set[UUID],
      adapter: ActorRef[Activation]
  ): Behavior[LockMsg] = Behaviors.receiveMessage { case Unlock(key) =>
    val updatedKeys = awaitedKeys - key

    if (updatedKeys.nonEmpty)
      active(scheduler, updatedKeys, adapter)
    else {
      scheduler ! Completion(adapter)
      Behaviors.stopped
    }
  }
}
