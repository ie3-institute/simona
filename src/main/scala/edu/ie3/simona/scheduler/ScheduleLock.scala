/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorContextOps
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior, Scheduler}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}

import java.util.UUID

/** Simple lock that can be placed on a [[Scheduler]], which means that the
  * scheduler cannot complete the tick that the lock was scheduled for, until
  * the lock has been dissolved. For example, when scheduling activations of
  * some actors, we need to prevent simulation time from advancing until
  * scheduling all of them has finished. Locks with one or multiple keys can be
  * created, for which all created keys are required for unlocking the lock.
  */
object ScheduleLock {
  sealed trait LockMsg

  private final case class Init(adapter: ActorRef[Activation]) extends LockMsg

  private final case object LockActivation extends LockMsg

  /** @param key
    *   the key that unlocks (part of) the lock
    */
  final case class Unlock(key: UUID) extends LockMsg

  private def lockAdapter(
      lock: ActorRef[LockMsg],
      expectedTick: Long,
  ): Behavior[Activation] =
    Behaviors.receive { case (ctx, Activation(tick)) =>
      if (tick == expectedTick)
        lock ! LockActivation
      else
        ctx.log.error(
          s"Received lock activation for tick $tick, but expected $expectedTick"
        )
      // We can stop after forwarding the activation (there will be only one)
      Behaviors.stopped
    }

  /** Key that can unlock (a part of) a [[ScheduleLock]].
    * @param lock
    *   The corresponding lock
    * @param key
    *   A key (can be one of multiple) that unlocks (part of) the lock
    */
  final case class ScheduleKey(lock: ActorRef[LockMsg], key: UUID) {
    def unlock(): Unit =
      lock ! Unlock(key)
  }

  /** Defines a method of spawning actors from behaviors
    */
  trait Spawner {
    def spawn[T](behavior: Behavior[T]): ActorRef[T]
  }

  private final case class TypedSpawner(ctx: ActorContext[_]) extends Spawner {
    override def spawn[T](behavior: Behavior[T]): ActorRef[T] =
      ctx.spawnAnonymous(behavior)
  }

  private final case class ClassicSpawner(
      ctx: org.apache.pekko.actor.ActorContext
  ) extends Spawner {
    override def spawn[T](behavior: Behavior[T]): ActorRef[T] =
      ctx.spawnAnonymous(behavior)
  }

  /** Creates a lock with a single key.
    *
    * @param ctx
    *   The typed ActorContext that is used to spawn actors
    * @param scheduler
    *   The scheduler to lock
    * @param tick
    *   The tick that the scheduler will be locked at (usually the current
    *   tick).
    * @return
    *   A single key that unlocks the lock
    */
  def singleKey(
      ctx: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      tick: Long,
  ): ScheduleKey =
    singleKey(TypedSpawner(ctx), scheduler, tick)

  /** Creates a lock with a single key.
    *
    * @param ctx
    *   The classic ActorContext that is used to spawn actors
    * @param scheduler
    *   The scheduler to lock
    * @param tick
    *   The tick that the scheduler will be locked at (usually the current
    *   tick).
    * @return
    *   A single key that unlocks the lock
    */
  def singleKey(
      ctx: org.apache.pekko.actor.ActorContext,
      scheduler: ActorRef[SchedulerMessage],
      tick: Long,
  ): ScheduleKey =
    singleKey(ClassicSpawner(ctx), scheduler, tick)

  /** Creates a lock with a single key.
    *
    * @param spawner
    *   Trait that defines a way to spawn actors
    * @param scheduler
    *   The scheduler to lock
    * @param tick
    *   The tick that the scheduler will be locked at (usually the current
    *   tick).
    * @return
    *   A single key that unlocks the lock
    */
  def singleKey(
      spawner: Spawner,
      scheduler: ActorRef[SchedulerMessage],
      tick: Long,
  ): ScheduleKey =
    multiKey(spawner, scheduler, tick, 1).headOption.getOrElse(
      throw new RuntimeException("Should not happen")
    )

  /** Creates a lock with a multiple keys.
    *
    * @param ctx
    *   The typed ActorContext that is used to spawn actors
    * @param scheduler
    *   The scheduler to lock
    * @param tick
    *   The tick that the scheduler will be locked at (usually the current
    *   tick).
    * @param count
    *   The number of keys needed to unlock the lock
    * @return
    *   A collection of keys that are needed to unlock the lock
    */
  def multiKey(
      ctx: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      tick: Long,
      count: Int,
  ): Iterable[ScheduleKey] =
    multiKey(TypedSpawner(ctx), scheduler, tick, count)

  /** Creates a lock with a multiple keys.
    *
    * @param ctx
    *   The classic ActorContext that is used to spawn actors
    * @param scheduler
    *   The scheduler to lock
    * @param tick
    *   The tick that the scheduler will be locked at (usually the current
    *   tick).
    * @param count
    *   The number of keys needed to unlock the lock
    * @return
    *   A collection of keys that are needed to unlock the lock
    */
  def multiKey(
      ctx: org.apache.pekko.actor.ActorContext,
      scheduler: ActorRef[SchedulerMessage],
      tick: Long,
      count: Int,
  ): Iterable[ScheduleKey] =
    multiKey(ClassicSpawner(ctx), scheduler, tick, count)

  /** Creates a lock with a multiple keys.
    *
    * @param spawner
    *   Trait that defines a way to spawn actors
    * @param scheduler
    *   The scheduler to lock
    * @param tick
    *   The tick that the scheduler will be locked at (usually the current
    *   tick).
    * @param count
    *   The number of keys needed to unlock the lock
    * @return
    *   A collection of keys that are needed to unlock the lock
    */
  def multiKey(
      spawner: Spawner,
      scheduler: ActorRef[SchedulerMessage],
      tick: Long,
      count: Int,
  ): Iterable[ScheduleKey] = {
    val keys = (1 to count).map(_ => UUID.randomUUID())

    val lock = spawner.spawn(ScheduleLock(scheduler, keys.toSet))

    val adapter = spawner.spawn(lockAdapter(lock, tick))
    lock ! Init(adapter)

    // We have to schedule the activation right away. If there is any
    // possibility for delay via a third actor, the lock could be
    // placed too late.
    scheduler ! ScheduleActivation(adapter, tick)

    keys.map(ScheduleKey(lock, _))
  }

  /** Default internal method to create a lock.
    *
    * @param scheduler
    *   The scheduler to lock
    * @param awaitedKeys
    *   The keys that have to be supplied in order to unlock this lock
    */
  private def apply(
      scheduler: ActorRef[SchedulerMessage],
      awaitedKeys: Set[UUID],
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

  private def uninitialized(
      scheduler: ActorRef[SchedulerMessage],
      awaitedKeys: Set[UUID],
      adapter: ActorRef[Activation],
  ): Behavior[LockMsg] =
    Behaviors.withStash(100) { buffer =>
      Behaviors.receiveMessage {
        case LockActivation =>
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
      adapter: ActorRef[Activation],
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
