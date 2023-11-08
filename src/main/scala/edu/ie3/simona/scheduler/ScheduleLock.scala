/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.trigger.Trigger

import java.util.UUID

/** Simple lock that can be placed on a [[Scheduler]], which means that the
  * scheduler cannot complete the the tick that the lock was scheduled for,
  * until the lock has been dissolved. For example, when scheduling activations
  * of some actors, we need to prevent simulation time from advancing until
  * scheduling all of them has finished.
  */
object ScheduleLock {
  trait LockMsg

  /** Trigger that should be scheduled for a tick that we want to prevent the
    * scheduler from completing
    * @param parent
    *   the scheduler to lock
    * @param tick
    *   the tick to lock. Is only needed for the [[Trigger]] superclass
    */
  final case class InitLock(
      parent: ActorRef[SchedulerMessage],
      tick: Long
  ) extends Trigger

  /** @param key
    *   the key that unlocks (part of) the lock
    */
  final case class Unlock(key: UUID) extends LockMsg

  /** @param awaitedKeys
    *   the keys that have to be supplied in order to unlock this lock
    */
  def apply(awaitedKeys: Set[UUID]): Behavior[LockMsg] =
    Behaviors.withStash(100) { buffer =>
      Behaviors.receiveMessage {
        case TriggerWithIdMessage(InitLock(parent, _), triggerId) =>
          buffer.unstashAll(active(awaitedKeys, parent, triggerId))

        case unlock: Unlock =>
          // stash unlock messages until we are initialized
          buffer.stash(unlock)
          Behaviors.same
      }
    }

  private def active(
      awaitedKeys: Set[UUID],
      parent: ActorRef[SchedulerMessage],
      triggerId: Long
  ): Behavior[LockMsg] = Behaviors.receiveMessage { case Unlock(key) =>
    val updatedKeys = awaitedKeys - key

    if (updatedKeys.nonEmpty)
      active(updatedKeys, parent, triggerId)
    else {
      parent ! CompletionMessage(triggerId)
      Behaviors.stopped
    }
  }
}
