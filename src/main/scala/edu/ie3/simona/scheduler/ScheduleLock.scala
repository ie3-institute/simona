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

/** TODO
  */
object ScheduleLock {
  trait LockMsg

  final case class InitLock(
      parent: ActorRef[SchedulerMessage],
      tick: Long
  ) extends Trigger

  final case class Unlock(key: UUID) extends LockMsg

  def apply(awaitedKeys: Set[UUID]): Behavior[LockMsg] =
    Behaviors.receiveMessage {
      case TriggerWithIdMessage(InitLock(parent, _), triggerId) =>
        active(awaitedKeys, parent, triggerId)
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
