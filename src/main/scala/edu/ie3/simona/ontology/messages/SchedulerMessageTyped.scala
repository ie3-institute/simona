/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages

import akka.actor.typed.ActorRef
import edu.ie3.simona.scheduler.ScheduleLock.LockMsg
import edu.ie3.simona.scheduler.{Scheduler, TimeAdvancer}

import java.util.UUID

// FIXME SchedulerMessage
trait SchedulerMessageTyped
    extends Scheduler.Incoming
    with TimeAdvancer.Incoming

object SchedulerMessageTyped {
  final case class Completion(
      actor: ActorRef[Activation],
      newTick: Option[Long] = None
  ) extends SchedulerMessageTyped

  final case class ScheduleActivation(
      actor: ActorRef[Activation],
      tick: Long,
      unlockKey: Option[(ActorRef[LockMsg], UUID)] = None
  ) extends SchedulerMessageTyped

}
