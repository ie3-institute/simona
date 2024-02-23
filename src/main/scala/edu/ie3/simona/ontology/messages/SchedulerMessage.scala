/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages

import org.apache.pekko.actor.typed.ActorRef
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.scheduler.{Scheduler, TimeAdvancer}

trait SchedulerMessage extends Scheduler.Request with TimeAdvancer.Request

object SchedulerMessage {
  final case class Completion(
      actor: ActorRef[Activation],
      newTick: Option[Long] = None,
  ) extends SchedulerMessage

  final case class ScheduleActivation(
      actor: ActorRef[Activation],
      tick: Long,
      unlockKey: Option[ScheduleKey] = None,
  ) extends SchedulerMessage

}
