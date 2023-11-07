/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor
import akka.actor.ActorRef
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.TriggerWithIdMessage
import edu.ie3.util.scala.collection.mutable.PriorityMultiBiSet

import scala.collection.mutable

final case class SchedulerData(
    parent: actor.typed.ActorRef[
      SchedulerMessage
    ], // FIXME typed: Scheduler or time advancer
    lastTriggerId: Long = 0L,
    triggerQueue: PriorityMultiBiSet[Long, ActorRef] = PriorityMultiBiSet.empty[
      Long,
      ActorRef
    ],
    actorToTrigger: mutable.Map[ActorRef, TriggerWithIdMessage] =
      mutable.Map.empty // TODO can be removed once this is handled by the receiving actor
)

object SchedulerData {

  /** Data that is only required when scheduler is active
    */
  final case class ActivationData(
      tick: Long,
      activationTriggerId: Long,
      triggerIdToActiveTrigger: mutable.Map[Long, ActorRef] =
        mutable.Map.empty[Long, ActorRef]
  )

}
