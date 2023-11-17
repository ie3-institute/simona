/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor
import akka.actor.typed.ActorRef
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.util.scala.collection.mutable.PriorityMultiBiSet

import scala.collection.mutable

final case class SchedulerData(
    parent: actor.typed.ActorRef[
      SchedulerMessage
    ],
    activationAdapter: ActorRef[Activation],
    triggerQueue: PriorityMultiBiSet[Long, ActorRef[Activation]] =
      PriorityMultiBiSet.empty[
        Long,
        ActorRef[Activation]
      ]
)

object SchedulerData {

  /** Data that is only required when scheduler is active
    */
  final case class ActivationData(
      tick: Long,
      activeActors: mutable.Set[ActorRef[Activation]] =
        mutable.Set.empty[ActorRef[Activation]]
  )

}
