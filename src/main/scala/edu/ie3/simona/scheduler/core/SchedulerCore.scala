/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler.core

import akka.actor.typed.ActorRef
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.scheduler.core.Core.{ActiveCore, InactiveCore}
import edu.ie3.util.scala.collection.mutable.PriorityMultiBiSet
object SchedulerCore {
  case class SchedulerInactive(
      triggerQueue: PriorityMultiBiSet[Long, ActorRef[Activation]],
      lastActiveTick: Option[Long]
  ) extends InactiveCore {
    override def checkActivation(newTick: Long): Boolean =
      triggerQueue.headKeyOption.contains(newTick)

    override def activate(): ActiveCore = {
      val newActiveTick = triggerQueue.headKeyOption.getOrElse(
        throw new RuntimeException("Nothing scheduled, cannot activate.")
      )
      SchedulerActive(triggerQueue, activeTick = newActiveTick)
    }

    override def checkSchedule(newTick: Long): Boolean =
      lastActiveTick.forall(newTick >= _ + 1)

    override def handleSchedule(
        actor: ActorRef[Activation],
        newTick: Long
    ): (Option[Long], InactiveCore) = {
      val oldEarliestTick = triggerQueue.headKeyOption

      triggerQueue.set(newTick, actor)
      val newEarliestTick = triggerQueue.headKeyOption

      val maybeScheduleTick =
        Option.when(newEarliestTick != oldEarliestTick)(newEarliestTick).flatten

      (maybeScheduleTick, this)
    }
  }

  object SchedulerInactive {
    def create(): SchedulerInactive =
      SchedulerInactive(PriorityMultiBiSet.empty, None)
  }

  case class SchedulerActive(
      triggerQueue: PriorityMultiBiSet[Long, ActorRef[Activation]],
      activeActors: Set[ActorRef[Activation]] = Set.empty,
      activeTick: Long
  ) extends ActiveCore {
    override def checkCompletion(actor: ActorRef[Activation]): Boolean =
      activeActors.contains(actor)

    override def handleCompletion(actor: ActorRef[Activation]): ActiveCore =
      copy(activeActors = activeActors.excl(actor))

    override def maybeComplete(): Option[(Option[Long], InactiveCore)] =
      Option.when(
        activeActors.isEmpty && !triggerQueue.headKeyOption.contains(activeTick)
      ) {
        (
          triggerQueue.headKeyOption,
          SchedulerInactive(triggerQueue, Some(activeTick))
        )
      }

    override def checkSchedule(newTick: Long): Boolean =
      newTick >= activeTick

    override def handleSchedule(
        actor: ActorRef[Activation],
        newTick: Long
    ): ActiveCore = {
      triggerQueue.set(newTick, actor)
      this
    }

    override def takeNewActivations()
        : (Iterable[ActorRef[Activation]], ActiveCore) = {
      val toActivate = triggerQueue.getAndRemoveSet(activeTick)
      val newActiveCore = copy(activeActors = activeActors.concat(toActivate))
      (toActivate, newActiveCore)
    }
  }
}
