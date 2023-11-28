/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler.core

import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.scheduler.core.Core.{
  ActiveCore,
  CoreFactory,
  InactiveCore
}
import edu.ie3.util.scala.collection.mutable.PriorityMultiBiSet
import org.apache.pekko.actor.typed.ActorRef

/** TODO scaladoc
  */
object RegularSchedulerCore extends CoreFactory {

  override def create(): SchedulerInactive =
    SchedulerInactive(PriorityMultiBiSet.empty, None)

  case class SchedulerInactive private (
      private val activationQueue: PriorityMultiBiSet[Long, ActorRef[
        Activation
      ]],
      private val lastActiveTick: Option[Long]
  ) extends InactiveCore {
    override def checkActivation(newTick: Long): Boolean =
      activationQueue.headKeyOption.contains(newTick)

    override def activate(): ActiveCore = {
      val newActiveTick = activationQueue.headKeyOption.getOrElse(
        throw new RuntimeException("Nothing scheduled, cannot activate.")
      )
      SchedulerActive(activationQueue, activeTick = newActiveTick)
    }

    override def checkSchedule(newTick: Long): Boolean =
      lastActiveTick.forall(newTick >= _ + 1)

    override def handleSchedule(
        actor: ActorRef[Activation],
        newTick: Long
    ): (Option[Long], InactiveCore) = {
      val oldEarliestTick = activationQueue.headKeyOption

      activationQueue.set(newTick, actor)
      val newEarliestTick = activationQueue.headKeyOption

      val maybeScheduleTick =
        Option.when(newEarliestTick != oldEarliestTick)(newEarliestTick).flatten

      (maybeScheduleTick, this)
    }

  }

  private case class SchedulerActive(
      private val activationQueue: PriorityMultiBiSet[Long, ActorRef[
        Activation
      ]],
      private val activeActors: Set[ActorRef[Activation]] = Set.empty,
      activeTick: Long
  ) extends ActiveCore {
    override def checkCompletion(actor: ActorRef[Activation]): Boolean =
      activeActors.contains(actor)

    override def handleCompletion(actor: ActorRef[Activation]): ActiveCore =
      copy(activeActors = activeActors.excl(actor))

    override def maybeComplete(): Option[(Option[Long], InactiveCore)] =
      Option.when(
        activeActors.isEmpty &&
          !activationQueue.headKeyOption.contains(activeTick)
      ) {
        (
          activationQueue.headKeyOption,
          SchedulerInactive(activationQueue, Some(activeTick))
        )
      }

    override def checkSchedule(newTick: Long): Boolean =
      newTick >= activeTick

    override def handleSchedule(
        actor: ActorRef[Activation],
        newTick: Long
    ): ActiveCore = {
      activationQueue.set(newTick, actor)
      this
    }

    override def takeNewActivations()
        : (Iterable[ActorRef[Activation]], ActiveCore) = {
      val toActivate = activationQueue.getAndRemoveSet(activeTick)
      val newActiveCore = copy(activeActors = activeActors.concat(toActivate))
      (toActivate, newActiveCore)
    }

  }
}
