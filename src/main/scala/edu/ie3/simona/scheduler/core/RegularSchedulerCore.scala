/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler.core

import edu.ie3.simona.scheduler.core.Core.{
  ActiveCore,
  Actor,
  CoreFactory,
  InactiveCore
}
import edu.ie3.util.scala.collection.mutable.PriorityMultiBiSet

/** A regular scheduler core that activates all actors for the current tick and
  * waits for all actors to complete before advancing.
  */
object RegularSchedulerCore extends CoreFactory {

  override def create(): SchedulerInactive =
    SchedulerInactive(PriorityMultiBiSet.empty, None)

  final case class SchedulerInactive private (
      private val activationQueue: PriorityMultiBiSet[Long, Actor],
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
        actor: Actor,
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

  private final case class SchedulerActive(
      private val activationQueue: PriorityMultiBiSet[Long, Actor],
      private val activeActors: Set[Actor] = Set.empty,
      activeTick: Long
  ) extends ActiveCore {
    override def checkCompletion(actor: Actor): Boolean =
      activeActors.contains(actor)

    override def handleCompletion(actor: Actor): ActiveCore =
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

    override def checkSchedule(actor: Actor, newTick: Long): Boolean =
      newTick >= activeTick

    override def handleSchedule(actor: Actor, newTick: Long): ActiveCore = {
      activationQueue.set(newTick, actor)
      this
    }

    override def takeNewActivations(): (Iterable[Actor], ActiveCore) = {
      val toActivate = activationQueue.getAndRemoveSet(activeTick)
      val newActiveCore = copy(activeActors = activeActors.concat(toActivate))
      (toActivate, newActiveCore)
    }

  }
}
