/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler.core

import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.scheduler.core.Core.{
  ActiveCore,
  Actor,
  CoreFactory,
  InactiveCore,
}
import edu.ie3.util.scala.collection.mutable.PriorityMultiBiSet

/** A regular scheduler core that activates all actors for the current tick and
  * waits for all actors to complete before advancing.
  */
object RegularSchedulerCore extends CoreFactory {

  override def create(): SchedulerInactive =
    SchedulerInactive(PriorityMultiBiSet.empty, None)

  final case class SchedulerInactive(
      private val activationQueue: PriorityMultiBiSet[Long, Actor],
      private val lastActiveTick: Option[Long],
  ) extends InactiveCore {

    override def activate(newTick: Long): ActiveCore = {
      val nextScheduledTick = activationQueue.headKeyOption.getOrElse(
        throw new CriticalFailureException(
          "No activation scheduled, cannot activate."
        )
      )

      if nextScheduledTick != newTick then
        throw new CriticalFailureException(
          s"Cannot activate with new tick $newTick because $nextScheduledTick is the next scheduled tick."
        )

      SchedulerActive(activationQueue, activeTick = newTick)
    }

    override def handleSchedule(
        actor: Actor,
        newTick: Long,
    ): (Option[Long], InactiveCore) = {
      lastActiveTick.filter(newTick <= _).foreach { lastActive =>
        throw new CriticalFailureException(
          s"Cannot schedule an activation for $actor at tick $newTick because the last active tick is $lastActive"
        )
      }

      val oldEarliestTick = activationQueue.headKeyOption

      activationQueue.set(newTick, actor)
      val newEarliestTick = activationQueue.headKeyOption

      val maybeScheduleTick =
        Option
          .when(newEarliestTick != oldEarliestTick)(newEarliestTick)
          .flatten

      (maybeScheduleTick, this)
    }

  }

  private final case class SchedulerActive(
      private val activationQueue: PriorityMultiBiSet[Long, Actor],
      private val activeActors: Set[Actor] = Set.empty,
      activeTick: Long,
  ) extends ActiveCore {

    override def handleCompletion(actor: Actor): ActiveCore = {
      if !activeActors.contains(actor) then
        throw new CriticalFailureException(
          s"Actor $actor is not part of the expected completing actors"
        )

      copy(activeActors = activeActors.excl(actor))
    }

    override def maybeComplete(): Option[(Option[Long], InactiveCore)] =
      Option.when(
        activeActors.isEmpty &&
          !activationQueue.headKeyOption.contains(activeTick)
      ) {
        (
          activationQueue.headKeyOption,
          SchedulerInactive(activationQueue, Some(activeTick)),
        )
      }

    override def handleSchedule(
        actor: Actor,
        newTick: Long,
    ): ActiveCore = {
      if newTick < activeTick then
        throw new CriticalFailureException(
          s"Cannot schedule an activation at tick $newTick"
        )

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
