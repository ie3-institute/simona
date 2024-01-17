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

    override def tryActivate(newTick: Long): Either[String, ActiveCore] =
      activationQueue.headKeyOption
        .toRight("No activation scheduled, cannot activate.")
        .flatMap { nextScheduledTick =>
          Either.cond(
            nextScheduledTick == newTick,
            SchedulerActive(activationQueue, activeTick = newTick),
            s"Cannot activate with new tick $newTick because $nextScheduledTick is the next scheduled tick."
          )
        }

    override def tryHandleSchedule(
        actor: Actor,
        newTick: Long
    ): Either[String, (Option[Long], InactiveCore)] = Either.cond(
      lastActiveTick.forall(newTick >= _ + 1), {
        val oldEarliestTick = activationQueue.headKeyOption

        activationQueue.set(newTick, actor)
        val newEarliestTick = activationQueue.headKeyOption

        val maybeScheduleTick =
          Option
            .when(newEarliestTick != oldEarliestTick)(newEarliestTick)
            .flatten

        (maybeScheduleTick, this)
      },
      s"Cannot schedule an activation for $actor at tick $newTick because the last active tick is $lastActiveTick"
    )

  }

  private final case class SchedulerActive(
      private val activationQueue: PriorityMultiBiSet[Long, Actor],
      private val activeActors: Set[Actor] = Set.empty,
      activeTick: Long
  ) extends ActiveCore {

    override def tryHandleCompletion(actor: Actor): Either[String, ActiveCore] =
      Either.cond(
        activeActors.contains(actor),
        copy(activeActors = activeActors.excl(actor)),
        s"Actor $actor is not part of the expected completing actors"
      )

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

    override def tryHandleSchedule(
        actor: Actor,
        newTick: Long
    ): Either[String, ActiveCore] = Either.cond(
      newTick >= activeTick, {
        activationQueue.set(newTick, actor)
        this
      },
      s"Cannot schedule an activation at tick $newTick"
    )

    override def takeNewActivations(): (Iterable[Actor], ActiveCore) = {
      val toActivate = activationQueue.getAndRemoveSet(activeTick)
      val newActiveCore = copy(activeActors = activeActors.concat(toActivate))
      (toActivate, newActiveCore)
    }

  }
}
