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
  InactiveCore
}
import edu.ie3.util.scala.collection.immutable.PrioritySwitchBiSet

/** A scheduler core that activates actors in phases when active. This means
  * that only one actor at any given time is activated and the completion of
  * said actor is necessary before activating other actors scheduled for the
  * same tick.
  *
  * When multiple actors are scheduled for the same tick, they are always
  * activated in the same order, which is given by the initial scheduling of
  * said actors. Thus, if e.g. actor 1 has been scheduled for initialization
  * before actor 2, actor 1 will be activated before actor 2 for the
  * initialization tick and all consecutive ticks.
  */
object PhaseSwitchCore extends CoreFactory {

  override def create(): PhaseSwitchInactive =
    PhaseSwitchInactive(PrioritySwitchBiSet.empty, None)

  final case class PhaseSwitchInactive private (
      private val activationQueue: PrioritySwitchBiSet[Long, Actor],
      private val lastActiveTick: Option[Long]
  ) extends InactiveCore {
    override def activate(newTick: Long): ActiveCore = {
      val nextScheduledTick = activationQueue.headKeyOption.getOrElse(
        throw new CriticalFailureException(
          "No activation scheduled, cannot activate."
        )
      )

      if (nextScheduledTick != newTick)
        throw new CriticalFailureException(
          s"Cannot activate with new tick $newTick because $nextScheduledTick is the next scheduled tick."
        )

      PhaseSwitchActive(activationQueue, activeTick = newTick)
    }

    override def handleSchedule(
        actor: Actor,
        newTick: Long
    ): (Option[Long], InactiveCore) = {
      lastActiveTick.filter(newTick < _).foreach { lastActive =>
        throw new CriticalFailureException(
          s"Cannot schedule an activation for $actor at tick $newTick because the last active tick is $lastActive"
        )
      }

      val oldEarliestTick = activationQueue.headKeyOption

      val updatedQueue = activationQueue.set(newTick, actor)
      val newEarliestTick = updatedQueue.headKeyOption

      val maybeScheduleTick =
        Option
          .when(newEarliestTick != oldEarliestTick)(newEarliestTick)
          .flatten

      (maybeScheduleTick, copy(activationQueue = updatedQueue))
    }

  }

  private final case class PhaseSwitchActive(
      private val activationQueue: PrioritySwitchBiSet[Long, Actor],
      activeTick: Long,
      private val phase: Int = 0,
      private val activeActors: Set[Actor] = Set.empty
  ) extends ActiveCore {

    override def handleCompletion(actor: Actor): ActiveCore = {
      if (!activeActors.contains(actor))
        throw new CriticalFailureException(
          s"Actor $actor is not part of the expected completing actors"
        )

      copy(activeActors = activeActors.excl(actor))
    }

    override def maybeComplete(): Option[(Option[Long], InactiveCore)] = {
      Option.when(
        activeActors.isEmpty && activationQueue.headKeyOption.forall(
          _ > activeTick
        )
      ) {
        (
          activationQueue.headKeyOption,
          PhaseSwitchInactive(activationQueue, Some(activeTick))
        )
      }
    }

    override def handleSchedule(
        actor: Actor,
        newTick: Long
    ): ActiveCore = {
      if (newTick == activeTick) {
        // what's done, is done: old phases are completed,
        // thus they cannot handle new activation schedulings
        if (activationQueue.indexOf(actor).exists(_ < phase)) {
          val activeActor = activationQueue.values(phase)
          throw new CriticalFailureException(
            s"Cannot schedule an activation at tick $newTick for $actor while actor $activeActor is active now"
          )
        }
      } else {
        if (newTick < activeTick)
          throw new CriticalFailureException(
            s"Cannot schedule an activation at tick $newTick for $actor while tick $activeTick is currently active"
          )
      }

      copy(activationQueue = activationQueue.set(newTick, actor))
    }

    override def takeNewActivations(): (Iterable[Actor], ActiveCore) = {
      Option
        .when(activeActors.isEmpty) {
          // only one actor can be active at a time, and only
          // if the last actor of the current tick has completed
          activationQueue.takeNextValueFor(activeTick)
        }
        .flatten
        .map { case (actor, updatedQueue) =>
          val newPhase = activationQueue
            .indexOf(actor)
            .getOrElse(
              throw new RuntimeException(
                "Actor not scheduled, should not happen"
              )
            )
          (
            Iterable.single(actor),
            copy(
              activationQueue = updatedQueue,
              phase = newPhase,
              activeActors = activeActors.incl(actor)
            )
          )
        }
        .getOrElse((Iterable.empty, this))
    }

  }

}
