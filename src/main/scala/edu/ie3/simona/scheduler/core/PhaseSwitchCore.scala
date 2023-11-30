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
import edu.ie3.util.scala.collection.immutable.PrioritySwitchBiSet
import org.apache.pekko.actor.typed.ActorRef

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
      private val activationQueue: PrioritySwitchBiSet[Long, ActorRef[
        Activation
      ]],
      private val lastActiveTick: Option[Long]
  ) extends InactiveCore {
    override def checkActivation(newTick: Long): Boolean =
      activationQueue.headKeyOption.contains(newTick)

    override def activate(): Core.ActiveCore = {
      val nextActiveTick = activationQueue.headKeyOption.getOrElse(
        throw new RuntimeException("Nothing scheduled, cannot activate.")
      )
      PhaseSwitchActive(activationQueue, activeTick = nextActiveTick)
    }

    override def checkSchedule(newTick: Long): Boolean =
      lastActiveTick.forall(newTick >= _ + 1)

    override def handleSchedule(
        actor: ActorRef[Activation],
        newTick: Long
    ): (Option[Long], InactiveCore) = {
      val oldEarliestTick = activationQueue.headKeyOption

      val updatedQueue = activationQueue.set(newTick, actor)
      val newEarliestTick = updatedQueue.headKeyOption

      val maybeScheduleTick =
        Option.when(newEarliestTick != oldEarliestTick)(newEarliestTick).flatten

      (maybeScheduleTick, copy(activationQueue = updatedQueue))
    }

  }

  private final case class PhaseSwitchActive(
      private val activationQueue: PrioritySwitchBiSet[Long, ActorRef[
        Activation
      ]],
      activeTick: Long,
      private val phase: Int = 0,
      private val activeActors: Set[ActorRef[Activation]] = Set.empty
  ) extends ActiveCore {

    override def checkCompletion(actor: ActorRef[Activation]): Boolean =
      activeActors.contains(actor)

    override def handleCompletion(actor: ActorRef[Activation]): ActiveCore =
      copy(activeActors = activeActors.excl(actor))

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

    override def checkSchedule(
        actor: ActorRef[Activation],
        newTick: Long
    ): Boolean = {
      if (newTick == activeTick) {
        // what's done, is done: old phases are completed,
        // thus they cannot handle new activation schedulings
        activationQueue.indexOf(actor).forall(_ >= phase)
      } else
        newTick > activeTick
    }

    override def handleSchedule(
        actor: ActorRef[Activation],
        newTick: Long
    ): ActiveCore =
      copy(activationQueue = activationQueue.set(newTick, actor))

    override def takeNewActivations()
        : (Iterable[ActorRef[Activation]], ActiveCore) = {
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
