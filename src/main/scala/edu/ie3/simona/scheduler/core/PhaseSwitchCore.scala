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

/** TODO scaladoc
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

    override def checkSchedule(newTick: Long): Boolean = {
      if (newTick == activeTick) {
        // what's done, is done: old phases are completed,
        activationQueue.headKeyIndexOption.forall(_ >= phase)
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
        .when(activeActors.isEmpty) { // only one actor can be active at a time
          activationQueue.nextValueFor(activeTick)
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