/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler.core

import edu.ie3.simona.ontology.messages.Activation
import org.apache.pekko.actor.typed.ActorRef

/** The core of a [[edu.ie3.simona.scheduler.Scheduler]] provides all
  * functionality used to schedule, activate and complete actors within the
  * discrete event framework.
  */
object Core {

  private[scheduler] type Actor = ActorRef[Activation]

  /** Factory for cores used by [[edu.ie3.simona.scheduler.Scheduler]].
    */
  trait CoreFactory {

    /** Creates a new instance of an (inactive) core.
      */
    def create(): InactiveCore
  }

  /** Data structure holding relevant data and providing methods that handle
    * interactions with an inactive [[edu.ie3.simona.scheduler.Scheduler]]
    */
  trait InactiveCore {

    /** Checks whether an activation of the scheduler is valid for given tick.
      * This method cannot change the state of the inactive scheduler core.
      *
      * @param newTick
      *   The tick that the scheduler is to be activated with
      * @return
      *   true if activation with given tick is allowed, false if not
      */
    def checkActivation(newTick: Long): Boolean

    /** This method should be called when the scheduler is activated. An
      * [[ActiveCore]] is returned with the active tick set to the earliest tick
      * scheduled.
      *
      * @return
      *   The [[ActiveCore]] that should be used for the activated scheduler
      */
    def activate(): ActiveCore

    /** Checks whether scheduling an activation of an actor for given tick is
      * valid. This method cannot change the state of the inactive scheduler
      * core.
      *
      * @param newTick
      *   The tick that the actor wants to be scheduled for
      * @return
      *   true if scheduling the activation is allowed, false if not
      */
    def checkSchedule(newTick: Long): Boolean

    /** Handles the scheduling of an activation of given actor for given tick.
      * If this activation scheduling makes a separate scheduling of the current
      * scheduler with its parent necessary, the tick that the scheduler needs
      * to be scheduled for is returned.
      *
      * @param actor
      *   The actor to be scheduled
      * @param newTick
      *   The tick that the actor is scheduled for
      * @return
      *   A tuple of the optional tick that the current scheduler should be
      *   scheduled for with its parent, and the changed [[InactiveCore]]
      */
    def handleSchedule(
        actor: Actor,
        newTick: Long,
    ): (Option[Long], InactiveCore)
  }

  /** Data structure holding relevant data and providing methods that handle
    * interactions with an active [[edu.ie3.simona.scheduler.Scheduler]]
    */
  trait ActiveCore {

    /** Returns the currently active tick
      *
      * @return
      *   The active tick
      */
    def activeTick: Long

    /** Checks whether completing an activation of given actor for the currently
      * active tick is valid. This method cannot change the state of the active
      * scheduler core.
      *
      * @param actor
      *   The actor that wants to be complete its activation
      * @return
      *   true if the completion is allowed, false if not
      */
    def checkCompletion(actor: Actor): Boolean

    /** Handles the completion of an activation of given actor for the currently
      * active tick.
      *
      * @param actor
      *   The actor whose activation should be completed
      * @return
      *   The changed [[ActiveCore]]
      */
    def handleCompletion(actor: Actor): ActiveCore

    /** Checks whether the current activation of the scheduler can be completed,
      * which is usually the case when all activated actors have completed and
      * there are no new activations that can be sent out for the current tick.
      *
      * @return
      *   If the current activation of the scheduler can be completed, a tuple
      *   is returned of an optional tick that the scheduler should be scheduled
      *   for again, and the [[InactiveCore]] data that should be used in the
      *   following inactive state.
      */
    def maybeComplete(): Option[(Option[Long], InactiveCore)]

    /** Checks whether scheduling an activation of an actor for given tick is
      * valid. This method cannot change the state of the active scheduler core.
      *
      * @param actor
      *   The actor that wants to be scheduled for the given tick
      * @param newTick
      *   The tick that the actor wants to be scheduled for
      * @return
      *   true if scheduling the activation is allowed, false if not
      */
    def checkSchedule(actor: Actor, newTick: Long): Boolean

    /** Handles the scheduling of an activation of given actor for given tick.
      *
      * @param actor
      *   The actor to be scheduled
      * @param newTick
      *   The tick that the actor is scheduled for
      * @return
      *   The changed [[ActiveCore]]
      */
    def handleSchedule(actor: Actor, newTick: Long): ActiveCore

    /** Removes and returns activations scheduled for the current tick, which
      * can be sent out at the current moment.
      *
      * @return
      *   A tuple of a collection of actors scheduled for the current tick, and
      *   the changed [[ActiveCore]]
      */
    def takeNewActivations(): (Iterable[Actor], ActiveCore)
  }

}
