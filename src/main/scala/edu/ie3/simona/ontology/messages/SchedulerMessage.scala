/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages

import akka.actor.ActorRef
import edu.ie3.simona.ontology.trigger.Trigger
import edu.ie3.simona.scheduler.SimScheduler

sealed trait SchedulerMessage

/** Messages that should be send and received to and from [[SimScheduler]].
  * Every message that is NOT a one way message (e.g. is only received by the
  * [[SimScheduler]] should include the [[ActorRef]] of the agent that should
  * receive the message. This is necessary for routing in cluster mode.
  */
object SchedulerMessage {

  /** Tell the [[SimScheduler]] to initialize the simulation with all
    * [[edu.ie3.simona.ontology.trigger.Trigger.InitializeTrigger]] s
    */
  case object InitSimMessage extends SchedulerMessage

  /** Tell the [[SimScheduler]] to start the simulation
    */
  final case class StartScheduleMessage(
      pauseScheduleAtTick: Option[Long] = None
  ) extends SchedulerMessage

  /** schedule a new trigger TO the [[SimScheduler]]. This message should send
    * only to the [[SimScheduler]].
    *
    * Interface Trigger is extended so that ScheduleTriggerMessages can be
    * stacked.
    *
    * @param trigger
    *   to schedule
    * @param actorToBeScheduled
    *   the agent that should receive the trigger
    */
  final case class ScheduleTriggerMessage(
      trigger: Trigger,
      actorToBeScheduled: ActorRef,
      priority: Boolean = false
  ) extends SchedulerMessage
      with Trigger {
    // FIXME this currently only works as intended for triggers scheduled for the current tick
    override def tick: Long = trigger.tick
  }

  /** Confirm the end of an action e.g. fsm state transitions for one tick to
    * and ONLY to the [[SimScheduler]]
    *
    * @param triggerId
    *   the triggerId we want to confirm the completion
    * @param newTriggers
    *   optional new triggers to schedule
    */
  final case class CompletionMessage(
      triggerId: Long,
      newTriggers: Option[Seq[ScheduleTriggerMessage]] = None
  ) extends SchedulerMessage

  /** a message that is send by the scheduler to an agent including an unique id
    * to keep track of it until a corresponding completion message is received
    */
  final case class TriggerWithIdMessage(
      trigger: Trigger,
      triggerId: Long,
      receiverActor: ActorRef
  ) extends SchedulerMessage

  /** respond to agent that the send trigger is illegal
    */
  final case class IllegalTriggerMessage(
      reason: String,
      receiverActor: ActorRef
  ) extends SchedulerMessage

  /** Reported back from the scheduler if an error occurred during the
    * simulation
    */
  case object SimulationFailureMessage extends SchedulerMessage

  /** Reported back from the scheduler if the simulation terminated as expected
    */
  case object SimulationSuccessfulMessage extends SchedulerMessage

  /** Reported back by the superior [[edu.ie3.simona.agent.grid.GridAgent]] to
    * the scheduler of the power flow of the
    * [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] failed. If this message is not
    * send, the scheduler *assumes* that everything went well during power flow!
    */
  case object PowerFlowFailedMessage extends SchedulerMessage

}
