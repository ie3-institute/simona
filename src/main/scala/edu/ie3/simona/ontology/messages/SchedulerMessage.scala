/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages

import akka.actor
import akka.actor.ActorRef
import edu.ie3.simona.ontology.trigger.Trigger
import edu.ie3.simona.scheduler.ScheduleLock.LockMsg

import java.util.UUID

sealed trait SchedulerMessage

/** Messages that should be send and received to and from
  * [[edu.ie3.simona.scheduler.Scheduler]]. Every message that is NOT a one way
  * message (e.g. is only received by the [[edu.ie3.simona.scheduler.Scheduler]]
  * should include the [[ActorRef]] of the agent that should receive the
  * message. This is necessary for routing in cluster mode.
  */
object SchedulerMessage {

  /** Tell the [[edu.ie3.simona.scheduler.Scheduler]] to initialize the
    * simulation with all
    * [[edu.ie3.simona.ontology.trigger.Trigger.InitializeTrigger]] s
    */
  case object InitSimMessage extends SchedulerMessage

  /** schedule a new trigger TO the [[edu.ie3.simona.scheduler.Scheduler]]. This
    * message should send only to the [[edu.ie3.simona.scheduler.Scheduler]]
    *
    * @param trigger
    *   to schedule
    * @param actorToBeScheduled
    *   the agent that should receive the trigger
    */
  final case class ScheduleTriggerMessage(
      trigger: Trigger,
      actorToBeScheduled: ActorRef,
      unlockKey: Option[(actor.typed.ActorRef[LockMsg], UUID)] = None
  ) extends SchedulerMessage

  /** Confirm the end of an action e.g. fsm state transitions for one tick to
    * and ONLY to the [[edu.ie3.simona.scheduler.Scheduler]]
    *
    * @param triggerId
    *   the triggerId we want to confirm the completion
    * @param newTrigger
    *   optional new trigger to schedule
    */
  final case class CompletionMessage(
      triggerId: Long,
      newTrigger: Option[ScheduleTriggerMessage] = None
  ) extends SchedulerMessage

  /** a message that is send by the scheduler to an agent including an unique id
    * to keep track of it until a corresponding completion message is received
    */
  final case class TriggerWithIdMessage(
      trigger: Trigger,
      triggerId: Long
  ) extends SchedulerMessage
      with LockMsg

  /** respond to agent that the send trigger is illegal
    */
  final case class IllegalTriggerMessage(
      reason: String,
      receiverActor: ActorRef
  ) extends SchedulerMessage

  /** Reported back by the superior [[edu.ie3.simona.agent.grid.GridAgent]] to
    * the scheduler of the power flow of the
    * [[edu.ie3.simona.agent.grid.DBFSAlgorithm]] failed. If this message is not
    * send, the scheduler *assumes* that everything went well during power flow!
    */
  case object PowerFlowFailedMessage extends SchedulerMessage

}
