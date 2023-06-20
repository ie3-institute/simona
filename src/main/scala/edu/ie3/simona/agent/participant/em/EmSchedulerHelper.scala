/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import akka.actor.{ActorRef, ActorSystem}
import edu.ie3.simona.agent.participant.em.EmSchedulerStateData.{
  FlexTriggerData,
  ScheduledFlexTrigger,
  TriggerData
}
import edu.ie3.simona.ontology.messages.FlexibilityMessage.{
  FlexCtrlCompletion,
  RequestFlexOptions
}
import edu.ie3.simona.ontology.messages.{FlexibilityMessage, SchedulerMessage}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  IllegalTriggerMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.trigger.Trigger
import edu.ie3.simona.scheduler.SimSchedulerStateData.ScheduledTrigger

import java.util.UUID

/** Main functionalities of scheduling within [[EmAgent]]
  */
trait EmSchedulerHelper {
  this: EmAgent =>

  protected final implicit val system: ActorSystem = context.system

  /** Main method to handle all [[CompletionMessage]]s received by the
    * scheduler. Based on the received completion message, the provided
    * stateData is updated. In particular the awaitingResponseMap and the
    * triggerIdToScheduledTriggerMap are updated if the provided
    * [[CompletionMessage]] is valid. For an invalid message, the data is not
    * modified and the error is logged.
    *
    * A copy of the provided trigger data with updated trigger information is
    * returned.
    *
    * @param completionMessage
    *   the completion message that should be processed
    * @param inputStateData
    *   the scheduler state data that should be updated
    * @return
    *   trigger data with updated trigger and maybe updated runtime information
    */
  protected final def handleCompletionMessage(
      completionMessage: CompletionMessage,
      inputStateData: EmSchedulerStateData
  ): EmSchedulerStateData = {

    /* schedule new triggers, if any */
    val stateData = completionMessage.newTriggers
      .map(
        _.foldLeft(inputStateData)((stateData, newTrigger) =>
          scheduleTrigger(newTrigger, stateData)
        )
      )
      .getOrElse(inputStateData)
    /* after scheduling the new triggers, if any, we go on with either the same or updated state data */

    val triggerId = completionMessage.triggerId

    if (!stateData.trigger.awaitedTriggerMap.contains(triggerId)) {
      log.warning(
        "Trigger id {} has not been awaited in trigger map {}",
        triggerId,
        stateData.trigger.awaitedTriggerMap
      )
    }

    val updatedAwaitedTriggerMap =
      stateData.trigger.awaitedTriggerMap -= triggerId

    stateData.copy(
      stateData.trigger.copy(
        awaitedTriggerMap = updatedAwaitedTriggerMap
      )
    )
  }

  protected final def revokeFlexTrigger(
      modelUuid: UUID,
      revokeTick: Long,
      triggerData: FlexTriggerData
  ): FlexTriggerData = {
    triggerData.triggerQueue.remove(
      revokeTick,
      ScheduledFlexTrigger(RequestFlexOptions(revokeTick), modelUuid)
    )

    triggerData
  }

  protected final def handleFlexCompletionMessage(
      completionMessage: FlexCtrlCompletion,
      triggerData: FlexTriggerData
  ): FlexTriggerData = {

    // revoke trigger if applicable
    completionMessage.revokeRequestAtTick.foreach { revokeTick =>
      revokeFlexTrigger(completionMessage.modelUuid, revokeTick, triggerData)
    }

    // mark participant to be activated at next tick if applicable
    if (completionMessage.requestAtNextActivation)
      triggerData.activateAtNextTick += completionMessage.modelUuid

    // schedule new flex requests, if applicable
    completionMessage.requestAtTick
      .foreach(newTick =>
        scheduleFlexTriggerOnce(
          triggerData,
          RequestFlexOptions(newTick),
          completionMessage.modelUuid
        )
      )

    if (
      !triggerData.awaitedFlexCompletions.contains(completionMessage.modelUuid)
    ) {
      throw new RuntimeException(
        s"Completion for UUID ${completionMessage.modelUuid} is not part of expected completions: ${triggerData.awaitedFlexCompletions}"
      )
    }

    // remove from awaited flex completions
    triggerData.awaitedFlexCompletions -= completionMessage.modelUuid

    triggerData
  }

  /** Adds the provided trigger to the proper trigger queue to schedule it at
    * the requested tick
    *
    * @param triggerMessage
    *   message containing trigger to be scheduled and it's receiver
    * @param stateData
    *   the scheduler state data that should be updated
    * @return
    *   a copy of the provided state data with updated trigger data
    */
  protected final def scheduleTrigger(
      triggerMessage: SchedulerMessage.ScheduleTriggerMessage,
      stateData: EmSchedulerStateData
  ): EmSchedulerStateData = {
    triggerMessage.trigger match {
      case flexMessage: FlexibilityMessage =>
        val uuid = stateData.flexTrigger.actorRefToUuid(
          triggerMessage.actorToBeScheduled
        )
        stateData.copy(
          flexTrigger = scheduleFlexTriggerOnce(
            stateData.flexTrigger,
            flexMessage,
            uuid
          )
        )
      case trigger =>
        stateData.copy(
          trigger = scheduleTrigger(
            trigger,
            triggerMessage.actorToBeScheduled,
            stateData.trigger,
            stateData.nowInTicks
          )
        )
    }

  }

  /** Adds the provided trigger to the trigger queue to schedule it at the
    * requested tick
    *
    * @param trigger
    *   the trigger that should be scheduled
    * @param actorToBeScheduled
    *   the actor that should receive the trigger
    * @param triggerData
    *   the trigger data that should be updated
    * @param nowInTicks
    *   the current tick
    * @return
    *   a copy of the provided trigger data with updated trigger data
    */
  protected final def scheduleTrigger(
      trigger: Trigger,
      actorToBeScheduled: ActorRef,
      triggerData: TriggerData,
      nowInTicks: Long
  ): TriggerData = {

    // if the tick of this trigger is too far in the past, we cannot schedule it
    if (nowInTicks > trigger.tick) {
      actorToBeScheduled ! IllegalTriggerMessage(
        s"Cannot schedule an event $trigger at tick ${trigger.tick} when 'nowInSeconds' is at $nowInTicks!",
        actorToBeScheduled
      )

      triggerData

    } else {

      /* update trigger id counter & create new triggerWithIdMessage */
      val updatedTriggerIdCounter = triggerData.triggerIdCounter + 1
      val triggerWithIdMessage =
        TriggerWithIdMessage(
          trigger,
          updatedTriggerIdCounter,
          actorToBeScheduled
        )

      /* update trigger queue */
      triggerData.triggerQueue.add(
        trigger.tick,
        ScheduledTrigger(
          triggerWithIdMessage,
          actorToBeScheduled
        )
      )

      /* return copy of state data */
      triggerData.copy(
        triggerQueue = triggerData.triggerQueue,
        triggerIdCounter = updatedTriggerIdCounter
      )
    }
  }

  protected def scheduleFlexRequestsOnce(
      flexTrigger: FlexTriggerData,
      participantUuids: Set[UUID],
      tick: Long
  ): FlexTriggerData =
    participantUuids.foldLeft(flexTrigger) { case (schedulerData, modelUuid) =>
      scheduleFlexTriggerOnce(
        schedulerData,
        RequestFlexOptions(tick),
        modelUuid
      )
    }

  protected def scheduleFlexTriggerOnce(
      flexTrigger: FlexTriggerData,
      trigger: Trigger with FlexibilityMessage,
      modelUuid: UUID
  ): FlexTriggerData = {
    val scheduledTrigger = ScheduledFlexTrigger(trigger, modelUuid)

    // adds only if not present already
    flexTrigger.triggerQueue.add(
      trigger.tick,
      scheduledTrigger
    )

    flexTrigger
  }

  protected def sendEligibleTrigger(
      stateData: EmSchedulerStateData
  ): EmSchedulerStateData = {

    // it's important that ActivityStartTriggers are sent out before flex triggers!
    stateData.trigger.triggerQueue.pollTo(stateData.nowInTicks).foreach {
      case scheduledTrigger @ ScheduledTrigger(triggerWithIdMessage, actor) =>
        // track the trigger id with the scheduled trigger
        stateData.trigger.awaitedTriggerMap +=
          triggerWithIdMessage.triggerId -> scheduledTrigger

        actor ! triggerWithIdMessage
    }

    stateData.flexTrigger.triggerQueue.pollTo(stateData.nowInTicks).foreach {
      case ScheduledFlexTrigger(trigger, modelUuid) =>
        val actor = stateData.flexTrigger.uuidToActorRef(modelUuid)

        stateData.flexTrigger.awaitedFlexCompletions += modelUuid

        actor ! trigger
    }

    stateData
  }

  protected final def handleFlexCompletionMessage(
      completionMessage: FlexCtrlCompletion,
      stateData: EmSchedulerStateData
  ): EmSchedulerStateData =
    stateData.copy(
      flexTrigger = handleFlexCompletionMessage(
        completionMessage,
        stateData.flexTrigger
      )
    )

  /** Schedule those actors that requested to be activated at the very next tick
    *
    * @param stateData
    *   The state data containing the actors to be scheduled
    * @param tick
    *   The tick that the actors should be scheduled for
    * @return
    *   The updated scheduler state data
    */
  protected def scheduleFlexRequestAtNextTick(
      stateData: EmSchedulerStateData,
      tick: Long
  ): EmSchedulerStateData =
    if (stateData.flexTrigger.activateAtNextTick.nonEmpty) {
      val updatedFlexTrigger = scheduleFlexRequestsOnce(
        stateData.flexTrigger,
        stateData.flexTrigger.activateAtNextTick.toSet,
        tick
      )

      // remove all, they should only be scheduled once
      updatedFlexTrigger.activateAtNextTick.clear()

      stateData.copy(flexTrigger = updatedFlexTrigger)
    } else
      stateData

  /** Maybe send completion message to main scheduler
    * @param stateData
    *   the current state data
    * @param scheduledRequest
    *   the last scheduled request (if this EmAgent is EM-controlled)
    * @return
    *   updated state data and possibly the scheduled tick
    */
  protected def maybeTicksCompleted(
      stateData: EmSchedulerStateData,
      selfUuid: UUID,
      scheduledRequest: Option[Long]
  ): (EmSchedulerStateData, Option[Long]) = {

    // since participants that are scheduled with an ActivityStartTrigger are also
    // scheduled for a flex options request, checking flex options is enough here
    val nextOpt = getNextScheduledTick(stateData)

    val allSent = nextOpt.forall(_ > stateData.nowInTicks)
    val allCompleted =
      stateData.trigger.awaitedTriggerMap.isEmpty &&
        stateData.flexTrigger.awaitedFlexCompletions.isEmpty

    (allSent, allCompleted, stateData.mainTriggerId) match {
      case (true, true, Some(mainTriggerId)) =>
        val nextTriggerOpt =
          nextOpt.map(tick =>
            Seq(
              ScheduleTriggerMessage(
                stateData.createMainTrigger(tick),
                self
              )
            )
          )

        scheduler ! CompletionMessage(
          mainTriggerId,
          nextTriggerOpt
        )

        (
          stateData.copy(
            mainTriggerId = None
          ),
          nextOpt
        )
      case (true, true, None) =>
        // if we've received a RequestFlexOptions message,
        // we want to complete with FlexCtrlCompletion

        val revokeRequest = scheduledRequest.filter {
          // revoke old tick if it exists and is placed in the future
          _ > stateData.nowInTicks
        }

        // no activation at next tick for speedier execution
        // ( stateData.flexTrigger.activateAtNextTick.nonEmpty )
        scheduler ! FlexCtrlCompletion(
          selfUuid,
          requestAtTick = nextOpt,
          revokeRequestAtTick = revokeRequest
        )

        (stateData, nextOpt)
      case _ => (stateData, scheduledRequest)
    }

  }

  protected def getNextScheduledTick(
      stateData: EmSchedulerStateData
  ): Option[Long] = {

    val nextTickOpt = stateData.trigger.triggerQueue.headKeyOption
    val nextFlexTickOpt = stateData.flexTrigger.triggerQueue.headKeyOption

    (nextTickOpt, nextFlexTickOpt) match {
      case (Some(nextTick), Some(nextFlexTick)) =>
        Some(math.min(nextTick, nextFlexTick))
      case (Some(_), _) => nextTickOpt
      case (_, Some(_)) => nextFlexTickOpt
      case _            => None
    }

  }
}
