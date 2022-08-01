/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import akka.actor.ActorSystem
import edu.ie3.simona.agent.participant.em.EmSchedulerStateData.TriggerData
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  IllegalTriggerMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.trigger.{ScheduledTrigger, Trigger}
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeTrigger
}
import edu.ie3.simona.util.SimonaConstants.PARALLELISM_WINDOW
import edu.ie3.util.scala.CountingMap

/** Main functionalities of [[EmScheduler]]. While the entry points for the
  * methods can be found in [[EmScheduler]], the functionalities that are
  * carried out are located here
  */
trait EmSchedulerHelper {
  this: EmScheduler =>

  protected final implicit val system: ActorSystem = context.system

  protected val parallelWindow: Long = PARALLELISM_WINDOW

  /** Send out all trigger that are eligible to be send for the current state of
    * the state data
    *
    * @param triggerData
    *   the trigger data that should be updated
    * @param nowInTicks
    *   the current tick
    * @return
    *   conditionally modified trigger data with updated trigger information
    */
  protected def sendEligibleTrigger(
      triggerData: TriggerData,
      nowInTicks: Long
  ): TriggerData = {
    val triggerQueue = triggerData.triggerQueue

    def triggerAvailable: Boolean =
      triggerQueue.nonEmpty && triggerQueue.headKey <= nowInTicks

    if (triggerAvailable) {
      val triggerIdToSTMapBuilder = Map.newBuilder[Long, ScheduledTrigger]
      triggerIdToSTMapBuilder ++= triggerData.triggerIdToScheduledTriggerMap

      /* send out eligible trigger */
      // this routine changes mutable data inside the loop for performance reasons
      while (triggerAvailable) {

        val scheduledTrigger = triggerQueue.poll()
        val triggerWithIdMessage = scheduledTrigger.triggerWithIdMessage

        // track that we wait for a response for this tick
        triggerData.awaitingResponseMap.add(triggerWithIdMessage.trigger.tick)

        // track the trigger id with the scheduled trigger
        triggerIdToSTMapBuilder += (
          triggerWithIdMessage.triggerId ->
            scheduledTrigger
        )

        CustomKamon.countTriggerMessage(
          triggerWithIdMessage,
          getClass,
          init = triggerWithIdMessage.trigger match {
            case _: InitializeTrigger => true
            case _                    => false
          }
        )

        scheduledTrigger.agent ! triggerWithIdMessage
      }

      triggerData.copy(
        triggerQueue = triggerQueue,
        awaitingResponseMap = triggerData.awaitingResponseMap,
        triggerIdToScheduledTriggerMap = triggerIdToSTMapBuilder.result()
      )
    } else
      triggerData
  }

  /** Main method to handle all [[CompletionMessage]]s received by the
    * scheduler. Based on the received completion message, the provided
    * triggerData is updated. In particular the awaitingResponseMap and the
    * triggerIdToScheduledTriggerMap are updated if the provided
    * [[CompletionMessage]] is valid. For an invalid message, the data is not
    * modified and the error is logged.
    *
    * A copy of the provided trigger data with updated trigger information is
    * returned.
    *
    * @param completionMessage
    *   the completion message that should be processed
    * @param inputTriggerData
    *   the trigger data that should be updated
    * @param nowInTicks
    *   the current tick
    * @return
    *   trigger data with updated trigger and maybe updated runtime information
    */
  protected final def handleCompletionMessage(
      completionMessage: CompletionMessage,
      inputTriggerData: TriggerData,
      nowInTicks: Long
  ): TriggerData = {

    /* schedule new triggers, if any */
    val triggerData = completionMessage.newTriggers
      .map(
        _.foldLeft(inputTriggerData)((triggerData, newTrigger) =>
          scheduleTrigger(newTrigger, triggerData, nowInTicks)
        )
      )
      .getOrElse(inputTriggerData)
    /* after scheduling the new triggers, if any, we go on with either the same or updated state data */

    val triggerId = completionMessage.triggerId

    // count message
    val isInit = triggerData.triggerIdToScheduledTriggerMap
      .get(completionMessage.triggerId)
      .exists {
        _.triggerWithIdMessage.trigger match {
          case _: InitializeTrigger => true
          case _                    => false
        }
      }

    val (
      updatedAwaitingResponseMap,
      updatedTriggerIdToScheduledTriggerMap
    ) = updateAwaitingResponseAndTriggerIdToScheduledTriggerMap(
      triggerId,
      triggerData.awaitingResponseMap,
      triggerData.triggerIdToScheduledTriggerMap
    ).getOrElse {
      log.error(
        s"Received bad completion notice $completionMessage from ${completionMessage.actor}"
      )
      (
        triggerData.awaitingResponseMap,
        triggerData.triggerIdToScheduledTriggerMap
      )
    }

    /* unwatch sender */
    context.unwatch(completionMessage.actor)

    triggerData.copy(
      awaitingResponseMap = updatedAwaitingResponseMap,
      triggerIdToScheduledTriggerMap = updatedTriggerIdToScheduledTriggerMap
    )
  }

  /** Updates awaitingResponse and triggerIdToScheduledTriggerMap with the
    * provided data if the provided triggerId is valid
    *
    * @param triggerId
    *   the trigger id that should be processed
    * @param awaitingResponseMap
    *   the mapping of ticks to scheduled trigger that are send out and a
    *   completion message has not been received yet
    * @param triggerIdToScheduledTriggerMap
    *   the mapping of the trigger id to the send out trigger
    * @return
    *   either a tuple with an updated awaiting response map and updated trigger
    *   id to scheduled trigger map or none if the provided trigger id is
    *   invalid
    */
  private def updateAwaitingResponseAndTriggerIdToScheduledTriggerMap(
      triggerId: Long,
      awaitingResponseMap: CountingMap[Long],
      triggerIdToScheduledTriggerMap: Map[Long, ScheduledTrigger]
  ): Option[
    (
        CountingMap[Long],
        Map[Long, ScheduledTrigger]
    )
  ] = {
    triggerIdToScheduledTriggerMap.get(triggerId) match {
      case None =>
        None
      case Some(scheduledTrigger) =>
        val tick = scheduledTrigger.triggerWithIdMessage.trigger.tick
        if (awaitingResponseMap.contains(tick)) {

          awaitingResponseMap.subtract(tick)

          val updatedTriggerIdToScheduledTriggerMap =
            triggerIdToScheduledTriggerMap - triggerId

          Some(
            awaitingResponseMap,
            updatedTriggerIdToScheduledTriggerMap
          )
        } else {
          None
        }
    }
  }

  /** Adds the provided trigger to the trigger queue to schedule it at the
    * requested tick
    *
    * @param triggerMessage
    *   message containing trigger to be scheduled and it's receiver
    * @param triggerData
    *   the trigger data that should be updated
    * @param nowInTicks
    *   the current tick
    * @return
    *   a copy of the provided state data with updated trigger data
    */
  protected final def scheduleTrigger(
      triggerMessage: SchedulerMessage.ScheduleTriggerMessage,
      triggerData: TriggerData,
      nowInTicks: Long
  ): TriggerData =
    scheduleTrigger(
      triggerMessage.trigger,
      triggerMessage.actorToBeScheduled,
      triggerData,
      nowInTicks
    )

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
      actorToBeScheduled: SimonaActorRef,
      triggerData: TriggerData,
      nowInTicks: Long
  ): TriggerData = {

    // watch the actor to be scheduled to know when it dies
    context.watch(actorToBeScheduled)

    // if the tick of this trigger is too far in the past, we cannot schedule it
    if (nowInTicks - trigger.tick > parallelWindow) {
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

  protected def sendEligibleTrigger(
      stateData: EmSchedulerStateData
  ): EmSchedulerStateData =
    stateData.copy(
      trigger = sendEligibleTrigger(stateData.trigger, stateData.nowInTicks)
    )

  protected final def handleCompletionMessage(
      completionMessage: CompletionMessage,
      stateData: EmSchedulerStateData
  ): EmSchedulerStateData =
    stateData.copy(
      trigger = handleCompletionMessage(
        completionMessage,
        stateData.trigger,
        stateData.nowInTicks
      )
    )

  protected final def scheduleTrigger(
      triggerMessage: SchedulerMessage.ScheduleTriggerMessage,
      stateData: EmSchedulerStateData
  ): EmSchedulerStateData =
    stateData.copy(
      trigger = scheduleTrigger(
        triggerMessage,
        stateData.trigger,
        stateData.nowInTicks
      )
    )

  /** Maybe send completion message to main scheduler
    * @param stateData
    *   the current state data
    * @return
    *   updated state data
    */
  protected def maybeTicksCompleted(
      stateData: EmSchedulerStateData
  ): EmSchedulerStateData = {
    val completedMainTicks =
      stateData.mainTrigger
        .flatMap {
          case (mainTick, Some(triggerId)) =>
            // since we only want to have main ticks that have already been triggered, we can unpack the trigger id
            Some((mainTick, triggerId))
          case _ =>
            // scheduled tick has been sent to main, but has not been triggered yet.
            // so, it can't be completed yet either.
            None
        }
        .filter { case (mainTick, _) =>
          stateData.trigger.awaitingResponseMap.minKey match {
            case Some(head) =>
              // main ticks that have been triggered and completed (no more awaited completion messages).
              // we do not check whether trigger has been init or not, as non-init-triggers are sent
              // only after init-triggers are sent
              mainTick < head
            case None =>
              // we're not waiting for any ticks any more, so all remaining main ticks are good to go
              true
          }
        }

    completedMainTicks.foldLeft(stateData) {
      case (currentStateData, (mainTick, mainTriggerId)) =>
        val nextTick =
          currentStateData.trigger.triggerQueue.keySet.collectFirst {
            // first tick that has not been scheduled with main scheduler.
            case tick if !currentStateData.mainTrigger.contains(tick) =>
              tick
          }

        val nextTriggerOpt = nextTick.map(tick =>
          Seq(
            ScheduleTriggerMessage(
              ActivityStartTrigger(tick),
              selfSharded(currentStateData.subnetNo)
            )
          )
        )

        mainScheduler ! CompletionMessage(
          mainTriggerId,
          selfSharded(currentStateData.subnetNo),
          nextTriggerOpt
        )

        currentStateData.copy(
          mainTrigger = currentStateData.mainTrigger
            - mainTick
            ++ nextTick.map(tick => (tick, None))
        )
    }
  }

}
