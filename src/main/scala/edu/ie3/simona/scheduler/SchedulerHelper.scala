/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor.ActorRef
import edu.ie3.simona.event.RuntimeEvent.{
  CheckWindowPassed,
  Done,
  InitComplete,
  Ready
}
import edu.ie3.simona.exceptions.SchedulerException
import edu.ie3.simona.logging.SimonaActorLogging
import edu.ie3.simona.ontology.messages.SchedulerMessage._
import edu.ie3.simona.ontology.trigger.Trigger
import edu.ie3.simona.scheduler.SimSchedulerStateData.{
  ScheduledTrigger,
  SchedulerStateData,
  TriggerData
}
import edu.ie3.simona.util.SimonaConstants
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.collection.mutable.{CountingMap, PriorityMultiQueue}

import scala.annotation.tailrec
import scala.collection.mutable

/** Main functionalities of [[SimScheduler]]. While the entry points for the
  * methods can be found in [[SimScheduler#schedulerReceive()]], the
  * functionalities that are carried out from the [[SimScheduler]] are located
  * here
  *
  * @version 0.1
  * @since 04.05.20
  */
trait SchedulerHelper extends SimonaActorLogging {
  this: SimScheduler =>

  // user config parameters
  protected val endTick: Long = TimeUtil.withDefaults
    .zonedDateTimeDifferenceInSeconds(
      TimeUtil.withDefaults.toZonedDateTime(simonaTimeConfig.startDateTime),
      TimeUtil.withDefaults.toZonedDateTime(simonaTimeConfig.endDateTime)
    )

  // if currentTick % checkWindowTick == 0 and all completionMessages have been
  // received for currentTick, a CheckWindowPassed RuntimeEvent is issued
  private val schedulerReadyCheckWindow =
    simonaTimeConfig.schedulerReadyCheckWindow

  protected def sendEligibleTrigger(
      stateData: SchedulerStateData
  ): SchedulerStateData =
    stateData.copy(
      trigger =
        sendEligibleTrigger(stateData.trigger, stateData.time.nowInTicks)
    )

  /** Send out all trigger that are eligible to be send for the current state of
    * the state data. This method modifies the trigger data within the provided
    * [[SimSchedulerStateData]].
    *
    * @param triggerData
    *   the trigger data that should be updated
    * @param nowInTicks
    *   the current tick
    * @return
    *   possibly modified trigger data with updated trigger information
    */
  protected def sendEligibleTrigger(
      triggerData: TriggerData,
      nowInTicks: Long
  ): TriggerData = {

    triggerData.triggerQueue.pollTo(nowInTicks).foreach {
      case scheduledTrigger @ ScheduledTrigger(triggerWithIdMessage, actor) =>
        // track that we wait for a response for this tick
        triggerData.awaitingResponseMap.add(triggerWithIdMessage.trigger.tick)

        // track the trigger id with the scheduled trigger
        triggerData.triggerIdToScheduledTriggerMap +=
          triggerWithIdMessage.triggerId -> scheduledTrigger

        actor ! triggerWithIdMessage
    }

    triggerData
  }

  /** The actual scheduling and moving forward in time process if possible. This
    * method first decides whether the current tick should be used for a
    * simulation hold (if [[SchedulerStateData.time.pauseScheduleAtTick]] is not
    * None) or not. If yes, the simulation checks if there are trigger to send
    * out for the hold tick (which might be provided by [[CompletionMessage]] s
    * that have been received @ the hold tick and if yes sends them out. If no
    * new triggers for the hold tick are provided AND no completion messages are
    * left to receive for the hold tick, it stops the schedule. (see
    * [[this.doPauseInSimStep()]] for details)
    *
    * If [[SchedulerStateData.time.pauseScheduleAtTick]] is None, the normal
    * routine of time advancement is carried out. For details please see
    * documentation in the code below.
    *
    * @param stateData
    *   the state data that should be used
    * @return
    *   modified state data if advancement in time took place or the same state
    *   data if no time advancement took place
    */
  @tailrec
  protected final def doSimStep(
      stateData: SchedulerStateData
  ): SchedulerStateData = {

    val nowInTicks = stateData.time.nowInTicks

    stateData.time.pauseScheduleAtTick match {
      case Some(pauseScheduleAtTick) if pauseScheduleAtTick == nowInTicks - 1 =>
        /* we want to pause at this tick, the - 1 is necessary as in the recursive definition we
         * need to move on one more tick to send the trigger of the last tick */
        doPauseInSimStep(stateData, nowInTicks - 1)

      case _ =>
        /* we do not want to pause the schedule, go on with normal schedule handling  */
        if (notFinishedAndTriggerAvailable(nowInTicks, stateData)) {

          /* if we do not exceed nowInSeconds OR do not wait on any responses we can send out new triggers */
          if (
            canWeSendTrigger(
              stateData.trigger.awaitingResponseMap,
              nowInTicks
            )
          ) {

            /* send out eligible triggers, where eligible means all triggers with 'tick <= nowInTicks' */
            val eligibleTriggerSendStateData = sendEligibleTrigger(stateData)

            /* conditionally send information to listeners that all triggers for a specific step has been send out (= Ready)*/
            val updatedStateData = maybeCheckWindowPassed(
              eligibleTriggerSendStateData,
              schedulerReadyCheckWindow,
              eligibleTriggerSendStateData.time.nowInTicks
            )

            /* if we do not exceed (nowInTicks+1) OR do not wait on any responses, we can move on in time by one tick */
            if (
              nowInTicks <= endTick && canWeSendTrigger(
                updatedStateData.trigger.awaitingResponseMap,
                nowInTicks + 1
              )
            ) {
              doSimStep(
                updatedStateData.copy(
                  time = updatedStateData.time
                    .copy(nowInTicks = nowInTicks + 1)
                )
              )
            } else {
              /* we cannot move on in time for (nowInTicks+1), return updated data */
              updatedStateData
            }

          } else {
            /* we cannot send out more triggers, return unmodified state data  */
            stateData
          }

        } else {
          /* no triggers should be send out anymore as we reached the last tick - now we only have to
           * wait for all agents to reply with completion messages */

          /* Please keep this dead code for now at it is NOT 100% clear (but 95%) if this part is needed or not
          /* check if we passed the check window has been passed */
          val stateDataAfterCheckWindow = maybeCheckWindowPassed(
            stateData,
            schedulerReadyCheckWindow,
            nowInTicks) */

          maybeFinishSimulation(stateData)
        }
    }
  }

  /** Executes several routines if the simulation should pause at a specific
    * tick. The condition to pause has to be checked from the outside and the
    * tick to pause has to be pass in. The method then executes several checks
    * if more trigger needs to be send out, listeners needs to be informed with
    * specific events and so on. For details please take a look at the comments
    * in the code below.
    *
    * @param stateData
    *   the current state data that should be processed
    * @param pauseTick
    *   the pause tick
    * @return
    *   conditionally modified state data
    */
  private def doPauseInSimStep(
      stateData: SchedulerStateData,
      pauseTick: Long
  ): SchedulerStateData = {

    /* notify about the fact that the simulation stopped here, if we do not wait on trigger completion */
    val stateDataAfterReady = maybeReady(stateData, pauseTick)

    /* it might be possible that this pause tick is also the end tick, finish if we do not wait on trigger completion */
    val stateDataAfterFinish = if (endTick == pauseTick) {
      maybeFinishSimulation(stateDataAfterReady)
    } else {
      stateDataAfterReady
    }

    /* if we are in tick pauseScheduleAtTick, it might be possible that we still need to schedule triggers due to
     * incoming triggers for this tick inside completion messages  */
    if (
      !noScheduledTriggersForCurrentTick(
        stateData.trigger.triggerQueue,
        pauseTick
      )
    ) {
      /* send out eligible triggers, where eligible means all triggers with 'tick <= nowInTicks' */
      val eligibleTriggerSendStateData = sendEligibleTrigger(
        stateDataAfterFinish
      )

      val updatedStateData = maybeCheckWindowPassed(
        eligibleTriggerSendStateData,
        schedulerReadyCheckWindow,
        eligibleTriggerSendStateData.time.nowInTicks
      )
      updatedStateData

    } else {
      stateDataAfterFinish
    }
  }

  /** Check if a) the trigger queue is empty b) the next trigger's tick is after
    * the tick, the simulation is in, or after the last tick of the simulation
    *
    * @param triggerQueue
    *   Queue of triggers to be send out
    * @param nowInTicks
    *   Current tick, the simulation is in
    * @return
    *   a boolean
    */
  private def noScheduledTriggersForCurrentTick(
      triggerQueue: PriorityMultiQueue[Long, ScheduledTrigger],
      nowInTicks: Long
  ): Boolean =
    triggerQueue.headKeyOption match {
      case Some(nextTriggerTick) =>
        nextTriggerTick > nowInTicks || nextTriggerTick > endTick
      case None =>
        true
    }

  /** Checks if the simulation has not reached its endTick yet and if there are
    * still trigger available in the trigger queue that can be send out
    *
    * @param nowInTicks
    *   the current tick of the simulation
    * @param stateData
    *   the state data with the trigger data
    * @return
    *   either true if the end tick is not reached yet or if there are still
    *   trigger to be scheduled whose tick <= endTick, false otherwise
    */
  private def notFinishedAndTriggerAvailable(
      nowInTicks: Long,
      stateData: SchedulerStateData
  ): Boolean =
    nowInTicks <= endTick ||
      stateData.trigger.triggerQueue.headKeyOption.exists(_ <= endTick)

  /** Checks if we can move on in time in the schedule by comparing the awaiting
    * response map data with the current tick
    *
    * @param awaitingResponseMap
    *   the map containing all information about triggers we still wait for
    *   completion messages
    * @param nowInTicks
    *   the current tick
    * @return
    *   true if trigger can be send, false otherwise
    */
  private def canWeSendTrigger(
      awaitingResponseMap: CountingMap[Long],
      nowInTicks: Long
  ): Boolean =
    awaitingResponseMap.minKeyOption match {
      case Some(minKey) =>
        nowInTicks <= minKey
      case None =>
        true // map empty, no completions awaited
    }

  /** Checks if the provided state data and the current tick is eligible to
    * issue a [[CheckWindowPassed]] event
    *
    * @param stateData
    *   the current state data
    * @param readyCheckWindow
    *   the ready check window tick
    * @param nowInTicks
    *   the current tick
    * @return
    *   conditionally adapted state data or the provided state data
    */
  private def maybeCheckWindowPassed(
      stateData: SchedulerStateData,
      readyCheckWindow: Option[Int],
      nowInTicks: Long
  ): SchedulerStateData = {

    val readyCheckVal = readyCheckWindow.getOrElse(3600) // defaults to 1h

    val awaitingResponseMap = stateData.trigger.awaitingResponseMap

    if (
      nowInTicks > 0 && (nowInTicks % readyCheckVal == 0)
      && !awaitingResponseMap.contains(nowInTicks)
      && noScheduledTriggersForCurrentTick(
        stateData.trigger.triggerQueue,
        nowInTicks
      )
      && stateData.event.lastCheckWindowPassedTick < nowInTicks
    ) {
      // calculate duration
      val duration = calcDuration(stateData.time.checkStepStartTime)

      // notify listeners
      notifyListener(CheckWindowPassed(nowInTicks, duration))

      // update state data with ne time system time until next step
      stateData.copy(
        time = stateData.time.copy(checkStepStartTime = System.nanoTime),
        event = stateData.event.copy(lastCheckWindowPassedTick = nowInTicks)
      )
    } else {
      stateData
    }

  }

  /** Checks if all conditions to issue a [[Ready]] event are met and if yes do
    * so as well as returns a copy of the provided state data with updated
    * runtime and time information. If now [[Ready]] event can be issued, the
    * original state data is returned.
    *
    * @param stateData
    *   the state data that should be processed
    * @param nowInTicks
    *   the current time step
    * @return
    *   either updated state data based on the rule in the description or the
    *   original state data
    */
  private def maybeReady(
      stateData: SchedulerStateData,
      nowInTicks: Long
  ): SchedulerStateData = {
    /* ready notification can only be send if we don't have any triggers we wait for anymore in the current tick +
     * the next trigger in the queue is not the current tick */
    if (
      !stateData.trigger.awaitingResponseMap.contains(nowInTicks)
      && noScheduledTriggersForCurrentTick(
        stateData.trigger.triggerQueue,
        nowInTicks
      )
    ) {
      /* ready! - notify listener */
      notifyListener(
        Ready(
          nowInTicks,
          calcDuration(stateData.time.readyStepStartTime)
        )
      )

      /* set simulation on hold + reset readyStepStartTime*/
      stateData.copy(
        runtime = stateData.runtime.copy(scheduleStarted = false),
        time = stateData.time.copy(readyStepStartTime = System.nanoTime())
      )
    } else {
      stateData
    }
  }

  /** Determines if the simulation is finished based on the provided state data.
    * This method only checks if a) there are no missing completion messages b)
    * if the trigger queue is empty OR if the remaining the trigger queue have a
    * higher tick number than the current nowInTicks When using this method, one
    * has to check manually as well if other criteria are fulfilled as well to
    * finish the simulation.
    *
    * @param stateData
    *   the state data that should be checked
    * @return
    *   either unmodified state data or state data with runtime parameter
    *   'scheduleStarted = false'
    */
  private def maybeFinishSimulation(
      stateData: SchedulerStateData
  ): SchedulerStateData = {
    if (
      stateData.trigger.awaitingResponseMap.isEmpty && noScheduledTriggersForCurrentTick(
        stateData.trigger.triggerQueue,
        stateData.time.nowInTicks
      )
    ) {
      finishSimulation(stateData)
    } else {
      stateData
    }
  }

  /** Finish the simulation by informing all agents that the simulation is done,
    * issuing a [[Done]] event, notifying the sender that send the
    * [[StartScheduleMessage]] about the simulation status and return a state
    * data copy with disabled schedule
    *
    * @param stateData
    *   the state data that should be used for processing
    * @param errorInSim
    *   true if an error occurred in the simulation, false otherwise
    * @return
    *   a state data with scheduleStarted == false
    */
  private def finishSimulation(
      stateData: SchedulerStateData,
      errorInSim: Boolean = false
  ): SchedulerStateData = {
    val totalSimDuration: Long = stateData.time.simStartTime
      .map(startTime => calcDuration(startTime))
      .getOrElse(0)

    /* unwatch all agents that have triggered themselves as the simulation will shutdown now */
    stateData.trigger.triggerQueue.allValues.foreach(trig =>
      context.unwatch(trig.agent)
    )

    /* notify listeners */
    /*The usage of min is necessary because the scheduler overshoots the target tick by 1 at the end of the simulation*/
    notifyListener(
      Done(
        Math.min(stateData.time.nowInTicks, endTick),
        totalSimDuration,
        stateData.runtime.noOfFailedPF,
        errorInSim
      )
    )

    /* notify start sender */
    if (errorInSim) {
      stateData.runtime.initSender ! SimulationFailureMessage
    } else {
      stateData.runtime.initSender ! SimulationSuccessfulMessage
    }

    /* disable schedule */
    stateData.copy(runtime = stateData.runtime.copy(scheduleStarted = false))
  }

  protected def finishSimulationOnError(
      stateData: SchedulerStateData
  ): SchedulerStateData = {
    finishSimulation(stateData, errorInSim = true)
  }

  /** Validation to ensure that a [[StartScheduleMessage]] received by the
    * [[SimScheduler]] can be used to continue the schedule safely
    *
    * @param pauseScheduleAtTick
    *   the optional next pause tick the schedule should hold on
    * @param nowInTicks
    *   the current tick the simulation is standing at
    * @param endTick
    *   the end tick of the simulation
    */
  protected final def validStartScheduleRequest(
      pauseScheduleAtTick: Option[Long],
      nowInTicks: Long,
      endTick: Long
  ): Unit = {

    pauseScheduleAtTick.foreach(pauseScheduleAtTick => {
      if (pauseScheduleAtTick < nowInTicks) {
        throw new SchedulerException(
          s"Cannot pause schedule @ already passed tick! Current tick is: $nowInTicks, " +
            s"requested pause is $pauseScheduleAtTick!"
        )
      }

      if (pauseScheduleAtTick > endTick)
        log.warning(
          s"Requested pause for tick '$pauseScheduleAtTick' but simulation end in config is '$endTick'. " +
            s"Will simulate until tick '$endTick'!"
        )
    })

    if (nowInTicks > endTick)
      throw new SchedulerException(
        s"Requested tick $nowInTicks is after end tick $endTick. Did you remember to adapt simona.time.endDateTime in the config?"
      )
  }

  /** Either do a simulation step (move forward in tick time if possible), carry
    * out the initialization of the agents (only @ tick 0) or just return the
    * unmodified state data
    *
    * @param stateData
    *   the state data that should be used for the initialization or the
    *   simulation step
    * @return
    *   either the same or modified state data based on the executed steps
    */
  protected final def doSimStepOrInitAgents(
      stateData: SchedulerStateData
  ): SchedulerStateData = {
    if (stateData.runtime.initComplete && stateData.runtime.scheduleStarted)
      doSimStep(stateData)
    else if (!stateData.runtime.initComplete && stateData.runtime.initStarted)
      sendEligibleTrigger(stateData)
    else
      stateData
  }

  /** Main method to handle all [[CompletionMessage]] s received by the
    * [[SimScheduler]]. Based on the received completion message, the provided
    * stateData is updated. In particular the awaitingResponseMap and the
    * triggerIdToScheduledTriggerMap are updated if the provided
    * [[CompletionMessage]] is valid. For an invalid message, the data is not
    * modified and the error is logged.
    *
    * Depending on nowInTicks, the method behaves differently. If nowInTicks ==
    * 0, the method checks if the last completion message received leads to a
    * fully initialized simulation and if yes, it sets
    * [[SchedulerStateData.runtime.initComplete]] to true. Futhermore, if
    * [[this.autoStart]] is enabled, the simulation schedule is executed
    * afterwards. Otherwise, it just returned the modified state data.
    *
    * If nowInTicks > 0, a copy of the provided state data with updated trigger
    * information (as described above) is returned
    *
    * @param completionMessage
    *   the completion message that should be processed
    * @param inputStateData
    *   the current state data
    * @return
    *   state data with updated trigger and maybe updated runtime information
    */
  protected final def handleCompletionMessage(
      completionMessage: CompletionMessage,
      inputStateData: SchedulerStateData
  ): SchedulerStateData = {

    /* schedule new triggers, if any */
    val updatedStateData = completionMessage.newTriggers.map(
      _.foldLeft(inputStateData)((updatedStateData, newTrigger) =>
        scheduleTrigger(newTrigger, updatedStateData)
      )
    ) match {
      /* after scheduling the new triggers, if any, we go on with either the same or updated state data */
      case stateDataOpt =>
        val triggerId = completionMessage.triggerId
        val stateData = stateDataOpt.getOrElse(inputStateData)
        val triggerData = stateData.trigger

        val (
          updatedAwaitingResponseMap,
          updatedTriggerIdToScheduledTriggerMap
        ) = updateAwaitingResponseAndTriggerIdToScheduledTriggerMap(
          triggerId,
          triggerData.awaitingResponseMap,
          triggerData.triggerIdToScheduledTriggerMap
        ).getOrElse {
          log.error(
            s"Received bad completion notice $completionMessage from ${sender().path}"
          )
          (
            triggerData.awaitingResponseMap,
            triggerData.triggerIdToScheduledTriggerMap
          )
        }

        /* unwatch sender */
        context.unwatch(sender())

        /* if we are in the init tick, check if initialization is completed, otherwise just return state data with
         * updated trigger information */
        // initialization should always take place @ init tick -> initialization is done if we do not wait for
        // responses on init triggers @ init tick anymore
        if (stateData.time.nowInTicks != SimonaConstants.INIT_SIM_TICK) {
          /* normal behavior */
          val updatedStateData = stateData.copy(
            trigger = triggerData.copy(
              awaitingResponseMap = updatedAwaitingResponseMap,
              triggerIdToScheduledTriggerMap =
                updatedTriggerIdToScheduledTriggerMap
            )
          )

          /* if there are no triggers left to send and we are only receiving completion messages, we still might
           * pass a ready check window, this call is to ensure that for the last tick of the simulation a CheckWindowPassed()
           * is issued lastTick % schedulerReadyCheckWindow == 0 */
          maybeCheckWindowPassed(
            updatedStateData,
            schedulerReadyCheckWindow,
            stateData.time.nowInTicks - 1
          )

          maybeCheckWindowPassed(
            updatedStateData,
            schedulerReadyCheckWindow,
            stateData.time.nowInTicks
          )
        } else {
          /* we are @ the init tick (SimonaSim#initTick), if no init triggers are in the queue and in the await map
           * anymore we're done with the init process, hence we check for this case */
          val initDone =
            isInitDone(updatedAwaitingResponseMap, triggerData.triggerQueue)

          /* steps to be carried out when init is done */
          val updateTime = if (initDone) {
            val initDuration = calcDuration(
              stateData.time.initStartTime
            )

            notifyListener(InitComplete(initDuration))

            /* trigger a message to self to start schedule */
            // this is a) autostart after init and b) never pause schedule at specific tick
            if (autoStart)
              self ! StartScheduleMessage()

            /* Advance time, if init is done */
            stateData.time.copy(nowInTicks = stateData.time.nowInTicks + 1)
          } else {
            stateData.time
          }

          // set initComplete to initDone value
          stateData.copy(
            runtime = stateData.runtime.copy(initComplete = initDone),
            trigger = triggerData.copy(
              awaitingResponseMap = updatedAwaitingResponseMap,
              triggerIdToScheduledTriggerMap =
                updatedTriggerIdToScheduledTriggerMap
            ),
            time = updateTime
          )
        }
    }

    updatedStateData
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
      triggerIdToScheduledTriggerMap: mutable.Map[Long, ScheduledTrigger]
  ): Option[
    (
        CountingMap[Long],
        mutable.Map[Long, ScheduledTrigger]
    )
  ] = {
    triggerIdToScheduledTriggerMap.get(triggerId) match {
      case None =>
        None
      case Some(scheduledTrigger) =>
        val tick = scheduledTrigger.triggerWithIdMessage.trigger.tick
        if (awaitingResponseMap.contains(tick)) {

          awaitingResponseMap.subtract(tick)

          triggerIdToScheduledTriggerMap -= triggerId

          Some(
            awaitingResponseMap,
            triggerIdToScheduledTriggerMap
          )
        } else {
          None
        }
    }
  }

  /** Checks if initialization of the simulation is complete. It is complete, if
    * two conditions are fulfilled: <ol> <li>there is no trigger with tick
    * [[SimonaConstants.INIT_SIM_TICK]] inside the provided trigger queue
    * left</li> <li>there is no sent out trigger with tick
    * [[SimonaConstants.INIT_SIM_TICK]], which is not completed, yet</li> </ol>
    *
    * @param awaitingResponseMap
    *   a map containing a tick to scheduled trigger mapping
    * @param triggerQueue
    *   the trigger queue with all triggers that should be send out
    * @return
    *   true if init is done, false otherwise
    */
  private def isInitDone(
      awaitingResponseMap: CountingMap[Long],
      triggerQueue: PriorityMultiQueue[Long, ScheduledTrigger]
  ): Boolean =
    !awaitingResponseMap.contains(SimonaConstants.INIT_SIM_TICK) &&
      !triggerQueue.headKeyOption.contains(SimonaConstants.INIT_SIM_TICK)

  /** Calculate the duration between a given start time and the current system
    * time in milliseconds
    *
    * @param startTime
    *   the start time
    * @return
    *   the duration between the given start time and the current system time in
    *   milliseconds
    */
  protected def calcDuration(startTime: Long): Long = {
    (System.nanoTime - startTime) / 1000000L // in msec
  }

  /** Adds the provided trigger to the trigger queue to schedule it at the
    * requested tick
    *
    * @param triggerMessage
    *   message containing trigger to be scheduled and it's receiver
    * @param stateData
    *   the state data that should be updated
    * @return
    *   a copy of the provided state data with updated trigger data
    */
  protected final def scheduleTrigger(
      triggerMessage: ScheduleTriggerMessage,
      stateData: SchedulerStateData
  ): SchedulerStateData = {
    val updatedStateData = triggerMessage.revokeTrigger
      .map { case (trigger, actor) =>
        revokeTrigger(trigger, actor, stateData)
      }
      .getOrElse(stateData)

    scheduleTrigger(
      triggerMessage.trigger,
      triggerMessage.actorToBeScheduled,
      updatedStateData
    )
  }

  /** Adds the provided trigger to the trigger queue to schedule it at the
    * requested tick
    *
    * @param trigger
    *   the trigger that should be scheduled
    * @param actorToBeScheduled
    *   the actor that should receive the trigger
    * @param stateData
    *   the state data that should be updated
    * @return
    *   a copy of the provided state data with updated trigger data
    */
  protected final def scheduleTrigger(
      trigger: Trigger,
      actorToBeScheduled: ActorRef,
      stateData: SchedulerStateData
  ): SchedulerStateData = {

    // watch the actor to be scheduled to know when it dies
    context.watch(actorToBeScheduled)

    // if the tick of this trigger is too much in the past, we cannot schedule it
    if (stateData.time.nowInTicks > trigger.tick) {
      actorToBeScheduled ! IllegalTriggerMessage(
        s"Cannot schedule an event $trigger at tick ${trigger.tick} when 'nowInSeconds' is at ${stateData.time.nowInTicks}!",
        actorToBeScheduled
      )

      stateData

    } else {

      /* update trigger id counter & create new triggerWithIdMessage */
      val updatedTriggerIdCounter = stateData.trigger.triggerIdCounter + 1
      val triggerWithIdMessage =
        TriggerWithIdMessage(
          trigger,
          updatedTriggerIdCounter,
          actorToBeScheduled
        )

      /* update trigger queue */
      stateData.trigger.triggerQueue.add(
        trigger.tick,
        ScheduledTrigger(
          triggerWithIdMessage,
          actorToBeScheduled
        )
      )

      /* return copy of state data */
      stateData.copy(
        trigger = stateData.trigger.copy(
          triggerIdCounter = updatedTriggerIdCounter
        )
      )
    }

  }

  protected def revokeTrigger(
      trigger: Trigger,
      actor: ActorRef,
      stateData: SchedulerStateData
  ): SchedulerStateData = {

    // sanity check

    if (trigger.tick <= stateData.time.nowInTicks)
      log.warning(
        s"Trying to revoke a trigger for a tick (${trigger.tick}) earlier or equal to now (${stateData.time.nowInTicks})"
      )

    // just remove trigger from both priority and regular queue

    stateData.trigger.triggerQueue.remove(
      trigger.tick,
      scheduledTrigger =>
        scheduledTrigger.triggerWithIdMessage.trigger == trigger &&
          scheduledTrigger.agent == actor
    )

    stateData
  }

}
