/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor.{Actor, ActorRef}
import edu.ie3.simona.ontology.messages.SchedulerMessage.TriggerWithIdMessage
import edu.ie3.simona.util.SimonaConstants
import edu.ie3.util.scala.collection.mutable.{CountingMap, PriorityMultiQueue}

import scala.collection.mutable

/** Trait containing the state data of the [[SimScheduler]]
  */
private[scheduler] trait SimSchedulerStateData {
  this: SimScheduler =>
}

object SimSchedulerStateData {

  /** Class holding all different kinds of state data a [[SimScheduler]] needs
    *
    * @param runtime
    *   state data about the current runtime
    * @param trigger
    *   state data about trigger
    * @param time
    *   state data about ticks and simulation time information
    */
  private[scheduler] final case class SchedulerStateData(
      runtime: RuntimeData = RuntimeData(),
      trigger: TriggerData = TriggerData(),
      time: TimeData = TimeData(),
      event: EventData = EventData()
  )

  private[scheduler] final case class EventData(
      lastCheckWindowPassedTick: Long = 0
  )

  /** Status information that are needed by a [[SimScheduler]] instance to
    * determine specific behavior
    *
    * @param initComplete
    *   true if the initialization process has been finished, false otherwise
    * @param initStarted
    *   true if the initialization process has started or has been finished (=
    *   started and finished afterwards) already, false otherwise
    * @param scheduleStarted
    *   true if the schedule has been started, e.g. moving on in time, false
    *   otherwise (incl. false if the scheduled has been paused)
    * @param initSender
    *   sender of the init message, only altered if autostart == false as then
    *   initSender != startSender is possible
    */
  private[scheduler] final case class RuntimeData(
      initComplete: Boolean = false,
      initStarted: Boolean = false,
      scheduleStarted: Boolean = false,
      initSender: ActorRef = Actor.noSender,
      noOfFailedPF: Int = 0
  )

  /** Holds information about [[edu.ie3.simona.ontology.trigger.Trigger]] that
    * has been scheduled, trigger to be scheduled as well as trigger that are
    * not completed yet
    *
    * @param triggerIdCounter
    *   no of triggers that has been scheduled for now
    * @param triggerQueue
    *   holds trigger that needs to be scheduled in ascending tick order
    * @param triggerIdToScheduledTriggerMap
    *   the triggerId mapped on its trigger for fast access
    * @param awaitingResponseMap
    *   maps a specific tick to all triggers that are not completed yet
    */
  private[scheduler] final case class TriggerData(
      triggerIdCounter: Int = 0,
      triggerQueue: PriorityMultiQueue[Long, ScheduledTrigger] =
        PriorityMultiQueue.empty[Long, ScheduledTrigger],
      triggerIdToScheduledTriggerMap: mutable.Map[Long, ScheduledTrigger] =
        mutable.Map.empty[Long, ScheduledTrigger],
      awaitingResponseMap: CountingMap[Long] = CountingMap.empty[Long]
  )

  /** Time data information
    *
    * @param nowInTicks
    *   the current tick of the simulation (by default starting with init tick)
    * @param initStartTime
    *   the real time when the initialization process has started
    * @param simStartTime
    *   the real time information when the simulation has started
    * @param checkStepStartTime
    *   the real time when the check window has passed the last time
    * @param readyStepStartTime
    *   the real time the latest ready event has been issued
    * @param pauseScheduleAtTick
    *   the next tick the simulation should pause
    */
  private[scheduler] final case class TimeData(
      nowInTicks: Long = SimonaConstants.INIT_SIM_TICK,
      initStartTime: Long = 0L,
      simStartTime: Option[Long] = None,
      checkStepStartTime: Long = 0L,
      readyStepStartTime: Long = 0L,
      pauseScheduleAtTick: Option[Long] = None
  )

  /** Wrapper class for trigger, that are already scheduled for execution @
    * [[TriggerWithIdMessage.trigger.tick]] in the
    * [[edu.ie3.simona.scheduler.SimScheduler]]
    *
    * @param triggerWithIdMessage
    *   the trigger that has to be scheduled
    * @param agent
    *   the agent that wants to be scheduled
    */
  private[scheduler] final case class ScheduledTrigger(
      triggerWithIdMessage: TriggerWithIdMessage,
      agent: ActorRef
  )
}
