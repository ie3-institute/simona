/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor.{Actor, ActorRef, Props, Terminated}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.RuntimeEvent.{Error, Initializing, Simulating}
import edu.ie3.simona.event.notifier.Notifier
import edu.ie3.simona.ontology.messages.SchedulerMessage._
import edu.ie3.simona.scheduler.SimSchedulerStateData.SchedulerStateData

object SimScheduler {

  def props(
      simonaTimeConfig: SimonaConfig.Simona.Time,
      listener: Iterable[ActorRef],
      stopOnFailedPowerFlow: Boolean,
      autoStart: Boolean =
        true // note: this can become simona config dependant in the future
  ): Props =
    Props(
      new SimScheduler(
        simonaTimeConfig,
        listener,
        stopOnFailedPowerFlow,
        autoStart
      )
    )
}

/** Scheduler for the discrete events of the simulation
  *
  * @param simonaTimeConfig
  *   time configuration parameters
  * @param listener
  *   listener for scheduler [[edu.ie3.simona.event.RuntimeEvent]] s
  * @param stopOnFailedPowerFlow
  *   true, if the scheduler should end the simulation if a power flow failed,
  *   false if we just skip a failed power flow and go on with the simulation
  * @param autoStart
  *   if autostart is set to true, the simulation starts right after finishing
  *   the initialization process
  */
class SimScheduler(
    val simonaTimeConfig: SimonaConfig.Simona.Time,
    override val listener: Iterable[ActorRef],
    val stopOnFailedPowerFlow: Boolean,
    val autoStart: Boolean =
      true // note: this can become simona config dependant in the future
) extends Actor
    with Notifier
    with SchedulerHelper
    with SimSchedulerStateData {

  override def receive: Receive = schedulerReceive(SchedulerStateData())

  def schedulerReceive(stateData: SchedulerStateData): Receive = {

    /* start the initialization process based on all up to this call scheduled initialization triggers  */
    case InitSimMessage(sender) =>
      /* initialize agents */
      // notify listeners
      notifyListener(Initializing)

      // initializing process
      val initStartTime = System.nanoTime
      sendEligibleTrigger(stateData)

      context become schedulerReceive(
        stateData.copy(
          runtime = stateData.runtime
            .copy(initStarted = true, initSender = sender),
          time = stateData.time.copy(
            initStartTime = initStartTime
          )
        )
      )

    /* start the schedule with optional pause schedule @ provided tick */
    case msg @ StartScheduleMessage(pauseScheduleAtTick) =>
      /* check if the request is valid */
      validStartScheduleRequest(
        pauseScheduleAtTick,
        stateData.time.nowInTicks,
        endTick
      )

      if (
        stateData.runtime.initComplete && !stateData.runtime.scheduleStarted
      ) {
        /* do simulation steps */
        val currentTime = System.nanoTime

        /* notify listener that we are running */
        notifyListener(
          Simulating(
            stateData.time.nowInTicks,
            pauseScheduleAtTick
              .map(pauseScheduleAtTick =>
                if (pauseScheduleAtTick > endTick) endTick
                else pauseScheduleAtTick
              )
              .getOrElse(endTick)
          )
        )

        // if autostart is false, then it might be possible that the init sender is not equal to the start sender
        // and reporting the end state of the simulation would not be possible
        val updatedSender =
          if (!autoStart) sender() else stateData.runtime.initSender

        val updatedStateData = doSimStep(
          stateData.copy(
            runtime = stateData.runtime
              .copy(scheduleStarted = true, initSender = updatedSender),
            time = stateData.time
              .copy(
                pauseScheduleAtTick = pauseScheduleAtTick,
                simStartTime =
                  stateData.time.simStartTime.orElse(Some(currentTime)),
                checkStepStartTime = currentTime,
                readyStepStartTime = currentTime
              )
          )
        )

        context become schedulerReceive(updatedStateData)
      } else if (!stateData.runtime.initComplete) {
        // init is not complete yet, do nothing
        log.error(
          "Cannot start schedule before initialization took place! Please send an initialization start message to the scheduler first!"
        )
      } else {
        // schedule is already running, do nothing
        log.error(s"Schedule already started! Discarding $msg!")
      }

    /* schedule new triggers */
    case triggerToSchedule: ScheduleTriggerMessage =>
      val updatedStateData = doSimStepOrInitAgents(
        scheduleTrigger(triggerToSchedule, stateData)
      )
      context become schedulerReceive(updatedStateData)

    /* process completion messages */
    case completionMessage: CompletionMessage =>
      val updatedStateData = doSimStepOrInitAgents(
        handleCompletionMessage(completionMessage, stateData)
      )

      context become schedulerReceive(updatedStateData)

    case PowerFlowFailedMessage =>
      /* config dependant we either go into onErrorReceive and terminate when we have
       * all completion messages received or we just go on with the normal schedule*/
      val updatedStateData = stateData.copy(
        runtime = stateData.runtime
          .copy(noOfFailedPF = stateData.runtime.noOfFailedPF + 1)
      )
      if (stopOnFailedPowerFlow) {
        /* go to onError receive state */
        context become schedulerReceiveOnError(updatedStateData)
      } else {
        context become schedulerReceive(updatedStateData)
      }

    /* received whenever a watched agent dies */
    case Terminated(_) =>
      // inform start sender about the error & stop scheduler gracefully
      notifyListener(
        Error(s"Received termination message from ${sender().path}")
      )
      stateData.runtime.initSender ! SimulationFailureMessage
      context.stop(self)

    /* all unhandled messages */
    case unhandledMessage =>
      log.error(s"Received unhandled message: $unhandledMessage")

  }

  /* receive method that is used as state where the scheduler received a PowerFlowFailedMessage and should stop on
  a failed power flow. here we only wait for all missing completion messages and then terminating the simulation*/
  def schedulerReceiveOnError(stateData: SchedulerStateData): Receive = {
    /* process completion messages, terminate simulation afterwards  */
    case completionMessage: CompletionMessage =>
      val updatedStateData =
        handleCompletionMessage(completionMessage, stateData)

      if (updatedStateData.trigger.awaitingResponseMap.isEmpty)
        finishSimulationOnError(updatedStateData)

      context become schedulerReceiveOnError(updatedStateData)

    case PowerFlowFailedMessage =>
      /* config dependant we either go into onErrorReceive and terminate when we have
       * all completion messages received or we just go on with the normal schedule*/
      val updatedStateData = stateData.copy(
        runtime = stateData.runtime
          .copy(noOfFailedPF = stateData.runtime.noOfFailedPF + 1)
      )
      context become schedulerReceiveOnError(updatedStateData)

    /* all unhandled messages */
    case unhandledMessage =>
      log.error(
        s"Received unhandled message when terminating due to error: $unhandledMessage"
      )

  }

}
