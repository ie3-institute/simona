/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import edu.ie3.simona.agent.participant.em.EmSchedulerStateData.TriggerData
import edu.ie3.simona.scheduler.SimSchedulerStateData.ScheduledTrigger
import edu.ie3.simona.util.SimonaConstants
import edu.ie3.util.scala.collection.mutable.{CountingMap, PriorityMultiQueue}

import scala.collection.mutable

/** Class holding scheduling state data a [[EmAgent]] needs
  *
  * @param trigger
  *   state data about trigger
  * @param mainTrigger
  *   tick -> trigger id received from main scheduler
  * @param nowInTicks
  *   the current tick of the simulation
  */
private[em] final case class EmSchedulerStateData(
    trigger: TriggerData = TriggerData(),
    nowInTicks: Long = SimonaConstants.INIT_SIM_TICK,
    mainTrigger: Map[Long, Option[Long]] = Map.empty[Long, Option[Long]]
)

object EmSchedulerStateData {

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
  private[em] final case class TriggerData(
      triggerIdCounter: Int = 0,
      triggerQueue: PriorityMultiQueue[Long, ScheduledTrigger] =
        PriorityMultiQueue.empty[Long, ScheduledTrigger],
      triggerIdToScheduledTriggerMap: mutable.Map[Long, ScheduledTrigger] =
        mutable.Map
          .empty[Long, ScheduledTrigger],
      awaitingResponseMap: CountingMap[Long] = CountingMap.empty[Long]
  )
}
