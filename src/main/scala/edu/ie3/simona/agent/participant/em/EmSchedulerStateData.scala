/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import akka.actor.ActorRef
import edu.ie3.simona.agent.participant.em.EmSchedulerStateData.{
  FlexTriggerData,
  TriggerData
}
import edu.ie3.simona.ontology.trigger.Trigger
import edu.ie3.simona.scheduler.SimSchedulerStateData.ScheduledTrigger
import edu.ie3.simona.util.SimonaConstants
import edu.ie3.util.scala.collection.mutable.PriorityMultiQueue

import java.util.UUID
import scala.collection.mutable

/** Class holding scheduling state data a [[EmAgent]] needs
  *
  * @param trigger
  *   state data about trigger
  * @param createMainTrigger
  *   Function that creates a trigger for this EmAgent with given tick
  * @param mainTriggerId
  *   trigger id received from main scheduler
  * @param nowInTicks
  *   the current tick of the simulation
  */
private[em] final case class EmSchedulerStateData(
    trigger: TriggerData = TriggerData(),
    flexTrigger: FlexTriggerData,
    createMainTrigger: Long => Trigger,
    nowInTicks: Long = SimonaConstants.INIT_SIM_TICK,
    mainTriggerId: Option[Long] = None
)

object EmSchedulerStateData {

  def apply(
      triggerData: TriggerData,
      uuidToActorRef: Map[UUID, ActorRef],
      createMainTrigger: Long => Trigger
  ): EmSchedulerStateData = {
    val actorRefToUuid = uuidToActorRef.map { case (uuid, actor) =>
      actor -> uuid
    }

    new EmSchedulerStateData(
      trigger = triggerData,
      flexTrigger = FlexTriggerData(
        actorRefToUuid,
        uuidToActorRef
      ),
      createMainTrigger
    )
  }

  /** Holds information about [[edu.ie3.simona.ontology.trigger.Trigger]] that
    * has been scheduled, trigger to be scheduled as well as trigger that are
    * not completed yet
    *
    * @param triggerIdCounter
    *   no of triggers that has been scheduled for now
    * @param triggerQueue
    *   holds trigger that needs to be scheduled in ascending tick order
    * @param awaitedTriggerMap
    *   the triggerId mapped on its trigger for fast access
    */
  private[em] final case class TriggerData(
      triggerIdCounter: Int = 0,
      triggerQueue: PriorityMultiQueue[Long, ScheduledTrigger] =
        PriorityMultiQueue.empty[Long, ScheduledTrigger],
      awaitedTriggerMap: mutable.Map[Long, ScheduledTrigger] =
        mutable.Map.empty[Long, ScheduledTrigger]
  )

  private[em] final case class FlexTriggerData(
      actorRefToUuid: Map[ActorRef, UUID],
      uuidToActorRef: Map[UUID, ActorRef],
      triggerQueue: PriorityMultiQueue[Long, ScheduledFlexTrigger] =
        PriorityMultiQueue.empty,
      awaitedFlexCompletions: mutable.Set[UUID] = mutable.Set.empty,
      activateAtNextTick: mutable.Set[UUID] = mutable.Set.empty
  )

  private[em] final case class ScheduledFlexTrigger(
      trigger: Trigger,
      modelUuid: UUID
  )
}
