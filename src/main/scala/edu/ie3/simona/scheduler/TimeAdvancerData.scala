/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor.typed.ActorRef
import edu.ie3.simona.ontology.messages.SchedulerMessage

/** This data container stores objects that are not supposed to change for a
  * [[TimeAdvancer]] during simulation
  * @param schedulee
  *   scheduler or other actor whose time advancement is controlled
  * @param endTick
  *   the last tick of the simulation
  */
case class TimeAdvancerData(
    schedulee: ActorRef[SchedulerMessage],
    endTick: Long
)
