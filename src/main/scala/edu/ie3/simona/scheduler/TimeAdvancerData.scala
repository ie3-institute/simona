/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor.typed.ActorRef
import edu.ie3.simona.ontology.messages.SchedulerMessage

case class TimeAdvancerData(
    schedulee: ActorRef[SchedulerMessage],
    endTick: Long
)
