/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import akka.actor.typed.ActorRef
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.ontology.messages.SchedulerMessage

case class TimeAdvancerData(
    scheduler: ActorRef[SchedulerMessage],
    eventListener: Option[ActorRef[RuntimeEvent]],
    endTick: Long
)

object TimeAdvancerData {
  case class RuntimeData(
      simStartTime: Long,
      lastStepTime: Long,
      numberOfFailedPf: Int
  )
}
