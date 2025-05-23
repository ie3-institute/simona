/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent

import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.services.{
  EvMessage,
  LoadProfileMessage,
  ServiceMessage,
  WeatherMessage,
}
import org.apache.pekko.actor.typed.ActorRef

/** Container class, that gather together reference to relevant entities, that
  * represent the environment in the simulation.
  *
  * @param scheduler
  *   Reference to the event handling entity.
  * @param runtimeEventListener
  *   Reference to the runtime event listener.
  * @param primaryServiceProxy
  *   Reference to the primary service proxy.
  * @param weather
  *   Reference to the service, that provides weather information.
  * @param loadProfiles
  *   Reference to the service, that provides load profile information.
  * @param evDataService
  *   Reference to the EV data service, if existing.
  */
final case class EnvironmentRefs(
    scheduler: ActorRef[SchedulerMessage],
    runtimeEventListener: ActorRef[RuntimeEvent],
    primaryServiceProxy: ActorRef[ServiceMessage],
    weather: ActorRef[WeatherMessage],
    loadProfiles: ActorRef[LoadProfileMessage],
    evDataService: Option[ActorRef[EvMessage]],
)
