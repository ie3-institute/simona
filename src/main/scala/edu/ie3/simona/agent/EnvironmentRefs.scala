/*
 * © 2021-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent

import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.ontology.messages.{SchedulerMessage, ServiceMessage}
import edu.ie3.simona.service.ServiceType
import edu.ie3.simona.service.em.ExtEmDataService
import edu.ie3.simona.service.ev.ExtEvDataService
import edu.ie3.simona.service.results.ResultServiceProxy
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
  * @param resultProxy
  *   Reference to the result service proxy.
  * @param weather
  *   Reference to the service, that provides weather information.
  * @param price
  *   Reference to the price service, if configured.
  * @param loadProfiles
  *   Reference to the service, that provides load profile information.
  * @param emDataService
  *   Reference to the energy management service, if existing.
  * @param evDataService
  *   Reference to the EV data service, if existing.
  */
final case class EnvironmentRefs(
    scheduler: ActorRef[SchedulerMessage],
    runtimeEventListener: ActorRef[RuntimeEvent],
    primaryServiceProxy: ActorRef[ServiceMessage],
    resultProxy: ActorRef[ResultServiceProxy.Message],
    weather: ActorRef[ServiceMessage],
    price: Option[ActorRef[ServiceMessage]],
    loadProfiles: ActorRef[ServiceMessage],
    emDataService: Option[ActorRef[ExtEmDataService.Message]],
    evDataService: Option[ActorRef[ExtEvDataService.Message]],
) {

  /** Returns references to services by service type.
    */
  lazy val serviceMap: Map[ServiceType, ActorRef[ServiceMessage]] =
    Seq(
      Some(ServiceType.WeatherService -> weather),
      price.map(ServiceType.PriceService -> _),
      Some(ServiceType.LoadProfileService -> loadProfiles),
      evDataService.map(ref => ServiceType.EvMovementService -> ref),
    ).flatten.toMap
}
