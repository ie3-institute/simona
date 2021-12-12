/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent

import akka.actor.ActorRef

/** Container class, that gather together reference to relevant entities, that
  * represent the environment in the simulation
  *
  * @param scheduler
  *   Reference to the event handling entity
  * @param primaryServiceProxy
  *   Reference to the primary service proxy
  * @param weather
  *   Reference to the service, that provides weather information
  * @param evDataService
  *   Reference to the EV data service, if existing
  */
final case class EnvironmentRefs(
    scheduler: ActorRef,
    primaryServiceProxy: ActorRef,
    weather: ActorRef,
    evDataService: Option[ActorRef]
)
