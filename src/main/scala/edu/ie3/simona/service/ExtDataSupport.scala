/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service

import edu.ie3.simona.api.data.ontology.DataMessageFromExt
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.ServiceMessage
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  ScheduleServiceActivation,
  ServiceResponseMessage,
}
import edu.ie3.simona.service.ServiceStateData.ServiceConstantStateData
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

/** Trait that enables handling of external data.
  */
trait ExtDataSupport {
  this: SimonaService =>

  override protected type ServiceMessages = ServiceMessage | Activation |
    DataMessageFromExt

  override protected def idleExternal(using
      stateData: S,
      constantData: ServiceConstantStateData,
  ): PartialFunction[(ActorContext[ServiceMessages], ServiceMessages), Behavior[
    ServiceMessages
  ]] = {
    case (_, extMsg: DataMessageFromExt) =>
      val updatedStateData = handleDataMessage(extMsg)

      idle(using updatedStateData, constantData)

    case (_, extResponseMsg: ServiceResponseMessage) =>
      val updatedStateData = handleDataResponseMessage(extResponseMsg)

      idle(using updatedStateData, constantData)
  }

  /** Handle a message from outside the simulation
    *
    * @param extMsg
    *   the external incoming message
    * @param serviceStateData
    *   the current state data of this service
    * @return
    *   the updated state data
    */
  protected def handleDataMessage(
      extMsg: DataMessageFromExt
  )(using serviceStateData: S): S

  /** Handle a message from inside SIMONA sent to external
    *
    * @param extResponseMsg
    *   the external incoming message
    * @param serviceStateData
    *   the current state data of this service
    * @return
    *   the updated state data
    */
  protected def handleDataResponseMessage(
      extResponseMsg: ServiceResponseMessage
  )(using serviceStateData: S): S
}
