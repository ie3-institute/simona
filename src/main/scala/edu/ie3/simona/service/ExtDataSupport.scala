/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service

import edu.ie3.simona.api.data.ontology.DataMessageFromExt
import edu.ie3.simona.ontology.messages.services.DataMessage
import edu.ie3.simona.ontology.messages.services.EvMessage.EvResponseMessage
import edu.ie3.simona.service.ServiceStateData.ServiceBaseStateData

trait ExtDataSupport[
    S <: ServiceBaseStateData
] {
  this: SimonaService[S] =>

  override def idleExternal(implicit stateData: S): Receive = {
    case extMsg: DataMessageFromExt =>
      val updatedStateData = handleDataMessage(extMsg)(stateData)
      context become idle(updatedStateData)

    case extResponseMsg: EvResponseMessage =>
      val updatedStateData =
        handleDataResponseMessage(extResponseMsg)(stateData)
      context become idle(updatedStateData)
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
  )(implicit serviceStateData: S): S

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
      extResponseMsg: DataMessage
  )(implicit serviceStateData: S): S
}
