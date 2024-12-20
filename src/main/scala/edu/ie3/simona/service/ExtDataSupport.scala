/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service

import edu.ie3.simona.api.data.ontology.DataMessageFromExt
import edu.ie3.simona.ontology.messages.services.EvMessage.EvResponseMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessageUniversal
import edu.ie3.simona.ontology.messages.services.ServiceMessageUniversal.WrappedExternalMessage
import edu.ie3.simona.service.ServiceStateData.{
  ServiceBaseStateData,
  ServiceConstantStateData,
}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.typed.scaladsl.{Behaviors, StashBuffer}

trait ExtDataSupport[
    S <: ServiceBaseStateData,
    T >: ServiceMessageUniversal,
] {
  this: SimonaService[S, T] =>

  override private[service] def idleExternal(implicit
      stateData: S,
      constantData: ServiceConstantStateData,
      buffer: StashBuffer[T],
  ): Behavior[T] = Behaviors.receive {
    case (_, WrappedExternalMessage(extMsg)) =>
      val updatedStateData = handleDataMessage(extMsg)(stateData)

      buffer.unstashAll(idle(updatedStateData, constantData, buffer))

    case (_, extResponseMsg: EvResponseMessage) =>
      val updatedStateData =
        handleDataResponseMessage(extResponseMsg)(stateData)

      buffer.unstashAll(idle(updatedStateData, constantData, buffer))

    case (ctx, unsupported) =>
      ctx.log.warn(s"Received unsupported message: $unsupported!")
      buffer.stash(unsupported)
      buffer.unstashAll(idleInternal)
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
      extResponseMsg: EvResponseMessage
  )(implicit serviceStateData: S): S
}
