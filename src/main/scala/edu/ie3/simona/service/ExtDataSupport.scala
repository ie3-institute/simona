/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service

import edu.ie3.simona.api.data.ontology.DataMessageFromExt
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  ScheduleServiceActivation,
  ServiceResponseMessage,
  WrappedExternalMessage,
}
import edu.ie3.simona.service.ServiceStateData.ServiceConstantStateData
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

/** Trait that enables handling of external data.
  *
  * @tparam T
  *   the type of messages this service accepts.
  */
trait ExtDataSupport[T >: ServiceMessage] {
  this: SimonaService[T] =>

  /** Creates an adapter, that enables a service with [[ExtDataSupport]] to
    * receive a [[DataMessageFromExt]] by wrapping it in an
    * [[WrappedExternalMessage]].
    * @param service
    *   For which an adapter should be created
    * @return
    *   The behavior of the adapter.
    */
  def adapter(service: ActorRef[T]): Behavior[DataMessageFromExt] =
    Behaviors.receiveMessagePartial[DataMessageFromExt] {
      case scheduleServiceActivation: ScheduleServiceActivation =>
        // TODO: Refactor this with scala3
        service ! scheduleServiceActivation
        Behaviors.same

      case extMsg =>
        service ! WrappedExternalMessage(extMsg)
        Behaviors.same
    }

  override protected def idleExternal(implicit
      stateData: S,
      constantData: ServiceConstantStateData,
  ): PartialFunction[(ActorContext[T], T), Behavior[T]] = {
    case (_, WrappedExternalMessage(extMsg)) =>
      val updatedStateData = handleDataMessage(extMsg)(stateData)

      idle(updatedStateData, constantData)

    case (_, extResponseMsg: ServiceResponseMessage) =>
      val updatedStateData =
        handleDataResponseMessage(extResponseMsg)(stateData)

      idle(updatedStateData, constantData)
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
      extResponseMsg: ServiceResponseMessage
  )(implicit serviceStateData: S): S
}
