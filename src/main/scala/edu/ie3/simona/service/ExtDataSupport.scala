/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service

import edu.ie3.simona.api.data.ontology.DataMessageFromExt
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  RegisterForEvDataMessage,
  ScheduleServiceActivation,
  ServiceResponseMessage,
  WrappedExternalMessage,
}
import edu.ie3.simona.service.ServiceStateData.ServiceConstantStateData
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.typed.scaladsl.{Behaviors, StashBuffer}

import scala.util.{Failure, Success}

/** Trait that enables handling of external data.
  * @tparam T
  *   the type of messages this service accepts
  */
trait ExtDataSupport[
    T >: ServiceMessage
] {
  this: TypedSimonaService[T] =>

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

  override private[service] def idleExternal(implicit
      stateData: S,
      constantData: ServiceConstantStateData,
      buffer: StashBuffer[T],
  ): Behavior[T] = Behaviors.receive {
    case (_, WrappedExternalMessage(extMsg)) =>
      val updatedStateData = handleDataMessage(extMsg)(stateData)

      buffer.unstashAll(idle(updatedStateData, constantData, buffer))

    case (_, extResponseMsg: ServiceResponseMessage) =>
      val updatedStateData =
        handleDataResponseMessage(extResponseMsg)(stateData)

      buffer.unstashAll(idle(updatedStateData, constantData, buffer))

    case (ctx, msg: RegisterForEvDataMessage) =>
      handleRegistrationRequest(msg)(stateData, ctx) match {
        case Success(updatedStateData) =>
          buffer.unstashAll(idle(updatedStateData, constantData, buffer))
        case Failure(exception) =>
          ctx.log.error(
            s"Failed to handle registration request: ${exception.getMessage}"
          )
          buffer.unstashAll(idle(stateData, constantData, buffer))
      }

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
      extResponseMsg: ServiceResponseMessage
  )(implicit serviceStateData: S): S
}
