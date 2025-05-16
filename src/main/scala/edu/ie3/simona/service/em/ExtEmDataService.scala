/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.api.data.em.ontology._
import edu.ie3.simona.api.data.em.{EmMode, ExtEmDataConnection}
import edu.ie3.simona.api.data.ontology.DataMessageFromExt
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.exceptions.{
  CriticalFailureException,
  InitializationException,
  ServiceException,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage._
import edu.ie3.simona.ontology.messages.services.EmMessage
import edu.ie3.simona.ontology.messages.services.EmMessage.{
  WrappedFlexRequest,
  WrappedFlexResponse,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  RegisterForEmDataService,
  ServiceRegistrationMessage,
  ServiceResponseMessage,
}
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceBaseStateData,
}
import edu.ie3.simona.service.{ExtDataSupport, SimonaService}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.slf4j.{Logger, LoggerFactory}

import java.time.ZonedDateTime
import java.util.UUID
import scala.util.{Failure, Success, Try}

object ExtEmDataService
    extends SimonaService[EmMessage]
    with ExtDataSupport[EmMessage] {

  private val log: Logger = LoggerFactory.getLogger(ExtEmDataService.getClass)

  override type S = ExtEmDataStateData

  def emServiceResponseAdapter(
      emService: ActorRef[EmMessage],
      receiver: Option[ActorRef[FlexResponse]],
      self: UUID,
  )(implicit ctx: ActorContext[EmAgent.Request]): ActorRef[FlexResponse] = {

    val request = Behaviors.receiveMessagePartial[FlexResponse] {
      case response: FlexResponse =>
        emService ! WrappedFlexResponse(
          response,
          receiver.map(Right(_)).getOrElse(Left(self)),
        )

        Behaviors.same
    }

    ctx.spawn(request, "response-adapter")
  }

  def emServiceRequestAdapter(
      emService: ActorRef[EmMessage],
      receiver: ActorRef[FlexRequest],
  )(implicit ctx: ActorContext[EmAgent.Request]): ActorRef[FlexRequest] = {
    val response = Behaviors.receiveMessagePartial[FlexRequest] {
      case request: FlexRequest =>
        emService ! WrappedFlexRequest(
          request,
          receiver,
        )

        Behaviors.same
    }

    ctx.spawn(response, "request-adapter")
  }

  final case class ExtEmDataStateData(
      extEmDataConnection: ExtEmDataConnection,
      startTime: ZonedDateTime,
      serviceCore: EmServiceCore,
      tick: Long = INIT_SIM_TICK,
      extEmDataMessage: Option[EmDataMessageFromExt] = None,
  ) extends ServiceBaseStateData

  case class InitExtEmData(
      extEmData: ExtEmDataConnection,
      startTime: ZonedDateTime,
  ) extends InitializeServiceStateData

  override protected def handleServiceResponse(
      serviceResponse: ServiceResponseMessage
  )(implicit
      ctx: ActorContext[EmMessage]
  ): Unit = serviceResponse match {
    case WrappedFlexResponse(
          scheduleFlexActivation: ScheduleFlexActivation,
          receiver,
        ) =>
      log.debug(s"Received response message: $scheduleFlexActivation")

      receiver match {
        case Right(ref) =>
          log.debug(s"Forwarding the message to: $ref")
          ref ! scheduleFlexActivation
        case Left(_) =>
          log.debug(s"Unlocking msg: $scheduleFlexActivation")

          scheduleFlexActivation.scheduleKey.foreach(_.unlock())
      }
  }

  override def init(
      initServiceData: InitializeServiceStateData
  ): Try[(ExtEmDataStateData, Option[Long])] = initServiceData match {
    case InitExtEmData(extEmDataConnection, startTime) =>
      val serviceCore = extEmDataConnection.mode match {
        case EmMode.BASE =>
          EmServiceBaseCore.empty
        case EmMode.EM_COMMUNICATION =>
          EmCommunicationCore.empty
      }

      val emDataInitializedStateData =
        ExtEmDataStateData(extEmDataConnection, startTime, serviceCore)
      Success(
        emDataInitializedStateData,
        None,
      )

    case invalidData =>
      Failure(
        new InitializationException(
          s"Provided init data '${invalidData.getClass.getSimpleName}' for ExtEmDataService are invalid!"
        )
      )
  }

  override protected def handleRegistrationRequest(
      registrationMessage: ServiceRegistrationMessage
  )(implicit
      serviceStateData: ExtEmDataStateData,
      ctx: ActorContext[EmMessage],
  ): Try[ExtEmDataStateData] =
    registrationMessage match {
      case registrationMsg: RegisterForEmDataService =>
        val updatedCore =
          serviceStateData.serviceCore.handleRegistration(registrationMsg)

        if (registrationMsg.parentEm.isEmpty) {
          registrationMsg.flexAdapter ! FlexActivation(INIT_SIM_TICK)
        }

        Success(serviceStateData.copy(serviceCore = updatedCore))
      case invalidMessage =>
        Failure(
          InvalidRegistrationRequestException(
            s"An external em service is not able to handle registration request '$invalidMessage'."
          )
        )
    }

  override protected def announceInformation(tick: Long)(implicit
      serviceStateData: ExtEmDataStateData,
      ctx: ActorContext[EmMessage],
  ): (ExtEmDataStateData, Option[Long]) = {
    val stateTick = serviceStateData.tick

    if (tick != stateTick) {
      // we received an activation for the next tick

      // check the last finished tick of the core
      val lastFinishedTick = serviceStateData.serviceCore.lastFinishedTick

      val updatedStateData = if (lastFinishedTick == stateTick) {
        // we finished the last tick and update the core with the requested tick
        serviceStateData.copy(tick = tick)

      } else {
        // we are still waiting for data for the state data tick
        serviceStateData
      }

      // we request a new activation for the same tick
      (updatedStateData, Some(tick))

    } else {
      val extMsg = serviceStateData.extEmDataMessage.getOrElse(
        throw ServiceException(
          "ExtEmDataService was triggered without ExtEmDataMessage available"
        )
      )

      val (updatedCore, msgToExt) =
        serviceStateData.serviceCore.handleExtMessage(tick, extMsg)(ctx.log)

      msgToExt.foreach(serviceStateData.extEmDataConnection.queueExtResponseMsg)

      (
        serviceStateData.copy(
          tick = tick,
          serviceCore = updatedCore,
          extEmDataMessage = None,
        ),
        None,
      )
    }
  }

  override protected def handleDataMessage(
      extMsg: DataMessageFromExt
  )(implicit
      serviceStateData: ExtEmDataStateData
  ): ExtEmDataStateData = {
    extMsg match {
      case extEmDataMessage: EmDataMessageFromExt =>
        serviceStateData.copy(
          extEmDataMessage = Some(extEmDataMessage)
        )
    }
  }

  override protected def handleDataResponseMessage(
      extResponseMsg: ServiceResponseMessage
  )(implicit
      serviceStateData: ExtEmDataStateData
  ): ExtEmDataStateData = {

    val (updatedCore, extMsg) =
      serviceStateData.serviceCore.handleDataResponseMessage(
        serviceStateData.tick,
        extResponseMsg,
      )(serviceStateData.startTime, log)

    extMsg.foreach(serviceStateData.extEmDataConnection.queueExtResponseMsg)

    serviceStateData.copy(serviceCore = updatedCore)
  }
}
