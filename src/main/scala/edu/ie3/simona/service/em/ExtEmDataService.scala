/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.api.data.connection.ExtEmDataConnection
import edu.ie3.simona.api.data.connection.ExtEmDataConnection.EmMode
import edu.ie3.simona.api.ontology.DataMessageFromExt
import edu.ie3.simona.api.ontology.em.*
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.exceptions.{InitializationException, ServiceException}
import edu.ie3.simona.ontology.messages.ServiceMessage.*
import edu.ie3.simona.ontology.messages.flex.FlexType
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceBaseStateData,
}
import edu.ie3.simona.service.{DataTimeType, ExtDataSupport, SimonaService}
import edu.ie3.simona.util.SimonaConstants.{
  FIRST_TICK_IN_SIMULATION,
  INIT_SIM_TICK,
}
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.slf4j.{Logger, LoggerFactory}

import java.time.ZonedDateTime
import java.util.UUID
import scala.util.{Failure, Success, Try}

object ExtEmDataService extends SimonaService with ExtDataSupport {

  private val log: Logger = LoggerFactory.getLogger(ExtEmDataService.getClass)

  override type S = ExtEmDataStateData

  /** Method to create an adapter for responses for the em service.
    * @param emService
    *   The actor reference for the em service.
    * @param receiver
    *   Of the message.
    * @param ctx
    *   The actor context to use.
    * @return
    *   An adapter for to use.
    */
  def emServiceResponseAdapter(
      emService: ActorRef[ServiceResponseMessage],
      receiver: UUID | ActorRef[FlexResponse],
  )(using ctx: ActorContext[EmAgent.Message]): ActorRef[FlexResponse] = {

    val request = Behaviors.receiveMessagePartial[FlexResponse] { msg =>
      emService ! EmFlexMessage(
        msg,
        receiver,
      )

      Behaviors.same
    }

    ctx.spawn(request, "response-adapter")
  }

  /** Method to create an adapter for requests for the em service.
    * @param emService
    *   The actor reference for the em service.
    * @param receiver
    *   Of the message.
    * @param ctx
    *   The actor context to use.
    * @return
    *   An adapter for to use.
    */
  def emServiceRequestAdapter(
      emService: ActorRef[ServiceResponseMessage],
      receiver: ActorRef[EmAgent.Message],
  )(using ctx: ActorContext[EmAgent.Message]): ActorRef[FlexRequest] = {
    val response = Behaviors.receiveMessagePartial[FlexRequest] { msg =>
      emService ! EmFlexMessage(
        msg,
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
  )(using
      ctx: ActorContext[Message]
  ): Unit = serviceResponse match {
    case EmFlexMessage(
          scheduleFlexActivation: ScheduleFlexActivation,
          receiver,
        ) =>
      log.debug(s"Received response message: $scheduleFlexActivation")

      receiver match {
        case _: UUID =>
          log.debug(s"Unlocking msg: $scheduleFlexActivation")
          scheduleFlexActivation.scheduleKey.foreach(_.unlock())

        case ref: ActorRef[EmAgent.Message] =>
          log.debug(s"Forwarding the message to: $ref")
          ref ! scheduleFlexActivation
      }
  }

  override def init(
      initServiceData: InitializeServiceStateData
  )(using log: Logger): Try[(ExtEmDataStateData, Option[Long])] =
    initServiceData match {
      case InitExtEmData(extEmDataConnection, startTime) =>
        val serviceCore = extEmDataConnection.mode match {
          case EmMode.BASE             => EmServiceBaseCore()
          case EmMode.EM_COMMUNICATION => EmCommunicationCore()
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
      ctx: ActorContext[Message],
  ): Try[ExtEmDataStateData] =
    registrationMessage match {
      case emServiceRegistration: EmServiceRegistration =>
        val updatedCore =
          serviceStateData.serviceCore.handleRegistration(emServiceRegistration)

        if emServiceRegistration.parentEm.isEmpty then {
          emServiceRegistration.requestingActor ! FlexInit(
            FlexType.PowerLimit,
            DataTimeType.Current,
          )
        }

        Success(serviceStateData.copy(serviceCore = updatedCore))
      case invalidMessage =>
        Failure(
          InvalidRegistrationRequestException(
            s"An external em service is not able to handle registration request '$invalidMessage'."
          )
        )
    }

  override protected def announceInformation(tick: Long)(using
      serviceStateData: ExtEmDataStateData,
      ctx: ActorContext[Message],
  ): (ExtEmDataStateData, Option[Long]) = {
    given Logger = ctx.log

    val extMsg = serviceStateData.extEmDataMessage.getOrElse(
      throw ServiceException(
        "ExtEmDataService was triggered without ExtEmDataMessage available"
      )
    )

    val nonCompleted =
      tick != serviceStateData.tick && serviceStateData.serviceCore.completions.nonComplete

    serviceStateData.serviceCore match {
      case _ if nonCompleted =>
        // we request a new activation for the same tick
        (serviceStateData, Some(tick))

      case core =>
        log.warn(
          s"Tick ($tick): ServiceCore -> ${core.getClass}, msg -> ${serviceStateData.extEmDataMessage}"
        )

        val (updatedCore, msgToExt) = core.handleExtMessage(tick, extMsg)

        msgToExt.foreach(
          serviceStateData.extEmDataConnection.queueExtResponseMsg
        )

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
  )(using
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
      extResponseMsg: ServiceResponseMessage,
      ctx: ActorContext[Message],
  )(using
      serviceStateData: ExtEmDataStateData
  ): ExtEmDataStateData = {
    val tick = serviceStateData.tick

    val (updatedCore, extMsg) =
      serviceStateData.serviceCore.handleDataResponseMessage(
        tick,
        extResponseMsg,
      )(using serviceStateData.startTime, ctx.log)

    if tick >= FIRST_TICK_IN_SIMULATION then {
      extMsg.foreach(serviceStateData.extEmDataConnection.queueExtResponseMsg)
    }

    serviceStateData.copy(serviceCore = updatedCore)
  }
}
