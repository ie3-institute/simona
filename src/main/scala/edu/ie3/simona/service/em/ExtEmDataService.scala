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
import edu.ie3.simona.ontology.messages.ServiceMessage
import edu.ie3.simona.ontology.messages.ServiceMessage.*
import edu.ie3.simona.ontology.messages.flex.FlexType.PowerLimit
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
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

object ExtEmDataService extends SimonaService with ExtDataSupport {

  private val log: Logger = LoggerFactory.getLogger(ExtEmDataService.getClass)

  override type S = ExtEmDataStateData

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
        case uuid: UUID =>
          log.debug(s"Unlocking msg: $scheduleFlexActivation")
          scheduleFlexActivation.scheduleKey.foreach(_.unlock())

        case ref: ActorRef[EmAgent.Message] =>
          log.debug(s"Forwarding the message to: $ref")
          ref ! scheduleFlexActivation

        case _ =>
          // this should not happen
          log.warn(s"No receiver found for msg: $serviceResponse")
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
          EmCommunicationCore2()
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

        if (emServiceRegistration.parentEm.isEmpty) {
          emServiceRegistration.requestingActor ! FlexActivation(
            INIT_SIM_TICK,
            PowerLimit,
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
        serviceStateData.serviceCore.handleExtMessage(tick, extMsg)(using
          ctx.log
        )

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
      extResponseMsg: ServiceResponseMessage
  )(using
      serviceStateData: ExtEmDataStateData
  ): ExtEmDataStateData = {

    val (updatedCore, extMsg) =
      serviceStateData.serviceCore.handleDataResponseMessage(
        serviceStateData.tick,
        extResponseMsg,
      )(using serviceStateData.startTime, log)

    extMsg.foreach(serviceStateData.extEmDataConnection.queueExtResponseMsg)

    serviceStateData.copy(serviceCore = updatedCore)
  }
}
