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
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.{SchedulerMessage, ServiceMessage}
import edu.ie3.simona.ontology.messages.ServiceMessage.*
import edu.ie3.simona.ontology.messages.flex.FlexType.PowerLimit
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceBaseStateData,
}
import edu.ie3.simona.service.{ExtDataSupport, SimonaService}
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
import scala.jdk.OptionConverters.RichOptional

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
      simulateFrom: Long = FIRST_TICK_IN_SIMULATION,
      simulateUntil: Long = FIRST_TICK_IN_SIMULATION,
      extEmDataMessage: Option[EmDataMessageFromExt] = None,
      scheduler: Option[ActorRef[SchedulerMessage]] = None,
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
  )(using log: Logger): Try[(ExtEmDataStateData, Option[Long])] =
    initServiceData match {
      case InitExtEmData(extEmDataConnection, startTime) =>
        val serviceCore = InternalCore(extEmDataConnection.mode)

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
        val updatedCore = serviceStateData.serviceCore.toInternal
          .handleRegistration(emServiceRegistration)

        if emServiceRegistration.parentEm.isEmpty then {
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

  override protected def finishActivation(
      stateData: ExtEmDataStateData,
      scheduler: ActorRef[SchedulerMessage],
      maybeNextTick: Option[Long],
      ctx: ActorContext[ExtEmDataService.Message],
  ): Option[ExtEmDataStateData] = {
    val tick = stateData.tick
    val nextExtTick = stateData.simulateUntil

    if nextExtTick > tick && stateData.simulateFrom < tick then {
      Some(stateData.copy(scheduler = Some(scheduler)))
    } else {
      scheduler ! Completion(ctx.self, maybeNextTick.filter(_ < nextExtTick))
      None
    }
  }

  override protected def announceInformation(tick: Long)(using
      serviceStateData: ExtEmDataStateData,
      ctx: ActorContext[Message],
  ): (ExtEmDataStateData, Option[Long]) = {
    given Logger = ctx.log
    val stateTick = serviceStateData.tick

    if tick != stateTick then {
      // we received an activation for the next tick

      // check the last finished tick of the core
      val lastFinishedTick = serviceStateData.serviceCore.lastFinishedTick

      val updatedStateData = if lastFinishedTick == stateTick then {
        // we finished the last tick and update the core with the requested tick
        serviceStateData.copy(tick = tick)

      } else {
        // we are still waiting for data for the state data tick
        serviceStateData
      }

      // we request a new activation for the same tick
      (updatedStateData, Some(tick))

    } else {
      log.warn(
        s"Tick ($tick): ServiceCore -> ${serviceStateData.serviceCore.getClass}, msg -> ${serviceStateData.extEmDataMessage}"
      )

      val ((updatedCore, msgToExt), until) = (
        serviceStateData.extEmDataMessage,
        serviceStateData.serviceCore,
      ) match {
        case (Some(simulationUntil: EmSimulationUntil), core) =>
          ((core.toInternal, None), Some(simulationUntil.tick))

        case (Some(extMsg), core: (EmCommunicationCore | EmServiceBaseCore)) =>
          (core.handleExtMessage(tick, extMsg), None)

        case (Some(extMsg), core) =>
          (core.toExternal.handleExtMessage(tick, extMsg), None)

        case (extMsg, core: InternalCore)
            if serviceStateData.simulateUntil > tick =>
          extMsg.foreach(_ =>
            log.warn(s"Received external message with internal core!")
          )

          ((core.sendActivations(tick), None), None)

        case (None, _) =>
          throw ServiceException(
            "ExtEmDataService was triggered without ExtEmDataMessage available"
          )
      }

      until match {
        case Some(nextExternalTick) =>
          log.warn(s"Simulate until tick $nextExternalTick.")

          (
            serviceStateData.copy(
              tick = tick,
              serviceCore = updatedCore,
              simulateFrom = tick,
              simulateUntil = nextExternalTick,
              extEmDataMessage = None,
            ),
            updatedCore.nextActivation.values.minOption,
          )

        case _ =>
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

    val (updatedCore, extMsg) =
      serviceStateData.serviceCore.handleDataResponseMessage(
        serviceStateData.tick,
        extResponseMsg,
      )(using serviceStateData.startTime, log)

    (serviceStateData.scheduler, serviceStateData.simulateUntil) match {
      case (Some(scheduler), nextExternalTick)
          if nextExternalTick > serviceStateData.tick =>
        ctx.log.warn(s"Still activated!")
        // service is still activated

        extMsg match {
          case Some(_: EmCompletion) =>
            // every em agent is finished for this tick
            // go to next activation
            val nextTick = updatedCore.nextActivation.values.minOption
              .filter(_ < nextExternalTick)

            log.warn(s"Next tick option: $nextTick")

            scheduler ! Completion(
              ctx.self,
              nextTick,
            )

            serviceStateData.copy(
              serviceCore = updatedCore,
              scheduler = None,
            )

          case _ =>
            // we are not finished yet
            serviceStateData.copy(serviceCore = updatedCore)
        }

      case (Some(scheduler), _) =>
        ctx.log.warn(s"Still activated!")

        // service is still activated, but every em agent is finished

        log.warn(s"Next tick option: None")

        scheduler ! Completion(
          ctx.self,
          None,
        )

        serviceStateData.copy(
          serviceCore = updatedCore,
          scheduler = None,
        )

      case _ =>
        ctx.log.warn(s"Deactivated!")
        extMsg.foreach(serviceStateData.extEmDataConnection.queueExtResponseMsg)

        serviceStateData.copy(serviceCore = updatedCore)
    }
  }
}
