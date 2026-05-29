/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.api.data.connection.ExtPrimaryDataConnection
import edu.ie3.simona.api.ontology.DataMessageFromExt
import edu.ie3.simona.api.ontology.primary.{
  PrimaryDataMessageFromExt,
  ProvidePrimaryData,
}
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.exceptions.{InitializationException, ServiceException}
import edu.ie3.simona.ontology.messages.ServiceMessage
import edu.ie3.simona.ontology.messages.ServiceMessage.*
import edu.ie3.simona.service.Data.PrimaryData
import edu.ie3.simona.service.Data.PrimaryData.RichValue
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceBaseStateData,
}
import edu.ie3.simona.service.{ExtDataSupport, ServiceStateData, SimonaService}
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.slf4j.Logger

import java.util.UUID
import scala.jdk.CollectionConverters.MapHasAsScala
import scala.jdk.OptionConverters.{RichOptional, RichOptionalLong}
import scala.util.{Failure, Success, Try}

object ExtPrimaryServiceWorker extends SimonaService with ExtDataSupport {

  override type S = ExtPrimaryDataStateData

  /** State data of the external worker.
    * @param extPrimaryDataConnection
    *   The connection to the external simulation that will provide primary
    *   data.
    * @param uuidToActorRef
    *   Map: uuid to participant agent ref. This is used to forward the data.
    * @param extPrimaryDataMessage
    *   An option for the last received external data message.
    */
  final case class ExtPrimaryDataStateData(
      extPrimaryDataConnection: ExtPrimaryDataConnection,
      uuidToActorRef: Map[UUID, ActorRef[ServiceMessage.Response]] =
        Map.empty, // subscribers in SIMONA
      extPrimaryDataMessage: Option[PrimaryDataMessageFromExt] = None,
  ) extends ServiceBaseStateData

  case class InitExtPrimaryData(
      extPrimaryData: ExtPrimaryDataConnection
  ) extends InitializeServiceStateData

  override def init(
      initServiceData: ServiceStateData.InitializeServiceStateData
  )(using log: Logger): Try[(ExtPrimaryDataStateData, Option[Long])] =
    initServiceData match {
      case InitExtPrimaryData(extPrimaryData) =>
        val primaryDataInitializedStateData = ExtPrimaryDataStateData(
          extPrimaryData
        )
        Success(
          primaryDataInitializedStateData,
          None,
        )

      case invalidData =>
        Failure(
          new InitializationException(
            s"Provided init data '${invalidData.getClass.getSimpleName}' for ExtPrimaryService are invalid!"
          )
        )
    }

  override protected def handleRegistrationRequest(
      registrationMessage: ServiceRegistrationMessage
  )(using
      serviceStateData: ExtPrimaryDataStateData,
      ctx: ActorContext[Message],
  ): Try[ExtPrimaryDataStateData] = registrationMessage match {
    case PrimaryServiceRegistrationMessage(
          requestingActor,
          modelUuid,
        ) =>
      Success(handleRegistrationRequest(requestingActor, modelUuid))
    case invalidMessage =>
      Failure(
        InvalidRegistrationRequestException(
          s"A primary service provider is not able to handle registration request '$invalidMessage'."
        )
      )
  }

  /** Handles the registration of a participant agent.
    * @param agentToBeRegistered
    *   Actor ref of the participant that should be registered.
    * @param agentUUID
    *   UUID of the participant.
    * @param serviceStateData
    *   The current state data of the worker.
    * @param ctx
    *   Actor context of the worker.
    * @return
    *   The updated state data.
    */
  private def handleRegistrationRequest(
      agentToBeRegistered: ActorRef[ServiceMessage.Response],
      agentUUID: UUID,
  )(using
      serviceStateData: ExtPrimaryDataStateData,
      ctx: ActorContext[Message],
  ): ExtPrimaryDataStateData = {
    serviceStateData.uuidToActorRef.get(agentUUID) match {
      case None =>
        // checks if a value class was specified for the agent
        val valueClass = serviceStateData.extPrimaryDataConnection
          .getValueClass(agentUUID)
          .toScala
          .getOrElse(
            throw InvalidRegistrationRequestException(
              s"A primary service provider is not able to handle registration request, because there was no value class specified for the agent with id: '$agentUUID'."
            )
          )

        agentToBeRegistered ! PrimaryRegistrationSuccessfulMessage(
          ctx.self,
          0L,
          PrimaryData.getPrimaryDataExtra(valueClass),
        )
        ctx.log.info(s"Successful registration for $agentUUID")

        serviceStateData.copy(uuidToActorRef =
          serviceStateData.uuidToActorRef + (agentUUID -> agentToBeRegistered)
        )

      case Some(_) =>
        // actor is already registered, do nothing
        ctx.log.warn(
          "Sending actor {} is already registered",
          agentToBeRegistered,
        )
        serviceStateData
    }
  }

  override protected def announceInformation(
      tick: Long
  )(using
      serviceStateData: ExtPrimaryDataStateData,
      ctx: ActorContext[Message],
  ): (ExtPrimaryDataStateData, Option[Long]) = { // We got activated for this tick, so we expect incoming primary data
    serviceStateData.extPrimaryDataMessage.getOrElse(
      throw ServiceException(
        "ExtPrimaryDataService was triggered without ExtPrimaryDataMessage available"
      )
    ) match {
      case providedPrimaryData: ProvidePrimaryData =>
        processDataAndAnnounce(tick, providedPrimaryData)
    }
  }

  /** Method for processing the received data message and sending it to the
    * corresponding participant agents.
    * @param tick
    *   The current tick of the simulation.
    * @param primaryDataMessage
    *   The external data message.
    * @param serviceStateData
    *   The current state data of the worker.
    * @param ctx
    *   Actor context of the worker.
    * @return
    *   The updated state data and an option for the next activation tick.
    */
  private def processDataAndAnnounce(
      tick: Long,
      primaryDataMessage: ProvidePrimaryData,
  )(using
      serviceStateData: ExtPrimaryDataStateData,
      ctx: ActorContext[Message],
  ): (
      ExtPrimaryDataStateData,
      Option[Long],
  ) = {
    ctx.log.debug(
      s"Got activation to distribute primaryData = $primaryDataMessage"
    )

    val uuidToAgent = serviceStateData.uuidToActorRef
    val maybeNextTick = primaryDataMessage.maybeNextTick.toScala

    primaryDataMessage.primaryData.asScala.foreach { case (agentUuid, data) =>
      data.toPrimaryData match {
        case Success(primaryData) =>
          uuidToAgent.get(agentUuid) match {
            case Some(agentRef) =>
              agentRef ! DataProvision(
                tick,
                ctx.self,
                primaryData,
                maybeNextTick,
              )

            case None =>
              ctx.log.warn(
                "A corresponding actor ref for UUID {} could not be found",
                agentUuid,
              )
          }

        case Failure(exception) =>
          /* Processing of data failed */
          ctx.log.warn(
            "Unable to convert received value to primary data. Skipped that data." +
              "\nException: {}",
            exception,
          )
      }
    }

    (
      serviceStateData.copy(extPrimaryDataMessage = None),
      None,
    )
  }

  override protected def handleDataMessage(
      extMsg: DataMessageFromExt
  )(using
      serviceStateData: ExtPrimaryDataStateData
  ): ExtPrimaryDataStateData = {
    extMsg match {
      case extPrimaryDataMessage: PrimaryDataMessageFromExt =>
        serviceStateData.copy(
          extPrimaryDataMessage = Some(extPrimaryDataMessage)
        )
    }
  }

  // unused by this service, because no responses to the external simulation are possible
  override protected def handleDataResponseMessage(
      extResponseMsg: ServiceResponseMessage,
      ctx: ActorContext[Message],
  )(implicit
      serviceStateData: ExtPrimaryDataStateData
  ): ExtPrimaryDataStateData = serviceStateData
}
