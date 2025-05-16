/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.ParticipantAgent.{
  DataProvision,
  PrimaryRegistrationSuccessfulMessage,
}
import edu.ie3.simona.api.data.ontology.DataMessageFromExt
import edu.ie3.simona.api.data.primarydata.ExtPrimaryDataConnection
import edu.ie3.simona.api.data.primarydata.ontology.{
  PrimaryDataMessageFromExt,
  ProvidePrimaryData,
}
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.exceptions.{InitializationException, ServiceException}
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  PrimaryServiceRegistrationMessage,
  ServiceResponseMessage,
}
import edu.ie3.simona.service.Data.PrimaryData
import edu.ie3.simona.service.Data.PrimaryData.RichValue
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceBaseStateData,
}
import edu.ie3.simona.service.{ExtDataSupport, ServiceStateData, SimonaService}
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext

import java.util.UUID
import scala.jdk.CollectionConverters.MapHasAsScala
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try}

object ExtPrimaryDataService
    extends SimonaService[ServiceMessage]
    with ExtDataSupport[ServiceMessage] {

  override type S = ExtPrimaryDataStateData

  final case class ExtPrimaryDataStateData(
      extPrimaryData: ExtPrimaryDataConnection,
      subscribers: List[UUID] = List.empty,
      uuidToActorRef: Map[UUID, ActorRef[ParticipantAgent.Request]] =
        Map.empty, // subscribers in SIMONA
      extPrimaryDataMessage: Option[PrimaryDataMessageFromExt] = None,
      maybeNextTick: Option[Long] = None,
  ) extends ServiceBaseStateData

  case class InitExtPrimaryData(
      extPrimaryData: ExtPrimaryDataConnection
  ) extends InitializeServiceStateData

  override def init(
      initServiceData: ServiceStateData.InitializeServiceStateData
  ): Try[(ExtPrimaryDataStateData, Option[Long])] = initServiceData match {
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
      registrationMessage: ServiceMessage.ServiceRegistrationMessage
  )(implicit
      serviceStateData: ExtPrimaryDataStateData,
      ctx: ActorContext[ServiceMessage],
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

  private def handleRegistrationRequest(
      agentToBeRegistered: ActorRef[ParticipantAgent.Request],
      agentUUID: UUID,
  )(implicit
      serviceStateData: ExtPrimaryDataStateData,
      ctx: ActorContext[ServiceMessage],
  ): ExtPrimaryDataStateData = {
    serviceStateData.uuidToActorRef.get(agentUUID) match {
      case None =>
        // checks if a value class was specified for the agent
        val valueClass = serviceStateData.extPrimaryData
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

        serviceStateData.copy(
          subscribers = serviceStateData.subscribers :+ agentUUID,
          uuidToActorRef =
            serviceStateData.uuidToActorRef + (agentUUID -> agentToBeRegistered),
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

  /** Send out the information to all registered recipients
    *
    * @param tick
    *   current tick data should be announced for
    * @param serviceStateData
    *   the current state data of this service
    * @return
    *   the service stata data that should be used in the next state (normally
    *   with updated values) together with the completion message that is send
    *   in response to the trigger that was sent to start this announcement
    */
  override protected def announceInformation(
      tick: Long
  )(implicit
      serviceStateData: ExtPrimaryDataStateData,
      ctx: ActorContext[ServiceMessage],
  ): (ExtPrimaryDataStateData, Option[Long]) = { // We got activated for this tick, so we expect incoming primary data
    serviceStateData.extPrimaryDataMessage.getOrElse(
      throw ServiceException(
        "ExtPrimaryDataService was triggered without ExtPrimaryDataMessage available"
      )
    ) match {
      case providedPrimaryData: ProvidePrimaryData =>
        processDataAndAnnounce(tick, providedPrimaryData)(
          serviceStateData,
          ctx,
        )
    }
  }

  private def processDataAndAnnounce(
      tick: Long,
      primaryDataMessage: ProvidePrimaryData,
  )(implicit
      serviceStateData: ExtPrimaryDataStateData,
      ctx: ActorContext[ServiceMessage],
  ): (
      ExtPrimaryDataStateData,
      Option[Long],
  ) = {
    ctx.log.debug(
      s"Got activation to distribute primaryData = $primaryDataMessage"
    )
    val actorToPrimaryData = primaryDataMessage.primaryData.asScala.flatMap {
      case (agent, primaryDataPerAgent) =>
        serviceStateData.uuidToActorRef
          .get(agent)
          .map((_, primaryDataPerAgent))
          .orElse {
            ctx.log.warn(
              "A corresponding actor ref for UUID {} could not be found",
              agent,
            )
            None
          }
    }

    val maybeNextTick = primaryDataMessage.maybeNextTick.toScala.map(Long2long)

    // Distribute Primary Data
    if (actorToPrimaryData.nonEmpty) {
      actorToPrimaryData.foreach { case (actor, value) =>
        value.toPrimaryData match {
          case Success(primaryData) =>
            actor ! DataProvision(
              tick,
              ctx.self,
              primaryData,
              maybeNextTick,
            )
          case Failure(exception) =>
            /* Processing of data failed */
            ctx.log.warn(
              "Unable to convert received value to primary data. Skipped that data." +
                "\nException: {}",
              exception,
            )
        }
      }
    }

    (
      serviceStateData.copy(extPrimaryDataMessage = None),
      None,
    )
  }

  override protected def handleDataMessage(
      extMsg: DataMessageFromExt
  )(implicit
      serviceStateData: ExtPrimaryDataStateData
  ): ExtPrimaryDataStateData = {
    extMsg match {
      case extPrimaryDataMessage: PrimaryDataMessageFromExt =>
        serviceStateData.copy(
          extPrimaryDataMessage = Some(extPrimaryDataMessage)
        )
    }
  }

  override protected def handleDataResponseMessage(
      extResponseMsg: ServiceResponseMessage
  )(implicit
      serviceStateData: ExtPrimaryDataStateData
  ): ExtPrimaryDataStateData = serviceStateData
}
