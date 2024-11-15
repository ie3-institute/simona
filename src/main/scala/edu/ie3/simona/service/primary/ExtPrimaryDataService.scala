/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import edu.ie3.datamodel.models.value.Value
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.RichValue
import edu.ie3.simona.api.data.ontology.DataMessageFromExt
import edu.ie3.simona.api.data.primarydata.ExtPrimaryData
import edu.ie3.simona.api.data.primarydata.ontology.{
  PrimaryDataMessageFromExt,
  ProvidePrimaryData,
}
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.exceptions.{InitializationException, ServiceException}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ExtPrimaryDataServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationSuccessfulMessage
import edu.ie3.simona.ontology.messages.services.{DataMessage, ServiceMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceBaseStateData,
}
import edu.ie3.simona.service.primary.ExtPrimaryDataService.{
  ExtPrimaryDataStateData,
  InitExtPrimaryData,
}
import edu.ie3.simona.service.primary.PrimaryServiceWorker.ProvidePrimaryDataMessage
import edu.ie3.simona.service.{ExtDataSupport, ServiceStateData, SimonaService}
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.actor.{ActorContext, ActorRef, Props}

import java.util.UUID
import scala.jdk.CollectionConverters.MapHasAsScala
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try}

object ExtPrimaryDataService {

  def props(scheduler: ActorRef): Props =
    Props(
      new ExtPrimaryDataService(scheduler: ActorRef)
    )

  final case class ExtPrimaryDataStateData(
      extPrimaryData: ExtPrimaryData,
      subscribers: List[UUID] = List.empty,
      uuidToActorRef: Map[UUID, ActorRef] =
        Map.empty[UUID, ActorRef], // subscribers in SIMONA
      extPrimaryDataMessage: Option[PrimaryDataMessageFromExt] = None,
      maybeNextTick: Option[Long] = None,
  ) extends ServiceBaseStateData

  case class InitExtPrimaryData(
      extPrimaryData: ExtPrimaryData
  ) extends InitializeServiceStateData

}
final case class ExtPrimaryDataService(
    override val scheduler: ActorRef
) extends SimonaService[ExtPrimaryDataStateData](scheduler)
    with ExtDataSupport[ExtPrimaryDataStateData] {

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
      serviceStateData: ExtPrimaryDataStateData
  ): Try[ExtPrimaryDataStateData] = registrationMessage match {
    case ExtPrimaryDataServiceRegistrationMessage(
          modelUuid,
          requestingActor,
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
      agentToBeRegistered: ActorRef,
      agentUUID: UUID,
  )(implicit
      serviceStateData: ExtPrimaryDataStateData
  ): ExtPrimaryDataStateData = {
    serviceStateData.uuidToActorRef.get(agentUUID) match {
      case None =>
        // Actor is not registered yet
        agentToBeRegistered ! RegistrationSuccessfulMessage(self, Some(0L))
        log.info(s"Successful registration for $agentUUID")
        serviceStateData.copy(
          uuidToActorRef =
            serviceStateData.uuidToActorRef + (agentUUID -> agentToBeRegistered)
        )
      case Some(_) =>
        // actor is already registered, do nothing
        log.warning(
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
      ctx: ActorContext,
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
      ctx: ActorContext,
  ): (
      ExtPrimaryDataStateData,
      Option[Long],
  ) = {
    log.debug(s"Got activation to distribute primaryData = $primaryDataMessage")
    val actorToPrimaryData = primaryDataMessage.primaryData.asScala.flatMap {
      case (agent, primaryDataPerAgent) =>
        serviceStateData.uuidToActorRef
          .get(agent)
          .map((_, primaryDataPerAgent))
          .orElse {
            log.warning(
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
            actor ! ProvidePrimaryDataMessage(
              tick,
              self,
              primaryData,
              maybeNextTick,
            )
          case Failure(exception) =>
            /* Processing of data failed */
            log.warning(
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
      extResponseMsg: DataMessage
  )(implicit
      serviceStateData: ExtPrimaryDataStateData
  ): ExtPrimaryDataStateData = serviceStateData
}
