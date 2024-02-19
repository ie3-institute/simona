package edu.ie3.simona.service.primary

import edu.ie3.datamodel.models.value.Value
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.RichValue
import edu.ie3.simona.api.data.ontology.DataMessageFromExt
import edu.ie3.simona.api.data.primarydata.ExtPrimaryData
import edu.ie3.simona.api.data.primarydata.ontology.{PrimaryDataMessageFromExt, ProvidePrimaryData}
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.exceptions.{InitializationException, ServiceException}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{ExtPrimaryDataServiceRegistrationMessage, PrimaryServiceRegistrationMessage, WorkerRegistrationMessage}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationSuccessfulMessage
import edu.ie3.simona.ontology.messages.services.{DataMessage, ServiceMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.ServiceStateData.{InitializeServiceStateData, ServiceBaseStateData}
import edu.ie3.simona.service.primary.ExtPrimaryDataService.{ExtPrimaryDataStateData, InitExtPrimaryData}
import edu.ie3.simona.service.primary.PrimaryServiceWorker.ProvidePrimaryDataMessage
import edu.ie3.simona.service.{ExtDataSupport, ServiceStateData, SimonaService}
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.actor.{ActorContext, ActorRef, Props}

import java.util.UUID
import scala.jdk.CollectionConverters.MapHasAsScala
import scala.util.{Failure, Success, Try}

object ExtPrimaryDataService {

  def props(scheduler: ActorRef): Props =
    Props(
      new ExtPrimaryDataService(scheduler: ActorRef)
    )

  final case class ExtPrimaryDataStateData(
                                            extPrimaryData: ExtPrimaryData,
                                            uuidToActorRef: Map[UUID, ActorRef] = Map.empty[UUID, ActorRef],     // subscribers in SIMONA
                                            extPrimaryDataMessage: Option[PrimaryDataMessageFromExt] = None
                                          ) extends ServiceBaseStateData

  case class InitExtPrimaryData(
                                       extPrimaryData: ExtPrimaryData
                                     ) extends InitializeServiceStateData

}
final case class ExtPrimaryDataService(
                                        override val scheduler: ActorRef
                                      )
  extends SimonaService[ExtPrimaryDataStateData](scheduler)
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
        None
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
                                                  )(implicit serviceStateData: ExtPrimaryDataStateData):
  Try[ExtPrimaryDataStateData] = {
    println("Habe erhalten 0")
    registrationMessage match {
    case ExtPrimaryDataServiceRegistrationMessage(modelUuid, requestingActor) =>
      println("Habe erhalten")
      Success(handleRegistrationRequest(requestingActor, modelUuid))
    case invalidMessage =>
      Failure(
        InvalidRegistrationRequestException(
          s"A primary service provider is not able to handle registration request '$invalidMessage'."
        )
      )
    }
  }



  private def handleRegistrationRequest(
                                         agentToBeRegistered: ActorRef,
                                         agentUUID: UUID
                                       )(implicit serviceStateData: ExtPrimaryDataStateData):
  ExtPrimaryDataStateData = {
    serviceStateData.uuidToActorRef.get(agentUUID) match {
      case None =>
        // Actor is not registered yet
        agentToBeRegistered ! RegistrationSuccessfulMessage(None)
        serviceStateData.copy(
          uuidToActorRef =
            serviceStateData.uuidToActorRef + (agentUUID -> agentToBeRegistered)
        )
      case Some(_) =>
        // actor is already registered, do nothing
        log.warning(
          "Sending actor {} is already registered",
          agentToBeRegistered
        )
        serviceStateData
    }
  }

  /** Send out the information to all registered recipients
   *
   * @param tick
   * current tick data should be announced for
   * @param serviceStateData
   * the current state data of this service
   * @return
   * the service stata data that should be used in the next state (normally
   * with updated values) together with the completion message that is send
   * in response to the trigger that was sent to start this announcement
   */
  override protected def announceInformation(
                                              tick: Long
                                            )(
                                              implicit serviceStateData: ExtPrimaryDataStateData,
                                              ctx: ActorContext
                                            ): (ExtPrimaryDataStateData, Option[Long]) = {
    serviceStateData.extPrimaryDataMessage.getOrElse(
      throw ServiceException(
        "ExtPrimaryDataService was triggered without ExtPrimaryDataMessage available"
      )
    ) match {
      case providedPrimaryData: ProvidePrimaryData =>
        processDataAndAnnounce(tick, providedPrimaryData.primaryData)(serviceStateData, ctx)
    }
  }

  private def processDataAndAnnounce(
                                      tick: Long,
                                      primaryData: java.util.Map[UUID, Value]
                                    )(
                                      implicit
                                      serviceStateData: ExtPrimaryDataStateData,
                                      ctx: ActorContext
                                    ): (
    ExtPrimaryDataStateData,
      Option[Long]
    ) = {
    val actorToPrimaryData = primaryData.asScala.flatMap {
      case (agent, primaryDataPerAgent) =>
        serviceStateData.uuidToActorRef
          .get(agent)
          .map((_, primaryDataPerAgent))
          .orElse {
            log.warning(
              "A corresponding actor ref for UUID {} could not be found",
              agent
            )
            None
          }
    }

    // Verteile Primary Data
    if (actorToPrimaryData.nonEmpty) {
      val keys =
        ScheduleLock.multiKey(ctx, scheduler.toTyped, tick, actorToPrimaryData.size)

      actorToPrimaryData.zip(keys).foreach {
        case ((actor, primaryDataPerAgent), key) =>
          primaryDataPerAgent.toPrimaryData match {
            case Success(primaryData) =>
              actor ! ProvidePrimaryDataMessage(
                tick,
                primaryData,
                None,
                unlockKey = Some(key)
              )
            case Failure(exception) =>
              /* Processing of data failed */
              log.warning(
                "Unable to convert received value to primary data. Skipped that data." +
                  "\nException: {}",
                exception
              )
          }
      }
    }

    ( // Message leeren
      serviceStateData.copy(extPrimaryDataMessage = None),
      None
    )

  }

  override protected def handleDataMessage(
                                            extMsg: DataMessageFromExt
                                          )(implicit serviceStateData: ExtPrimaryDataStateData):
  ExtPrimaryDataStateData = {
    extMsg match {
      case extPrimaryDataMessage: PrimaryDataMessageFromExt =>
        serviceStateData.copy(
          extPrimaryDataMessage = Some(extPrimaryDataMessage)
        )
    }
  }

  override protected def handleDataResponseMessage(
                                                    extResponseMsg: DataMessage
                                                  )(
                                                    implicit serviceStateData: ExtPrimaryDataStateData):
  ExtPrimaryDataStateData = {
    // not implemented
    null
  }
}

