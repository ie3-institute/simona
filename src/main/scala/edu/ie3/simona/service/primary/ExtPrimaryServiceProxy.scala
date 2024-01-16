package edu.ie3.simona.service.primary

import edu.ie3.datamodel.io.naming.timeseries.IndividualTimeSeriesMetaInformation
import edu.ie3.simona.api.ExtSimAdapter
import edu.ie3.simona.api.data.primarydata.ExtPrimaryData
import edu.ie3.simona.config.SimonaConfig.Simona.Input
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{PrimaryServiceRegistrationMessage, WorkerRegistrationMessage}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationFailedMessage
import edu.ie3.simona.service.primary.ExtPrimaryServiceProxy.ExtPrimaryServiceStateData
import edu.ie3.simona.service.primary.PrimaryServiceProxy.{InitPrimaryServiceProxyStateData, PrimaryServiceStateData}
import org.apache.pekko.actor.{Actor, ActorRef}

import java.time.ZonedDateTime
import java.util.UUID
import scala.util.{Failure, Success, Try}

case class ExtPrimaryServiceProxy(
                                   scheduler: ActorRef,
                                   initStateData: InitPrimaryServiceProxyStateData,
                                   private implicit val startDateTime: ZonedDateTime
                                 ) extends PrimaryServiceProxy(scheduler, initStateData, startDateTime) {

  /** Message handling, if the actor has been initialized already. This method
   * basically handles registration requests, checks, if pre-calculated,
   * primary data is available and forwards the request to worker actors. If
   * needed, new workers are spun off.
   *
   * @param stateData
   *   Representing the current state of the agent
   * @return
   *   Message handling routine
   */
  private def onMessage(stateData: ExtPrimaryServiceStateData): Receive = {
    case PrimaryServiceRegistrationMessage(modelUuid) =>
      // Ist für modelUuid eine externe primaere Datenquellen vorgesehen?
      stateData.extPrimaryDataReferenceMap.get(modelUuid) match {
        case Some(ExtPrimaryData) =>              // Ja!
          // Registriere Agent beim entsprechenden Service
          handleCoveredModel(
            modelUuid,
            stateData,
            sender()
          )
        case None =>
          log.debug(
            s"There is no external data apparent for the model with uuid '{}'.",
            modelUuid
          )
          sender() ! RegistrationFailedMessage
      }
    case x =>
      log.error(
        s"Received message '$x', but I'm only able to handle registration requests."
      )
      unhandled(x)
  }

  override def prepareStateData(
                                 primaryConfig: Input.Primary,
                                 simulationStart: ZonedDateTime
                               ): Try[PrimaryServiceStateData] = {
    val participantToExtSimMapping: Map[UUID, ExtSimAdapter] = null




    ExtPrimaryServiceStateData(
      simulationStart,
      extPrimaryServices,
      extPrimaryDataReferenceMap
    )
  }

  protected def handleCoveredModel(
                                             modelUuid: UUID,
                                             stateData: ExtPrimaryServiceStateData,
                                             requestingActor: ActorRef
                                           ): Unit = {
    // Zu welchem DataService gehoert der Agent?
    val participantToExtReferenceMap = stateData.extPrimaryDataReferenceMap
    val worker = requestingActor
    worker ! WorkerRegistrationMessage(requestingActor)
  }
}

object ExtPrimaryServiceProxy {

  final case class ExtPrimaryServiceStateData(
                                               simulationStart: ZonedDateTime,
                                               extPrimaryServices: Vector[ExtPrimaryServiceWorker],
                                               extPrimaryDataReferenceMap: Map[UUID, ExtPrimaryData]
                                             ) extends PrimaryServiceStateData(
    simulationStart, null
  )




}
