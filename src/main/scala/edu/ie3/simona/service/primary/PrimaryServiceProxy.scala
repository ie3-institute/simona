package edu.ie3.simona.service.primary

import edu.ie3.datamodel.io.naming.timeseries.IndividualTimeSeriesMetaInformation
import edu.ie3.datamodel.io.source.TimeSeriesMappingSource
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.logging.SimonaActorLogging
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationFailedMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{PrimaryServiceRegistrationMessage, WorkerRegistrationMessage}
import edu.ie3.simona.service.ServiceStateData
import edu.ie3.simona.service.ServiceStateData.InitializeServiceStateData
import edu.ie3.simona.service.primary.IntPrimaryServiceProxy.SourceRef
import edu.ie3.simona.service.primary.PrimaryServiceProxy.{InitPrimaryServiceProxyStateData, PrimaryServiceStateData}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.simona.config.SimonaConfig.Simona.Input.{Primary => PrimaryConfig}
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.actor.{Actor, ActorRef, PoisonPill}

import java.time.ZonedDateTime
import java.util.UUID
import scala.util.{Failure, Success, Try}

abstract class PrimaryServiceProxy(
                                    scheduler: ActorRef,
                                    initStateData: InitPrimaryServiceProxyStateData,
                                    private implicit val startDateTime: ZonedDateTime
                                  ) extends Actor
  with SimonaActorLogging {


  /** Start receiving without knowing specifics about myself
   *
   * @return
   *   How receiving should be handled
   */
  override def receive: Receive = uninitialized

  /** Handle all messages, when the actor isn't initialized, yet.
   *
   * @return
   *   How receiving should be handled with gained insight of myself
   */
  private def uninitialized: Receive = {
    case Activation(INIT_SIM_TICK) =>
      /* The proxy is asked to initialize itself. If that happened successfully, change the logic of receiving
       * messages */
      prepareStateData(
        initStateData.primaryConfig,
        initStateData.simulationStart
      ) match {
        case Success(stateData) =>
          scheduler ! Completion(self.toTyped)
          context become onMessage(stateData)
        case Failure(exception) =>
          log.error(
            exception,
            s"Unable to initialize the $actorName. Shut it down."
          )
          self ! PoisonPill
      }

    case x =>
      /* Unhandled message */
      log.error("Received unhandled message: {}", x)
      unhandled(x)
  }

  def onMessage(stateData: PrimaryServiceStateData): Receive

  def askForExternalSimulation(

                              )

  def prepareStateData(
                        primaryConfig: PrimaryConfig,
                        time: ZonedDateTime): Try[PrimaryServiceStateData]


  /** Handle the registration request for a covered model. First, try to get a
   * already existing worker for this time series, otherwise spin-off a new
   * one, remember it and forward the request
   *
   * @param modelUuid
   *   Unique identifier of the model
   * @param timeSeriesUuid
   *   Unique identifier of the equivalent time series
   * @param stateData
   *   Current state data of the actor
   */
  protected def handleCoveredModel(
                                    modelUuid: UUID,
                                    timeSeriesUuid: UUID,
                                    stateData: PrimaryServiceStateData,
                                    requestingActor: ActorRef
                                  ): Unit
}

object PrimaryServiceProxy {
  /** State data with needed information to initialize this primary service
   * provider proxy
   *
   * @param primaryConfig
   *   Configuration for the primary source
   * @param simulationStart
   *   Wall clock time of the first instant in simulation
   */
  final case class InitPrimaryServiceProxyStateData(
                                                     primaryConfig: PrimaryConfig,
                                                     simulationStart: ZonedDateTime
                                                   ) extends InitializeServiceStateData

  abstract class PrimaryServiceStateData(
                                            simulationStart: ZonedDateTime,
                                            primaryConfig: PrimaryConfig
                                          ) extends ServiceStateData

}
