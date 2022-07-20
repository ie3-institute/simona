package edu.ie3.simona.service.dcopf

import akka.actor.{ActorRef, Props}
import edu.ie3.datamodel.models.value.{PValue, Value}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.RichValue
import edu.ie3.simona.api.data.dcopf.ontology._
import edu.ie3.simona.api.data.ontology.ExtDataMessage
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.exceptions.{InitializationException, ServiceException}
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.services.OpfMessage._
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ExtOpfRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationSuccessfulMessage
import edu.ie3.simona.service.ServiceStateData.{InitializeServiceStateData, ServiceActivationBaseStateData}
import edu.ie3.simona.service.dcopf.ExtOpfDataService.{ExtOpfStateData, InitExtOpfData}
import edu.ie3.simona.service.primary.PrimaryServiceWorker.ProvidePrimaryDataMessage
import edu.ie3.simona.service.{ExtDataSupport, ServiceStateData, SimonaService}
import edu.ie3.util.scala.collection.immutable.SortedDistinctSeq

import java.io.{BufferedReader, FileNotFoundException, FileReader, IOException}
import java.util
import java.util.UUID
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

object ExtOpfDataService {

  def props[V <: Value](
                         scheduler: ActorRef,
                       ): Props =
    Props(new ExtOpfDataService(scheduler))

  final case class InitExtOpfData(
                                 extOpfData: ExtOpfData,
                                 primaryServiceProxy: ActorRef
                                 ) extends InitializeServiceStateData {
  }

  /**
   * Class carrying the state of a fully initialized [[ExtOpfDataService]]
   *
   * @param extOpfData
   * @param uuidToActorRef
   * @param extOpfMessage
   * @param setpoints
   * @param activePowerResponses
   */
  final case class ExtOpfStateData(
                                    extOpfData: ExtOpfData,
                                    uuidToActorRef: Map[UUID, ActorRef] = Map.empty[UUID, ActorRef],
                                    extOpfMessage: Option[ExtOpfMessage] = None,

                                    override val maybeNextActivationTick: Option[Long] = None,
                                    override val activationTicks: SortedDistinctSeq[Long] = SortedDistinctSeq.empty
                                  ) extends ServiceActivationBaseStateData
}

class ExtOpfDataService(
                         override val scheduler: ActorRef
                                   )
  extends SimonaService[ExtOpfStateData](scheduler)
  with ExtDataSupport[ExtOpfStateData] {

  /**Initialize the actor with the given information. Try to figure out the
   * initialized state data and the next activation ticks, that will then be
   * sent to the scheduler
   *
   * @param initServiceData
   *   the data that should be used for initialization
   *  @return
   *   the state data of this service and optional triggers that should be
   *   included in the completion message
   */
    override def init(
                     initServiceData: ServiceStateData.InitializeServiceStateData
                   ): Try[
    (
        ExtOpfStateData,
        Option[Seq[SchedulerMessage.ScheduleTriggerMessage]]
    )
  ] = initServiceData match {
    case InitExtOpfData(extOpfData, primaryServiceProxy) =>
      val generators = getGeneratorsUuid()
      registerAsPrimaryServiceWorker(primaryServiceProxy, generators)
      val extOpfStateData = ExtOpfStateData(
        extOpfData
      )
      Success(extOpfStateData, None)

    case invalidData =>
      Failure(new InitializationException(s"Provided init data '${invalidData.getClass.getSimpleName}' for External OPF Data Service is invalid!"))

  }

  private def getGeneratorsUuid(): List[UUID] ={
    val directoryPath = "input/samples/vn_simona/fullGrid/fixed_feed_in_input.csv"

    val generators = csvreader(directoryPath, 0).asScala.map(UUID.fromString).toList

    generators
  }

  override protected def handleRegistrationRequest(
                                                    registrationMessage: ServiceMessage.ServiceRegistrationMessage
                                                  )(implicit
                                                    extOpfStateData: ExtOpfStateData
                                                  ): Try[ExtOpfStateData] = registrationMessage match {
    case ServiceMessage.WorkerRegistrationMessage(requestingActor, requestingUUID) =>
      requestingActor ! RegistrationSuccessfulMessage(
        extOpfStateData.maybeNextActivationTick
      )
      Success(mapActorRefToUuid(requestingActor, requestingUUID, extOpfStateData))
    case unsupported =>
      Failure(
        InvalidRegistrationRequestException(
          s"A primary service provider is not able to handle registration request '$unsupported'."
        )
      )
  }

  private def mapActorRefToUuid(actorRef: ActorRef, uuid: UUID, extOpfStateData: ExtOpfStateData): ExtOpfStateData = {
    val map = extOpfStateData.uuidToActorRef + (uuid -> actorRef)
    extOpfStateData.copy(
      uuidToActorRef = map
    )
  }

  def csvreader(path: String, index: Int): util.ArrayList[String] = {
    var line = ""
    val output = new util.ArrayList[String]
    try {
      val br = new BufferedReader(new FileReader(path))
      while ( {
        (line = br.readLine) != null
      }) {
        val values = line.split(";")
        output.add(values(index))
      }
    } catch {
      case e: FileNotFoundException =>
        e.printStackTrace()
      case e: IOException =>
        e.printStackTrace()
    }
    output
  }

  private def registerAsPrimaryServiceWorker(primaryServiceProxy: ActorRef, generators: List[UUID]): Unit = {
    primaryServiceProxy ! ExtOpfRegistrationMessage(generators)
  }

  /**Send out information to all registered recipients DEPENDING ON THE MAPPED SETPOINTS???
   *
   * @param tick
   *   current tick data should be announced for
   * @param extOpfStateData
   *   the current state data of this service
   *  @return
   *   the service stata data that should be used in the next state (normally
   *   with updated values) together with the completion message that is send
   *   in response to the trigger that was sent to start this announcement
   */
  override protected def announceInformation(
      tick: Long
  )(implicit extOpfStateData: ExtOpfStateData
  ): ExtOpfStateData
     = {
    extOpfStateData.extOpfMessage.getOrElse(
      throw ServiceException(
        "ExtOpfDataActor was triggered without available ExtOpfMessage"
      )
    ) match {
      case setpointsMessage: SetpointsMessage =>
        setpointsMessage.getSetpoints.asScala.map{
          case (generator, setpoint) =>
            val generatorRef = extOpfStateData.uuidToActorRef.get(generator).getOrElse(
              throw ServiceException(
              "ExtOpfDataActor was triggered without generators' ActorRef")
            )
            processData(tick, generatorRef, setpoint)
          case _ =>
            Failure(new IllegalArgumentException(
              s"Cannot convert SetpointsMessage."
            ))
            updateStateDataAndBuildTriggerMessages(extOpfStateData)
        }
        val (maybeNextTick, remainderActivationTicks) =
          extOpfStateData.activationTicks.pop
        val updatedStateData =
          extOpfStateData.copy(
            maybeNextActivationTick = maybeNextTick,
            activationTicks = remainderActivationTicks
            // muss alte Nachricht noch gelöscht werden?
            // muss nächster Tick (+900L) angegeben werden?
          )
        updatedStateData
    }
  }

  def processData(
                   tick: Long,
                   generator: ActorRef,
                   setpoint: PValue
    )
     = setpoint.toPrimaryData match {
    case Success(setpointPrimary) =>
        announcePrimaryData(tick, generator, setpointPrimary)
    case Failure(exception) =>
      /* Processing of data failed */
      log.warning(
        "Unable to convert received value to primary data. Skipped that data." +
          "\nException: {}",
        exception
      )
  }

  def announcePrimaryData(
                           tick: Long,
                           generator: ActorRef,
                           primaryData: PrimaryData
  )
  = {
    val provisionMessage =
      ProvidePrimaryDataMessage(tick, primaryData, Some(tick+900L))
    generator ! provisionMessage
  }

  def updateStateDataAndBuildTriggerMessages(
                                              extOpfStateData: ExtOpfStateData
  ):ExtOpfStateData={
    val (maybeNextActivationTick, remainderActivationTicks) =
      extOpfStateData.activationTicks.pop
    val triggerMessages =
      ServiceActivationBaseStateData.tickToScheduleTriggerMessages(
        maybeNextActivationTick,
        self
      )
    extOpfStateData.copy(
      maybeNextActivationTick = maybeNextActivationTick,
      activationTicks = remainderActivationTicks
    )
  }



  /** This method is called from State idleExternal for receiving
   * the external message and adding it to the ExtOpfStateData.
   * It does not trigger the processing of the data.
   *
   * @param extMsg
   *   the external incoming message
   * @param serviceStateData
   *   the current state data of this service
   *  @return
   *   the updated state data
   */
  override protected def handleDataMessage(
                                            extMsg: ExtDataMessage
                                          )(implicit serviceStateData: ExtOpfStateData
                                          ): ExtOpfStateData =
    extMsg match {
      case extOpfMessage: ExtOpfMessage =>
        serviceStateData.copy(
          extOpfMessage = Some(extOpfMessage)
        )
    }

}
