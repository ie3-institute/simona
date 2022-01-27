/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import akka.actor.{Actor, ActorRef, PoisonPill, Props}
import edu.ie3.datamodel.io.connectors.CsvFileConnector.CsvIndividualTimeSeriesMetaInformation
import edu.ie3.datamodel.io.csv.timeseries.ColumnScheme
import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.TimeSeriesMappingSource
import edu.ie3.datamodel.io.source.csv.CsvTimeSeriesMappingSource
import edu.ie3.datamodel.models.value.Value
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.Simona.Input.Primary.CsvParams
import edu.ie3.simona.config.SimonaConfig.Simona.Input.{
  Primary => PrimaryConfig
}
import edu.ie3.simona.exceptions.{
  InitializationException,
  InvalidConfigParameterException
}
import edu.ie3.simona.logging.SimonaActorLogging
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationFailedMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  PrimaryServiceRegistrationMessage,
  WorkerRegistrationMessage
}
import edu.ie3.simona.ontology.trigger.Trigger.InitializeServiceTrigger
import edu.ie3.simona.service.ServiceStateData
import edu.ie3.simona.service.ServiceStateData.InitializeServiceStateData
import edu.ie3.simona.service.primary.PrimaryServiceProxy.{
  InitPrimaryServiceProxyStateData,
  PrimaryServiceStateData,
  SourceRef
}
import edu.ie3.simona.service.primary.PrimaryServiceWorker.{
  CsvInitPrimaryServiceStateData,
  InitPrimaryServiceStateData
}

import java.text.SimpleDateFormat
import java.time.ZonedDateTime
import java.util.UUID
import scala.Option.when
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.util.{Failure, Success, Try}

/** This actor has information on which models can be replaced by precalculated
  * (primary) data and how to obtain those time series. It offers possibility to
  * register for a certain model. If data is available, a child actor is spun
  * of, that will do the actual provision and the requesting agent is informed
  * accordingly.
  *
  * @param scheduler
  *   Reference to the scheduler of the simulation
  * @param startDateTime
  *   Wall clock time of the first instant in simulation
  */
case class PrimaryServiceProxy(
    scheduler: ActorRef,
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
    case TriggerWithIdMessage(
          InitializeServiceTrigger(
            InitPrimaryServiceProxyStateData(
              primaryConfig,
              simulationStart
            )
          ),
          triggerId,
          _
        ) =>
      /* The proxy is asked to initialize itself. If that happened successfully, change the logic of receiving
       * messages */
      prepareStateData(primaryConfig, simulationStart) match {
        case Success(stateData) =>
          sender() ! CompletionMessage(triggerId, newTriggers = None)
          context become onMessage(stateData)
        case Failure(exception) =>
          log.error(
            s"Unable to initialize the $actorName. Shut it down.",
            exception
          )
          self ! PoisonPill
      }

    case x =>
      /* Unhandled message */
      log.error("Received unhandled message: {}", x)
      unhandled(x)
  }

  /** Prepare the needed state data by building a
    * [[edu.ie3.datamodel.io.source.TimeSeriesMappingSource]], obtain it's
    * information and compile them to state data
    *
    * @param primaryConfig
    *   Configuration for the primary source
    * @param simulationStart
    *   Wall clock time of first instant in simulation
    * @return
    *   State data, containing the known model and time series identifiers
    */
  private def prepareStateData(
      primaryConfig: PrimaryConfig,
      simulationStart: ZonedDateTime
  ): Try[PrimaryServiceStateData] =
    Seq(
      primaryConfig.sqlParams,
      primaryConfig.influxDb1xParams,
      primaryConfig.csvParams,
      primaryConfig.couchbaseParams
    ).filter(_.isDefined).flatten.headOption match {
      case Some(CsvParams(csvSep, folderPath, _)) =>
        // TODO: Configurable file naming strategy
        val mappingSource = new CsvTimeSeriesMappingSource(
          csvSep,
          folderPath,
          new FileNamingStrategy()
        )
        val modelToTimeSeries = mappingSource.getMapping.asScala.toMap
        val timeSeriesToSourceRef = modelToTimeSeries.values
          .to(LazyList)
          .distinct
          .flatMap { timeSeriesUuid =>
            mappingSource
              .getTimeSeriesMetaInformation(timeSeriesUuid)
              .toScala match {
              case Some(metaInformation) =>
                val columnScheme = metaInformation.getColumnScheme
                /* Only register those entries, that meet the supported column schemes */
                when(
                  PrimaryServiceWorker.supportedColumnSchemes
                    .contains(columnScheme)
                ) {
                  timeSeriesUuid -> SourceRef(columnScheme, None)
                }
              case None =>
                log.warning(
                  "Unable to acquire meta information for time series '{}'. Leave that out.",
                  timeSeriesUuid
                )
                None
            }
          }
          .toMap
        Success(
          PrimaryServiceStateData(
            modelToTimeSeries,
            timeSeriesToSourceRef,
            simulationStart,
            primaryConfig,
            mappingSource
          )
        )

      case Some(x) =>
        Failure(
          new IllegalArgumentException(
            s"Unsupported config for mapping source: '$x'"
          )
        )
      case None =>
        Failure(
          new IllegalArgumentException(
            "You have to provide exactly one config for the mapping source."
          )
        )
    }

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
  private def onMessage(stateData: PrimaryServiceStateData): Receive = {
    case PrimaryServiceRegistrationMessage(modelUuid) =>
      /* Try to register for this model */
      stateData.modelToTimeSeries.get(modelUuid) match {
        case Some(timeSeriesUuid) =>
          /* There is a time series apparent for this model, try to get a worker for it */
          handleCoveredModel(
            modelUuid,
            timeSeriesUuid,
            stateData,
            sender()
          )
        case None =>
          log.debug(
            s"There is no time series apparent for the model with uuid '{}'.",
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
  private[primary] def handleCoveredModel(
      modelUuid: UUID,
      timeSeriesUuid: UUID,
      stateData: PrimaryServiceStateData,
      requestingActor: ActorRef
  ): Unit = {
    val timeSeriesToSourceRef = stateData.timeSeriesToSourceRef
    timeSeriesToSourceRef.get(timeSeriesUuid) match {
      case Some(SourceRef(_, Some(worker))) =>
        /* There is yet a worker apparent. Register the requesting actor. The worker will reply to the original
         * requesting actor. */
        worker ! WorkerRegistrationMessage(requestingActor)
      case Some(SourceRef(columnScheme, None)) =>
        /* There is NO worker apparent, yet. Spin one off. */
        initializeWorker(
          columnScheme,
          timeSeriesUuid,
          stateData.simulationStart,
          stateData.primaryConfig,
          stateData.mappingSource
        ) match {
          case Success(workerRef) =>
            /* Forward the registration request. The worker will reply about successful registration or not. */
            workerRef ! WorkerRegistrationMessage(requestingActor)

            /* Register the new worker within the state data and change the context */
            context become onMessage(
              updateStateData(stateData, timeSeriesUuid, workerRef)
            )
          case Failure(exception) =>
            log.warning(
              s"A failure occurred during spin-off of a primary source for time series '$timeSeriesUuid'. " +
                s"Will inform the requesting actor, that registration is not possible.",
              exception
            )
            requestingActor ! RegistrationFailedMessage
        }

      case None =>
        log.warning(
          s"There is no source information for time series '$timeSeriesUuid' (requested for model " +
            s"'$modelUuid'), although the mapping contains information about it."
        )
        requestingActor ! RegistrationFailedMessage
    }
  }

  /** Instantiate a new [[PrimaryServiceWorker]] and send initialization
    * information
    *
    * @param columnScheme
    *   Scheme of the data to expect
    * @param primaryConfig
    *   Configuration for the primary config
    * @param mappingSource
    *   Source for time series mapping, that might deliver additional
    *   information for the source initialization
    * @return
    *   The [[ActorRef]] to the worker
    */
  private[primary] def initializeWorker(
      columnScheme: ColumnScheme,
      timeSeriesUuid: UUID,
      simulationStart: ZonedDateTime,
      primaryConfig: PrimaryConfig,
      mappingSource: TimeSeriesMappingSource
  ): Try[ActorRef] = {
    val workerRef = classToWorkerRef(
      columnScheme.getValueClass,
      timeSeriesUuid.toString,
      simulationStart
    )
    toInitData(
      primaryConfig,
      mappingSource,
      timeSeriesUuid,
      simulationStart
    ) match {
      case Success(initData) =>
        scheduler ! ScheduleTriggerMessage(
          InitializeServiceTrigger(initData),
          workerRef
        )
        Success(workerRef)
      case Failure(cause) =>
        workerRef ! PoisonPill
        Failure(
          new InitializationException(
            "Unable to build init data for worker. Kill the uninitialized worker. Goodbye my friend!",
            cause
          )
        )
    }
  }

  /** Build a primary source worker and type it to the foreseen value class to
    * come
    *
    * @param valueClass
    *   Class of the values to provide later on
    * @param timeSeriesUuid
    *   uuid of the time series the actor processes
    * @param simulationStart
    *   Wall clock time of first instant in simulation
    * @tparam V
    *   Type of the class to provide
    * @return
    *   The [[ActorRef]] to the spun off actor
    */
  private[primary] def classToWorkerRef[V <: Value](
      valueClass: Class[V],
      timeSeriesUuid: String,
      simulationStart: ZonedDateTime
  ): ActorRef = {
    import edu.ie3.simona.actor.SimonaActorNaming._
    context.system.simonaActorOf(
      PrimaryServiceWorker.props(scheduler, valueClass, simulationStart),
      timeSeriesUuid
    )
  }

  /** Building proper init data for the worker
    *
    * @param primaryConfig
    *   Configuration for primary sources
    * @param mappingSource
    *   Source to get mapping information about time series
    * @param timeSeriesUuid
    *   Unique identifier for the time series
    * @param simulationStart
    *   Wall clock time of the first instant in simulation
    * @return
    */
  private def toInitData(
      primaryConfig: PrimaryConfig,
      mappingSource: TimeSeriesMappingSource,
      timeSeriesUuid: UUID,
      simulationStart: ZonedDateTime
  ): Try[InitPrimaryServiceStateData] =
    primaryConfig match {
      case PrimaryConfig(
            None,
            Some(CsvParams(csvSep, directoryPath, timePattern)),
            None,
            None
          ) =>
        /* The mapping and actual data sources are from csv. At first, get the file name of the file to read. */
        Try(mappingSource.getTimeSeriesMetaInformation(timeSeriesUuid).get)
          .flatMap {
            /* Time series meta information could be successfully obtained */
            case csvMetaData: CsvIndividualTimeSeriesMetaInformation =>
              Success(
                CsvInitPrimaryServiceStateData(
                  timeSeriesUuid,
                  simulationStart,
                  csvSep,
                  directoryPath,
                  csvMetaData.getFullFilePath,
                  new FileNamingStrategy(),
                  timePattern
                )
              )
            case invalidMetaData =>
              Failure(
                new InitializationException(
                  s"Expected '${classOf[CsvIndividualTimeSeriesMetaInformation]}', but got '$invalidMetaData'."
                )
              )
          }
      case unsupported =>
        Failure(
          new InitializationException(
            s"Cannot build initialization data for a worker due to unsupported source config '$unsupported'."
          )
        )
    }

  /** Register the worker within the state data.
    *
    * @param stateData
    *   Current state information
    * @param timeSeriesUuid
    *   Unique identifier of the time series, the worker takes care of
    * @param workerRef
    *   [[ActorRef]] to the new worker actor
    * @return
    *   The updated state data, that holds reference to the worker
    */
  private def updateStateData(
      stateData: PrimaryServiceStateData,
      timeSeriesUuid: UUID,
      workerRef: ActorRef
  ): PrimaryServiceStateData = {
    val timeSeriesToSourceRef = stateData.timeSeriesToSourceRef
    val sourceRef = timeSeriesToSourceRef.getOrElse(
      timeSeriesUuid,
      throw new IllegalArgumentException(
        s"Cannot update entry for time series '$timeSeriesUuid', as it hasn't been part of it before."
      )
    )
    val updatedTimeSeriesToSourceRef = timeSeriesToSourceRef.updated(
      timeSeriesUuid,
      sourceRef.copy(worker = Some(workerRef))
    )
    stateData.copy(timeSeriesToSourceRef = updatedTimeSeriesToSourceRef)
  }
}

object PrimaryServiceProxy {

  def props(scheduler: ActorRef, startDateTime: ZonedDateTime): Props = Props(
    new PrimaryServiceProxy(scheduler, startDateTime)
  )

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

  /** Holding the state of an initialized proxy.
    *
    * @param modelToTimeSeries
    *   Mapping from models' to time series unique identifiers
    * @param timeSeriesToSourceRef
    *   Mapping from time series identifier to [[SourceRef]]
    * @param simulationStart
    *   Wall clock time of the first instant in simulation
    * @param primaryConfig
    *   The configuration for the sources
    * @param mappingSource
    *   The mapping source
    */
  final case class PrimaryServiceStateData(
      modelToTimeSeries: Map[UUID, UUID],
      timeSeriesToSourceRef: Map[UUID, SourceRef],
      simulationStart: ZonedDateTime,
      primaryConfig: PrimaryConfig,
      mappingSource: TimeSeriesMappingSource
  ) extends ServiceStateData

  /** Giving reference to the target time series and source worker.
    *
    * @param columnScheme
    *   Column scheme of the time series to get
    * @param worker
    *   Optional reference to a yet existing worker providing information on
    *   that time series
    */
  final case class SourceRef(
      columnScheme: ColumnScheme,
      worker: Option[ActorRef]
  )

  /** Check if the config holds correct information to instantiate a mapping
    * source
    *
    * @param primaryConfig
    *   Config entries for primary source
    */
  def checkConfig(primaryConfig: PrimaryConfig): Unit = {

    def checkTimePattern(dtfPattern: String): Unit =
      Try {
        new SimpleDateFormat(dtfPattern)
      } match {
        case Failure(exception) =>
          throw new InvalidConfigParameterException(
            s"Invalid timePattern '$dtfPattern' for a time series source. Please provide a valid pattern!" +
              s"\nException: $exception"
          )
        case Success(_) =>
        // this is fine
      }

    val supportedSources =
      Set("csv")

    val sourceConfigs = Seq(
      primaryConfig.couchbaseParams,
      primaryConfig.csvParams,
      primaryConfig.influxDb1xParams,
      primaryConfig.sqlParams
    ).filter(_.isDefined).flatten
    if (sourceConfigs.size > 1)
      throw new InvalidConfigParameterException(
        s"${sourceConfigs.size} time series source types defined. " +
          s"Please define only one type!\nAvailable types:\n\t${supportedSources.mkString("\n\t")}"
      )
    else if (sourceConfigs.isEmpty)
      throw new InvalidConfigParameterException(
        s"No time series source type defined. Please define exactly one type!" +
          s"\nAvailable types:\n\t${supportedSources.mkString("\n\t")}"
      )
    else {
      sourceConfigs.headOption match {
        case Some(csvParams: SimonaConfig.Simona.Input.Primary.CsvParams) =>
          // note: if inheritance is supported by tscfg,
          // the following method should be called for all different supported sources!
          checkTimePattern(csvParams.timePattern)
        case Some(x) =>
          throw new InvalidConfigParameterException(
            s"Invalid configuration '$x' for a time series source.\nAvailable types:\n\t${supportedSources
              .mkString("\n\t")}"
          )
        case None =>
          throw new InvalidConfigParameterException(
            s"No configuration for a time series mapping source provided.\nPlease provide one of the available sources:\n\t${supportedSources
              .mkString("\n\t")}"
          )
      }
    }
  }
}
