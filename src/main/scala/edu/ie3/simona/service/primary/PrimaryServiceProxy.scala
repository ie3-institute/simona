/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import edu.ie3.datamodel.io.connectors.SqlConnector
import edu.ie3.datamodel.io.csv.CsvIndividualTimeSeriesMetaInformation
import edu.ie3.datamodel.io.naming.timeseries.IndividualTimeSeriesMetaInformation
import edu.ie3.datamodel.io.naming.{
  DatabaseNamingStrategy,
  EntityPersistenceNamingStrategy,
  FileNamingStrategy,
}
import edu.ie3.datamodel.io.source.csv.{
  CsvTimeSeriesMappingSource,
  CsvTimeSeriesMetaInformationSource,
}
import edu.ie3.datamodel.io.source.sql.{
  SqlTimeSeriesMappingSource,
  SqlTimeSeriesMetaInformationSource,
}
import edu.ie3.datamodel.io.source.{
  TimeSeriesMappingSource,
  TimeSeriesMetaInformationSource,
}
import edu.ie3.datamodel.models.value.Value
import edu.ie3.simona.config.SimonaConfig.PrimaryDataCsvParams
import edu.ie3.simona.config.SimonaConfig.Simona.Input.Primary.SqlParams
import edu.ie3.simona.config.SimonaConfig.Simona.Input.{
  Primary => PrimaryConfig
}
import edu.ie3.simona.exceptions.{
  InitializationException,
  InvalidConfigParameterException,
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.PrimaryDataMessage
import edu.ie3.simona.ontology.messages.services.PrimaryDataMessage.{
  PrimaryServiceRegistrationMessage,
  WorkerRegistrationMessage,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessageUniversal.RegistrationResponseMessage.RegistrationFailedMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessageUniversal.{
  Create,
  WrappedActivation,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.ServiceStateData
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceConstantStateData,
}
import edu.ie3.simona.service.primary.PrimaryServiceWorker.{
  CsvInitPrimaryServiceStateData,
  InitPrimaryServiceStateData,
  SqlInitPrimaryServiceStateData,
}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.{ActorRef => ClassicRef}
import org.slf4j.Logger

import java.nio.file.Paths
import java.text.SimpleDateFormat
import java.time.ZonedDateTime
import java.util.UUID
import scala.Option.when
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOptional
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/** This actor has information on which models can be replaced by precalculated
  * (primary) data and how to obtain those time series. It offers possibility to
  * register for a certain model. If data is available, a child actor is spun
  * of, that will do the actual provision and the requesting agent is informed
  * accordingly.
  */
object PrimaryServiceProxy {

  /** State data with needed information to initialize this primary service
    * provider proxy
    *
    * @param primaryConfig
    *   Configuration for the primary source
    * @param simulationStart
    *   Simulation time of the first instant in simulation
    */
  final case class InitPrimaryServiceProxyStateData(
      primaryConfig: PrimaryConfig,
      simulationStart: ZonedDateTime,
  ) extends InitializeServiceStateData

  /** Holding the state of an initialized proxy.
    *
    * @param modelToTimeSeries
    *   Mapping from models' to time series unique identifiers
    * @param timeSeriesToSourceRef
    *   Mapping from time series identifier to [[SourceRef]]
    * @param simulationStart
    *   Simulation time of the first instant in simulation
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
      mappingSource: TimeSeriesMappingSource,
  ) extends ServiceStateData

  /** Giving reference to the target time series and source worker.
    *
    * @param metaInformation
    *   Meta information (including column scheme) of the time series
    * @param worker
    *   Optional reference to an already existing worker providing information
    *   on that time series
    */
  final case class SourceRef(
      metaInformation: IndividualTimeSeriesMetaInformation,
      worker: Option[ActorRef[PrimaryDataMessage]],
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
      Set("csv", "sql")

    val sourceConfigs = Seq(
      primaryConfig.couchbaseParams,
      primaryConfig.csvParams,
      primaryConfig.influxDb1xParams,
      primaryConfig.sqlParams,
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
        case Some(csvParams: PrimaryDataCsvParams) =>
          // note: if inheritance is supported by tscfg,
          // the following method should be called for all different supported sources!
          checkTimePattern(csvParams.timePattern)
        case Some(sqlParams: SqlParams) =>
          checkTimePattern(sqlParams.timePattern)
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

  def apply(
      scheduler: ActorRef[SchedulerMessage],
      initStateData: InitPrimaryServiceProxyStateData,
  ): Behavior[PrimaryDataMessage] = Behaviors.setup { ctx =>
    val activationAdapter: ActorRef[Activation] =
      ctx.messageAdapter[Activation](msg => WrappedActivation(msg))

    implicit val constantData: ServiceConstantStateData =
      ServiceConstantStateData(
        scheduler,
        activationAdapter,
      )

    scheduler ! ScheduleActivation(
      activationAdapter,
      INIT_SIM_TICK,
    )

    uninitialized(initStateData)
  }

  /** Handle all messages, when the actor isn't initialized, yet.
    *
    * @return
    *   How receiving should be handled with gained insight of myself
    */
  private def uninitialized(
      initStateData: InitPrimaryServiceProxyStateData
  )(implicit
      constantData: ServiceConstantStateData
  ): Behavior[PrimaryDataMessage] = Behaviors.receive {
    case (ctx, WrappedActivation(Activation(INIT_SIM_TICK))) =>
      /* The proxy is asked to initialize itself. If that happened successfully, change the logic of receiving
       * messages */
      prepareStateData(
        initStateData.primaryConfig,
        initStateData.simulationStart,
      )(ctx.log) match {
        case Success(stateData) =>
          constantData.scheduler ! Completion(constantData.activationAdapter)
          onMessage(stateData)
        case Failure(exception) =>
          ctx.log.error(
            s"Unable to initialize the ${ctx.self.path}. Shut it down.",
            exception,
          )

          Behaviors.stopped
      }

    case (ctx, x) =>
      /* Unhandled message */
      ctx.log.error("Received unhandled message: {}", x)
      Behaviors.same
  }

  /** Prepare the needed state data by building a
    * [[edu.ie3.datamodel.io.source.TimeSeriesMappingSource]], obtain its
    * information and compile them to state data
    *
    * @param primaryConfig
    *   Configuration for the primary source
    * @param simulationStart
    *   Simulation time of first instant in simulation
    * @return
    *   State data, containing the known model and time series identifiers
    */
  private[service] def prepareStateData(
      primaryConfig: PrimaryConfig,
      simulationStart: ZonedDateTime,
  )(implicit log: Logger): Try[PrimaryServiceStateData] = {
    createSources(primaryConfig).map {
      case (mappingSource, metaInformationSource) =>
        val modelToTimeSeries = mappingSource.getMapping.asScala.toMap
        val timeSeriesToSourceRef = modelToTimeSeries.values
          .to(LazyList)
          .distinct
          .flatMap { timeSeriesUuid =>
            metaInformationSource
              .getTimeSeriesMetaInformation(timeSeriesUuid)
              .toScala match {
              case Some(metaInformation) =>
                /* Only register those entries, that meet the supported column schemes */
                when(
                  PrimaryServiceWorker.supportedColumnSchemes
                    .contains(metaInformation.getColumnScheme)
                ) {
                  timeSeriesUuid -> SourceRef(metaInformation, None)
                }
              case None =>
                log.warn(
                  "Unable to acquire meta information for time series '{}'. Leave that out.",
                  timeSeriesUuid,
                )
                None
            }
          }
          .toMap
        PrimaryServiceStateData(
          modelToTimeSeries,
          timeSeriesToSourceRef,
          simulationStart,
          primaryConfig,
          mappingSource,
        )
    }
  }

  private def createSources(
      primaryConfig: PrimaryConfig
  ): Try[(TimeSeriesMappingSource, TimeSeriesMetaInformationSource)] = {
    Seq(
      primaryConfig.sqlParams,
      primaryConfig.influxDb1xParams,
      primaryConfig.csvParams,
      primaryConfig.couchbaseParams,
    ).filter(_.isDefined).flatten.headOption match {
      case Some(PrimaryDataCsvParams(csvSep, directoryPath, _, _)) =>
        val fileNamingStrategy = new FileNamingStrategy()
        Success(
          new CsvTimeSeriesMappingSource(
            csvSep,
            Paths.get(directoryPath),
            fileNamingStrategy,
          ),
          new CsvTimeSeriesMetaInformationSource(
            csvSep,
            Paths.get(directoryPath),
            fileNamingStrategy,
          ),
        )
      case Some(sqlParams: SqlParams) =>
        val sqlConnector = new SqlConnector(
          sqlParams.jdbcUrl,
          sqlParams.userName,
          sqlParams.password,
        )
        Success(
          new SqlTimeSeriesMappingSource(
            sqlConnector,
            sqlParams.schemaName,
            new EntityPersistenceNamingStrategy(),
          ),
          new SqlTimeSeriesMetaInformationSource(
            sqlConnector,
            sqlParams.schemaName,
            new DatabaseNamingStrategy(),
          ),
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
  private[service] def onMessage(stateData: PrimaryServiceStateData)(implicit
      constantData: ServiceConstantStateData
  ): Behavior[PrimaryDataMessage] = Behaviors.receive {
    case (ctx, PrimaryServiceRegistrationMessage(actorRef, modelUuid)) =>
      /* Try to register for this model */
      stateData.modelToTimeSeries.get(modelUuid) match {
        case Some(timeSeriesUuid) =>
          /* There is a time series apparent for this model, try to get a worker for it */
          handleCoveredModel(
            modelUuid,
            timeSeriesUuid,
            stateData,
            actorRef,
          )(constantData, ctx)
        case None =>
          ctx.log.debug(
            s"There is no time series apparent for the model with uuid '{}'.",
            modelUuid,
          )
          actorRef ! RegistrationFailedMessage(ctx.self)
      }

      Behaviors.same
    case (ctx, x) =>
      ctx.log.error(
        s"Received message '$x', but I'm only able to handle registration requests."
      )
      Behaviors.same
  }

  /** Handle the registration request for a covered model. First, try to get an
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
  protected[service] def handleCoveredModel(
      modelUuid: UUID,
      timeSeriesUuid: UUID,
      stateData: PrimaryServiceStateData,
      requestingActor: ClassicRef,
  )(implicit
      constantData: ServiceConstantStateData,
      ctx: ActorContext[PrimaryDataMessage],
  ): Unit = {
    val timeSeriesToSourceRef = stateData.timeSeriesToSourceRef
    timeSeriesToSourceRef.get(timeSeriesUuid) match {
      case Some(SourceRef(_, Some(worker))) =>
        /* There is yet a worker apparent. Register the requesting actor. The worker will reply to the original
         * requesting actor. */
        worker ! WorkerRegistrationMessage(requestingActor)
      case Some(SourceRef(metaInformation, None)) =>
        /* There is NO worker apparent, yet. Spin one off. */
        initializeWorker(
          metaInformation,
          stateData.simulationStart,
          stateData.primaryConfig,
        ) match {
          case Success(workerRef) =>
            /* Forward the registration request. The worker will reply about successful registration or not. */
            workerRef ! WorkerRegistrationMessage(requestingActor)

            /* Register the new worker within the state data and change the context */
            onMessage(
              updateStateData(stateData, timeSeriesUuid, workerRef)
            )
          case Failure(exception) =>
            ctx.log.warn(
              s"A failure occurred during spin-off of a primary source for time series '$timeSeriesUuid'. " +
                s"Will inform the requesting actor, that registration is not possible.",
              exception,
            )
            requestingActor ! RegistrationFailedMessage(ctx.self)
        }

      case None =>
        ctx.log.warn(
          s"There is no source information for time series '$timeSeriesUuid' (requested for model " +
            s"'$modelUuid'), although the mapping contains information about it."
        )
        requestingActor ! RegistrationFailedMessage(ctx.self)
    }
  }

  /** Instantiate a new [[PrimaryServiceWorker]] and send initialization
    * information
    *
    * @param metaInformation
    *   Meta information (including column scheme) of the time series
    * @param simulationStart
    *   The time of the simulation start
    * @param primaryConfig
    *   Configuration for the primary config
    * @return
    *   The [[ActorRef]] to the worker
    */
  protected[service] def initializeWorker(
      metaInformation: IndividualTimeSeriesMetaInformation,
      simulationStart: ZonedDateTime,
      primaryConfig: PrimaryConfig,
  )(implicit
      constantData: ServiceConstantStateData,
      ctx: ActorContext[_],
  ): Try[ActorRef[PrimaryDataMessage]] = {
    val valueClass = metaInformation.getColumnScheme.getValueClass

    val workerRef = classToWorkerRef(metaInformation.getUuid.toString)
    toInitData(
      metaInformation,
      simulationStart,
      primaryConfig,
      valueClass,
    ) match {
      case Success(initData) =>
        workerRef ! Create(
          initData,
          ScheduleLock.singleKey(ctx, constantData.scheduler, INIT_SIM_TICK),
        )
        Success(workerRef)
      case Failure(cause) =>
        ctx.stop(workerRef)
        Failure(
          new InitializationException(
            "Unable to build init data for worker. Kill the uninitialized worker. Goodbye my friend!",
            cause,
          )
        )
    }
  }

  /** Build a primary source worker and type it to the foreseen value class to
    * come
    *
    * @param timeSeriesUuid
    *   uuid of the time series the actor processes
    * @return
    *   The [[ActorRef]] to the spun off actor
    */
  private[service] def classToWorkerRef(
      timeSeriesUuid: String
  )(implicit
      constantData: ServiceConstantStateData,
      ctx: ActorContext[_],
  ): ActorRef[PrimaryDataMessage] =
    ctx.spawn(
      PrimaryServiceWorker(constantData.scheduler),
      timeSeriesUuid,
    )

  /** Building proper init data for the worker
    *
    * @param metaInformation
    *   Meta information (including column scheme) of the time series
    * @param simulationStart
    *   The time of the simulation start
    * @param primaryConfig
    *   Configuration for the primary config
    * @return
    */
  private[service] def toInitData[V <: Value](
      metaInformation: IndividualTimeSeriesMetaInformation,
      simulationStart: ZonedDateTime,
      primaryConfig: PrimaryConfig,
      valueClass: Class[V],
  ): Try[InitPrimaryServiceStateData[V]] =
    primaryConfig match {
      case PrimaryConfig(
            None,
            Some(PrimaryDataCsvParams(csvSep, directoryPath, _, timePattern)),
            None,
            None,
          ) =>
        /* The actual data sources are from csv. Meta information have to match */
        metaInformation match {
          case csvMetaData: CsvIndividualTimeSeriesMetaInformation =>
            Success(
              CsvInitPrimaryServiceStateData(
                csvMetaData.getUuid,
                simulationStart,
                valueClass,
                csvSep,
                Paths.get(directoryPath),
                csvMetaData.getFullFilePath,
                new FileNamingStrategy(),
                timePattern,
              )
            )
          case invalidMetaData =>
            Failure(
              new InitializationException(
                s"Expected '${classOf[CsvIndividualTimeSeriesMetaInformation]}', but got '$invalidMetaData'."
              )
            )
        }

      case PrimaryConfig(
            None,
            None,
            None,
            Some(sqlParams: SqlParams),
          ) =>
        Success(
          SqlInitPrimaryServiceStateData(
            metaInformation.getUuid,
            simulationStart,
            valueClass,
            sqlParams,
            new DatabaseNamingStrategy(),
          )
        )

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
      workerRef: ActorRef[PrimaryDataMessage],
  ): PrimaryServiceStateData = {
    val timeSeriesToSourceRef = stateData.timeSeriesToSourceRef
    val sourceRef = timeSeriesToSourceRef.getOrElse(
      timeSeriesUuid,
      throw new IllegalArgumentException(
        s"Cannot update entry for time series '$timeSeriesUuid', as it hasn't been part of it before."
      ),
    )
    val updatedTimeSeriesToSourceRef = timeSeriesToSourceRef.updated(
      timeSeriesUuid,
      sourceRef.copy(worker = Some(workerRef)),
    )
    stateData.copy(timeSeriesToSourceRef = updatedTimeSeriesToSourceRef)
  }

}
