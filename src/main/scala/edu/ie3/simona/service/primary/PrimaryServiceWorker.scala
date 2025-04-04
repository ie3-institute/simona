/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import edu.ie3.datamodel.io.connectors.SqlConnector
import edu.ie3.datamodel.io.factory.timeseries.TimeBasedSimpleValueFactory
import edu.ie3.datamodel.io.naming.timeseries.ColumnScheme
import edu.ie3.datamodel.io.naming.{DatabaseNamingStrategy, FileNamingStrategy}
import edu.ie3.datamodel.io.source.TimeSeriesSource
import edu.ie3.datamodel.io.source.csv.CsvTimeSeriesSource
import edu.ie3.datamodel.io.source.sql.SqlTimeSeriesSource
import edu.ie3.datamodel.models.value.Value
import edu.ie3.simona.agent.participant.data.Data.PrimaryData
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.RichValue
import edu.ie3.simona.agent.participant2.ParticipantAgent
import edu.ie3.simona.agent.participant2.ParticipantAgent.{
  DataProvision,
  PrimaryRegistrationSuccessfulMessage,
}
import edu.ie3.simona.config.ConfigParams.TimeStampedSqlParams
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.exceptions.agent.ServiceRegistrationException
import edu.ie3.simona.exceptions.{
  CriticalFailureException,
  InitializationException,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  ServiceRegistrationMessage,
  WorkerRegistrationMessage,
}
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceBaseStateData,
}
import edu.ie3.simona.service.SimonaService
import edu.ie3.simona.util.TickUtil.{RichZonedDateTime, TickLong}
import edu.ie3.util.scala.collection.immutable.SortedDistinctSeq
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.slf4j.Logger

import java.nio.file.Path
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try}

object PrimaryServiceWorker extends SimonaService[ServiceMessage] {

  override type S = PrimaryServiceInitializedStateData[Value]

  /** List of supported column schemes aka. column schemes, that belong to
    * primary data
    */
  val supportedColumnSchemes: Vector[ColumnScheme] = Vector(
    ColumnScheme.ACTIVE_POWER,
    ColumnScheme.ACTIVE_POWER_AND_HEAT_DEMAND,
    ColumnScheme.APPARENT_POWER,
    ColumnScheme.APPARENT_POWER_AND_HEAT_DEMAND,
  )

  /** Abstract class pattern for specific [[InitializeServiceStateData]].
    * Different implementations are needed, because the [[PrimaryServiceProxy]]
    * already has detailed information about different source types, that can be
    * handed over instead of being acquired once again.
    */
  abstract class InitPrimaryServiceStateData[V <: Value]
      extends InitializeServiceStateData {
    val timeSeriesUuid: UUID
    val simulationStart: ZonedDateTime
    val valueClass: Class[V]
  }

  /** Specific implementation of [[InitPrimaryServiceStateData]], if the source
    * to use utilizes csv files.
    *
    * @param timeSeriesUuid
    *   Unique identifier of the time series to read
    * @param simulationStart
    *   Simulation time of the beginning of simulation time
    * @param csvSep
    *   Column separation character of the csv files
    * @param directoryPath
    *   Base directory path, where all input information are given
    * @param filePath
    *   Path of the file to read with respect to the given folder path (Without
    *   ending!)
    * @param fileNamingStrategy
    *   [[FileNamingStrategy]], the input files follow
    * @param timePattern
    *   The time format pattern of the time series
    */
  final case class CsvInitPrimaryServiceStateData[V <: Value](
      override val timeSeriesUuid: UUID,
      override val simulationStart: ZonedDateTime,
      override val valueClass: Class[V],
      csvSep: String,
      directoryPath: Path,
      filePath: Path,
      fileNamingStrategy: FileNamingStrategy,
      timePattern: String,
  ) extends InitPrimaryServiceStateData[V]

  /** Specific implementation of [[InitPrimaryServiceStateData]], if the source
    * to use utilizes an SQL database.
    *
    * @param timeSeriesUuid
    *   Unique identifier of the time series to read
    * @param simulationStart
    *   Simulation time of the beginning of simulation time
    * @param sqlParams
    *   Parameters regarding SQL connection and table selection
    * @param databaseNamingStrategy
    *   Strategy of naming database entities, such as tables
    */
  final case class SqlInitPrimaryServiceStateData[V <: Value](
      override val timeSeriesUuid: UUID,
      override val simulationStart: ZonedDateTime,
      override val valueClass: Class[V],
      sqlParams: TimeStampedSqlParams,
      databaseNamingStrategy: DatabaseNamingStrategy,
  ) extends InitPrimaryServiceStateData[V]

  /** Class carrying the state of a fully initialized [[PrimaryServiceWorker]]
    *
    * @param maybeNextActivationTick
    *   The next tick, when this actor is triggered by scheduler
    * @param activationTicks
    *   Linked collection of ticks, in which data is available
    * @param startDateTime
    *   Simulation time of the first instant in simulation
    * @param valueClass
    *   The class
    * @param source
    *   Implementation of [[TimeSeriesSource]] to use for actual acquisition of
    *   data
    * @param subscribers
    *   Collection of interested actors
    * @tparam V
    *   Type of value to get from source
    */
  final case class PrimaryServiceInitializedStateData[V <: Value](
      maybeNextActivationTick: Option[Long],
      activationTicks: SortedDistinctSeq[Long] = SortedDistinctSeq.empty,
      startDateTime: ZonedDateTime,
      valueClass: Class[V],
      source: TimeSeriesSource[V],
      subscribers: Vector[ActorRef[ParticipantAgent.Request]] = Vector.empty,
  ) extends ServiceBaseStateData

  /** Initialize the actor with the given information. Try to figure out the
    * initialized state data and the next activation ticks, that will then be
    * sent to the scheduler
    *
    * @param initServiceData
    *   The data that should be used for initialization
    * @return
    *   The state data of this service actor and optional tick that should be
    *   included in the completion message
    */
  override def init(
      initServiceData: InitializeServiceStateData
  ): Try[(PrimaryServiceInitializedStateData[Value], Option[Long])] = {
    (initServiceData match {
      case PrimaryServiceWorker.CsvInitPrimaryServiceStateData(
            timeSeriesUuid,
            simulationStart,
            valueClass,
            csvSep,
            directoryPath,
            filePath,
            fileNamingStrategy,
            timePattern,
          ) =>
        Try {
          /* Set up source and acquire information */
          val factory = new TimeBasedSimpleValueFactory(
            valueClass,
            DateTimeFormatter.ofPattern(timePattern),
          )
          val source = new CsvTimeSeriesSource(
            csvSep,
            directoryPath,
            fileNamingStrategy,
            timeSeriesUuid,
            filePath,
            valueClass,
            factory,
          )
          (source, simulationStart, valueClass)
        }

      case PrimaryServiceWorker.SqlInitPrimaryServiceStateData(
            timeSeriesUuid: UUID,
            simulationStart: ZonedDateTime,
            valueClass,
            sqlParams: TimeStampedSqlParams,
            namingStrategy: DatabaseNamingStrategy,
          ) =>
        Try {
          val factory =
            new TimeBasedSimpleValueFactory(
              valueClass,
              DateTimeFormatter.ofPattern(sqlParams.timePattern),
            )

          val sqlConnector = new SqlConnector(
            sqlParams.jdbcUrl,
            sqlParams.userName,
            sqlParams.password,
          )

          val source = new SqlTimeSeriesSource(
            sqlConnector,
            sqlParams.schemaName,
            namingStrategy,
            timeSeriesUuid,
            valueClass,
            factory,
          )

          (source, simulationStart, valueClass)
        }

      case unsupported =>
        /* Got the wrong init data */
        Failure(
          new InitializationException(
            s"Provided init data '${unsupported.getClass.getSimpleName}' for primary service are invalid!"
          )
        )
    }).flatMap {
      case (
            source: TimeSeriesSource[Value],
            simulationStart,
            valueClass: Class[Value],
          ) =>
        implicit val startDateTime: ZonedDateTime = simulationStart

        // Note: because we want data for the start tick as well, we need to use any tick before the start tick
        val intervalStart = simulationStart.minusSeconds(1)

        val (maybeNextTick, furtherActivationTicks) = SortedDistinctSeq(
          source
            .getTimeKeysAfter(intervalStart)
            .asScala
            .toSeq
            .map(_.toTick)
        ).pop

        (maybeNextTick, furtherActivationTicks) match {
          case (Some(tick), furtherActivationTicks) if tick == 0L =>
            /* Set up the state data and determine the next activation tick. */
            val initializedStateData
                : PrimaryServiceInitializedStateData[Value] =
              PrimaryServiceInitializedStateData(
                maybeNextTick,
                furtherActivationTicks,
                simulationStart,
                valueClass,
                source,
              )

            Success(initializedStateData, maybeNextTick)

          case (Some(tick), _) if tick > 0L =>
            /* The start of the data needs to be at the first tick of the simulation. */
            Failure(
              new ServiceRegistrationException(
                s"The data for the timeseries '${source.getTimeSeries.getUuid}' starts after the start of this simulation (tick: $tick)! This is not allowed!"
              )
            )

          case _ =>
            /* No data for the simulation. */
            Failure(
              new ServiceRegistrationException(
                s"No appropriate data found within simulation time range in timeseries '${source.getTimeSeries.getUuid}'!"
              )
            )
        }
    }
  }

  override protected def handleRegistrationRequest(
      registrationMessage: ServiceRegistrationMessage
  )(implicit
      serviceStateData: PrimaryServiceInitializedStateData[Value],
      ctx: ActorContext[ServiceMessage],
  ): Try[PrimaryServiceInitializedStateData[Value]] =
    registrationMessage match {
      case WorkerRegistrationMessage(agentToBeRegistered) =>
        agentToBeRegistered ! PrimaryRegistrationSuccessfulMessage(
          ctx.self,
          serviceStateData.maybeNextActivationTick.getOrElse(
            throw new CriticalFailureException(
              s"There is no primary data for $agentToBeRegistered"
            )
          ),
          PrimaryData.getPrimaryDataExtra(serviceStateData.valueClass),
        )
        val subscribers = serviceStateData.subscribers :+ agentToBeRegistered
        Success(serviceStateData.copy(subscribers = subscribers))
      case unsupported =>
        Failure(
          InvalidRegistrationRequestException(
            s"A primary service provider is not able to handle registration request '$unsupported'."
          )
        )
    }

  /** Send out the information to all registered recipients
    *
    * @param tick
    *   Current tick data should be announced for
    * @param serviceBaseStateData
    *   The current state data of this service
    * @return
    *   The service stata data that should be used in the next state (normally
    *   with updated values) together with the completion message that is sent
    *   in response to the trigger that is sent to start the initialization
    *   process
    */
  override protected def announceInformation(
      tick: Long
  )(implicit
      serviceBaseStateData: PrimaryServiceInitializedStateData[Value],
      ctx: ActorContext[ServiceMessage],
  ): (
      PrimaryServiceInitializedStateData[Value],
      Option[Long],
  ) = {
    /* Get the information to distribute */
    val simulationTime = tick.toDateTime(serviceBaseStateData.startDateTime)
    serviceBaseStateData.source.getValue(simulationTime).toScala match {
      case Some(value) =>
        processDataAndAnnounce(tick, value, serviceBaseStateData)(
          ctx.self,
          ctx.log,
        )
      case None =>
        /* There is no data available in the source. */
        ctx.log.warn(
          s"I expected to get data for tick '{}' ({}), but data is not available",
          tick,
          simulationTime,
        )
        updateStateDataAndBuildTriggerMessages(serviceBaseStateData)
    }
  }

  /** Pop the next activation tick, remove it from given base state data and
    * hand back the updated state data together with an Option on a next tick
    *
    * @param baseStateData
    *   The base state data to update
    * @return
    *   Updated base state data and an option on a sequence of schedule trigger
    *   messages
    */
  private def updateStateDataAndBuildTriggerMessages[V <: Value](
      baseStateData: PrimaryServiceInitializedStateData[V]
  ): (
      PrimaryServiceInitializedStateData[V],
      Option[Long],
  ) = {
    val (maybeNextActivationTick, remainderActivationTicks) =
      baseStateData.activationTicks.pop
    (
      baseStateData.copy(
        maybeNextActivationTick = maybeNextActivationTick,
        activationTicks = remainderActivationTicks,
      ),
      maybeNextActivationTick,
    )
  }

  /** Process the information from source and announce it to subscribers
    *
    * @param tick
    *   Current tick in simulation
    * @param value
    *   Actual value from source
    * @param serviceBaseStateData
    *   State data of the service
    * @return
    *   Updated state data as well as an optional sequence of triggers to be
    *   sent to scheduler
    */
  private[service] def processDataAndAnnounce[V <: Value](
      tick: Long,
      value: Value,
      serviceBaseStateData: PrimaryServiceInitializedStateData[V],
  )(implicit
      self: ActorRef[ServiceMessage],
      log: Logger,
  ): (
      PrimaryServiceInitializedStateData[V],
      Option[Long],
  ) = value.toPrimaryData match {
    case Success(primaryData) =>
      announcePrimaryData(tick, primaryData, serviceBaseStateData)
    case Failure(exception) =>
      /* Processing of data failed */
      log.warn(
        "Unable to convert received value to primary data. Skipped that data." +
          "\nException: {}",
        exception,
      )
      updateStateDataAndBuildTriggerMessages(serviceBaseStateData)
  }

  /** Announce the given primary data to all subscribers
    *
    * @param tick
    *   Current tick in simulation
    * @param primaryData
    *   Actual data to distribute
    * @param serviceBaseStateData
    *   State data of the service
    * @return
    *   Updated state data as well as an optional sequence of triggers to be
    *   sent to scheduler
    */
  private[service] def announcePrimaryData[V <: Value](
      tick: Long,
      primaryData: PrimaryData,
      serviceBaseStateData: PrimaryServiceInitializedStateData[V],
  )(implicit self: ActorRef[ServiceMessage]): (
      PrimaryServiceInitializedStateData[V],
      Option[Long],
  ) = {
    val (maybeNextTick, remainderActivationTicks) =
      serviceBaseStateData.activationTicks.pop
    val updatedStateData =
      serviceBaseStateData.copy(
        maybeNextActivationTick = maybeNextTick,
        activationTicks = remainderActivationTicks,
      )

    val provisionMessage =
      DataProvision(tick, self, primaryData, maybeNextTick)
    serviceBaseStateData.subscribers.foreach(_ ! provisionMessage)
    (updatedStateData, maybeNextTick)
  }
}
