/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import akka.actor.{ActorRef, Props}
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
import edu.ie3.simona.config.SimonaConfig.Simona.Input.Primary.SqlParams
import edu.ie3.simona.exceptions.InitializationException
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationSuccessfulMessage
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceActivationBaseStateData
}
import edu.ie3.simona.service.primary.PrimaryServiceWorker.{
  PrimaryServiceInitializedStateData,
  ProvidePrimaryDataMessage
}
import edu.ie3.simona.service.{ServiceStateData, SimonaService}
import edu.ie3.simona.util.TickUtil.{RichZonedDateTime, TickLong}
import edu.ie3.util.scala.collection.immutable.SortedDistinctSeq

import java.time.ZonedDateTime
import java.util.UUID
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try}

final case class PrimaryServiceWorker[V <: Value](
    override protected val scheduler: ActorRef,
    valueClass: Class[V]
) extends SimonaService[PrimaryServiceInitializedStateData[V]](scheduler) {

  /** Initialize the actor with the given information. Try to figure out the
    * initialized state data and the next activation ticks, that will then be
    * sent to the scheduler
    *
    * @param initServiceData
    *   the data that should be used for initialization
    * @return
    *   the state data of this service actor and optional triggers that should
    *   be included in the completion message
    */
  override def init(
      initServiceData: ServiceStateData.InitializeServiceStateData
  ): Try[
    (
        PrimaryServiceInitializedStateData[V],
        Option[Seq[SchedulerMessage.ScheduleTriggerMessage]]
    )
  ] = {
    (initServiceData match {
      case PrimaryServiceWorker.CsvInitPrimaryServiceStateData(
            timeSeriesUuid,
            simulationStart,
            csvSep,
            directoryPath,
            filePath,
            fileNamingStrategy,
            timePattern
          ) =>
        Try {
          /* Set up source and acquire information */
          val factory = new TimeBasedSimpleValueFactory(valueClass, timePattern)
          val source = new CsvTimeSeriesSource(
            csvSep,
            directoryPath,
            fileNamingStrategy,
            timeSeriesUuid,
            filePath,
            valueClass,
            factory
          )
          (source, simulationStart)
        }

      case PrimaryServiceWorker.SqlInitPrimaryServiceStateData(
            timeSeriesUuid: UUID,
            simulationStart: ZonedDateTime,
            sqlParams: SqlParams,
            namingStrategy: DatabaseNamingStrategy
          ) =>
        Try {
          val factory =
            new TimeBasedSimpleValueFactory(valueClass, sqlParams.timePattern)

          val sqlConnector = new SqlConnector(
            sqlParams.jdbcUrl,
            sqlParams.userName,
            sqlParams.password
          )

          val source = new SqlTimeSeriesSource(
            sqlConnector,
            sqlParams.schemaName,
            namingStrategy,
            timeSeriesUuid,
            valueClass,
            factory
          )

          (source, simulationStart)
        }

      case unsupported =>
        /* Got the wrong init data */
        Failure(
          new InitializationException(
            s"Provided init data '${unsupported.getClass.getSimpleName}' for primary service are invalid!"
          )
        )
    }).map { case (source, simulationStart) =>
      implicit val startDateTime: ZonedDateTime = simulationStart

      val (maybeNextTick, furtherActivationTicks) = SortedDistinctSeq(
        // Note: The whole data set is used here, which might be inefficient depending on the source implementation.
        source.getTimeSeries.getEntries.asScala
          .filter { timeBasedValue =>
            val dateTime = timeBasedValue.getTime
            dateTime.isEqual(simulationStart) || dateTime.isAfter(
              simulationStart
            )
          }
          .map(timeBasedValue => timeBasedValue.getTime.toTick)
          .toSeq
          .sorted
      ).pop

      /* Set up the state data and determine the next activation tick. */
      val initializedStateData =
        PrimaryServiceInitializedStateData(
          maybeNextTick,
          furtherActivationTicks,
          simulationStart,
          source
        )
      val triggerMessage =
        ServiceActivationBaseStateData.tickToScheduleTriggerMessages(
          maybeNextTick,
          self
        )
      (initializedStateData, triggerMessage)
    }
  }

  /** Handle a request to register for information from this service
    *
    * @param registrationMessage
    *   registration message to handle
    * @param serviceStateData
    *   current state data of the actor
    * @return
    *   the service stata data that should be used in the next state (normally
    *   with updated values)
    */
  override protected def handleRegistrationRequest(
      registrationMessage: ServiceMessage.ServiceRegistrationMessage
  )(implicit
      serviceStateData: PrimaryServiceInitializedStateData[V]
  ): Try[PrimaryServiceInitializedStateData[V]] = registrationMessage match {
    case ServiceMessage.WorkerRegistrationMessage(requestingActor) =>
      requestingActor ! RegistrationSuccessfulMessage(
        serviceStateData.maybeNextActivationTick
      )
      val subscribers = serviceStateData.subscribers :+ requestingActor
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
    *   current tick data should be announced for
    * @param serviceBaseStateData
    *   the current state data of this service
    * @return
    *   the service stata data that should be used in the next state (normally
    *   with updated values) together with the completion message that is send
    *   in response to the trigger that is send to start the initialization
    *   process
    */
  override protected def announceInformation(
      tick: Long
  )(implicit serviceBaseStateData: PrimaryServiceInitializedStateData[V]): (
      PrimaryServiceInitializedStateData[V],
      Option[Seq[SchedulerMessage.ScheduleTriggerMessage]]
  ) = {
    /* Get the information to distribute */
    val wallClockTime = tick.toDateTime(serviceBaseStateData.startDateTime)
    serviceBaseStateData.source.getValue(wallClockTime).toScala match {
      case Some(value) =>
        processDataAndAnnounce(tick, value, serviceBaseStateData)
      case None =>
        /* There is no data available in the source. */
        log.warning(
          s"I expected to get data for tick '{}' ({}), but data is not available",
          tick,
          wallClockTime
        )
        updateStateDataAndBuildTriggerMessages(serviceBaseStateData)
    }
  }

  /** Pop the next activation tick, remove it from given base state data and
    * hand back the updated state data together with an Option on a sequence of
    * [[edu.ie3.simona.ontology.messages.SchedulerMessage#ScheduleTriggerMessage]]
    *
    * @param baseStateData
    *   The base state data to update
    * @return
    *   Updated base state data and an option on a sequence of schedule trigger
    *   messages
    */
  private def updateStateDataAndBuildTriggerMessages(
      baseStateData: PrimaryServiceInitializedStateData[V]
  ): (
      PrimaryServiceInitializedStateData[V],
      Option[Seq[SchedulerMessage.ScheduleTriggerMessage]]
  ) = {
    val (maybeNextActivationTick, remainderActivationTicks) =
      baseStateData.activationTicks.pop
    val triggerMessages =
      ServiceActivationBaseStateData.tickToScheduleTriggerMessages(
        maybeNextActivationTick,
        self
      )
    (
      baseStateData.copy(
        maybeNextActivationTick = maybeNextActivationTick,
        activationTicks = remainderActivationTicks
      ),
      triggerMessages
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
    *   updated state data as well as an optional sequence of triggers to be
    *   sent to scheduler
    */
  private def processDataAndAnnounce(
      tick: Long,
      value: V,
      serviceBaseStateData: PrimaryServiceInitializedStateData[V]
  ): (
      PrimaryServiceInitializedStateData[V],
      Option[Seq[SchedulerMessage.ScheduleTriggerMessage]]
  ) = value.toPrimaryData match {
    case Success(primaryData) =>
      announcePrimaryData(tick, primaryData, serviceBaseStateData)
    case Failure(exception) =>
      /* Processing of data failed */
      log.warning(
        "Unable to convert received value to primary data. Skipped that data." +
          "\nException: {}",
        exception
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
    *   updated state data as well as an optional sequence of triggers to be
    *   sent to scheduler
    */
  private def announcePrimaryData(
      tick: Long,
      primaryData: PrimaryData,
      serviceBaseStateData: PrimaryServiceInitializedStateData[V]
  ): (
      PrimaryServiceInitializedStateData[V],
      Option[Seq[SchedulerMessage.ScheduleTriggerMessage]]
  ) = {
    val (maybeNextTick, remainderActivationTicks) =
      serviceBaseStateData.activationTicks.pop
    val triggerMessages = ServiceActivationBaseStateData
      .tickToScheduleTriggerMessages(maybeNextTick, self)
    val updatedStateData =
      serviceBaseStateData.copy(
        maybeNextActivationTick = maybeNextTick,
        activationTicks = remainderActivationTicks
      )

    val provisionMessage =
      ProvidePrimaryDataMessage(tick, primaryData, maybeNextTick)
    serviceBaseStateData.subscribers.foreach(_ ! provisionMessage)
    (updatedStateData, triggerMessages)
  }
}

object PrimaryServiceWorker {

  /** List of supported column schemes aka. column schemes, that belong to
    * primary data
    */
  val supportedColumnSchemes: Vector[ColumnScheme] = Vector(
    ColumnScheme.ACTIVE_POWER,
    ColumnScheme.ACTIVE_POWER_AND_HEAT_DEMAND,
    ColumnScheme.APPARENT_POWER,
    ColumnScheme.APPARENT_POWER_AND_HEAT_DEMAND
  )

  def props[V <: Value](
      scheduler: ActorRef,
      valueClass: Class[V]
  ): Props =
    Props(new PrimaryServiceWorker(scheduler, valueClass))

  /** Abstract class pattern for specific [[InitializeServiceStateData]].
    * Different implementations are needed, because the [[PrimaryServiceProxy]]
    * already has detailed information about different source types, that can be
    * handed over instead of being acquired once again.
    */
  abstract class InitPrimaryServiceStateData
      extends InitializeServiceStateData {
    val timeSeriesUuid: UUID
    val simulationStart: ZonedDateTime
  }

  /** Specific implementation of [[InitPrimaryServiceStateData]], if the source
    * to use utilizes csv files.
    *
    * @param timeSeriesUuid
    *   Unique identifier of the time series to read
    * @param simulationStart
    *   Wall clock time of the beginning of simulation time
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
    *   the time format pattern of the time series
    */
  final case class CsvInitPrimaryServiceStateData(
      override val timeSeriesUuid: UUID,
      override val simulationStart: ZonedDateTime,
      csvSep: String,
      directoryPath: String,
      filePath: String,
      fileNamingStrategy: FileNamingStrategy,
      timePattern: String
  ) extends InitPrimaryServiceStateData

  /** Specific implementation of [[InitPrimaryServiceStateData]], if the source
    * to use utilizes an SQL database.
    *
    * @param timeSeriesUuid
    *   Unique identifier of the time series to read
    * @param simulationStart
    *   Wall clock time of the beginning of simulation time
    * @param sqlParams
    *   Parameters regarding SQL connection and table selection
    * @param databaseNamingStrategy
    *   Strategy of naming database entities, such as tables
    */
  final case class SqlInitPrimaryServiceStateData(
      override val timeSeriesUuid: UUID,
      override val simulationStart: ZonedDateTime,
      sqlParams: SqlParams,
      databaseNamingStrategy: DatabaseNamingStrategy
  ) extends InitPrimaryServiceStateData

  /** Class carrying the state of a fully initialized [[PrimaryServiceWorker]]
    *
    * @param maybeNextActivationTick
    *   the next tick, when this actor is triggered by scheduler
    * @param activationTicks
    *   Linked collection of ticks, in which data is available
    * @param startDateTime
    *   Wall clock time of the first instant in simulation
    * @param source
    *   Implementation of [[TimeSeriesSource]] to use for actual acquisition of
    *   data
    * @param subscribers
    *   Collection of interested actors
    * @tparam V
    *   Type of value to get from source
    */
  final case class PrimaryServiceInitializedStateData[V <: Value](
      override val maybeNextActivationTick: Option[Long],
      override val activationTicks: SortedDistinctSeq[Long] =
        SortedDistinctSeq.empty,
      startDateTime: ZonedDateTime,
      source: TimeSeriesSource[V],
      subscribers: Vector[ActorRef] = Vector.empty[ActorRef]
  ) extends ServiceActivationBaseStateData

  /** Provide primary data to subscribes
    *
    * @param tick
    *   Current tick
    * @param data
    *   The payload
    * @param nextDataTick
    *   The next tick, when data is available
    */
  final case class ProvidePrimaryDataMessage(
      override val tick: Long,
      override val data: PrimaryData,
      override val nextDataTick: Option[Long]
  ) extends ServiceMessage.ProvisionMessage[PrimaryData]
}
