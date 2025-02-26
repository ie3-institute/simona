/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.load

import edu.ie3.datamodel.io.connectors.SqlConnector
import edu.ie3.datamodel.io.csv.CsvLoadProfileMetaInformation
import edu.ie3.datamodel.io.factory.timeseries.{
  BdewLoadProfileFactory,
  LoadProfileFactory,
  RandomLoadProfileFactory,
}
import edu.ie3.datamodel.io.naming.{DatabaseNamingStrategy, FileNamingStrategy}
import edu.ie3.datamodel.io.source.LoadProfileSource
import edu.ie3.datamodel.io.source.csv.{
  CsvDataSource,
  CsvLoadProfileSource,
  CsvTimeSeriesMetaInformationSource,
}
import edu.ie3.datamodel.io.source.sql.{
  SqlDataSource,
  SqlLoadProfileSource,
  SqlTimeSeriesMetaInformationSource,
}
import edu.ie3.datamodel.models.profile.LoadProfile.RandomLoadProfile
import edu.ie3.datamodel.models.profile.{BdewStandardLoadProfile, LoadProfile}
import edu.ie3.datamodel.models.timeseries.repetitive.LoadProfileTimeSeries
import edu.ie3.datamodel.models.value.load.LoadValues
import edu.ie3.simona.agent.participant2.ParticipantAgent.{
  DataProvision,
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.config.SimonaConfig.BaseCsvParams
import edu.ie3.simona.config.SimonaConfig.Simona.Input.LoadProfile.Datasource
import edu.ie3.simona.config.SimonaConfig.Simona.Input.LoadProfile.Datasource.SqlParams
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.exceptions.{
  CriticalFailureException,
  InitializationException,
}
import edu.ie3.simona.ontology.messages.services.LoadProfileMessage.{
  LoadData,
  RegisterForLoadProfileService,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceActivationBaseStateData,
}
import edu.ie3.simona.service.SimonaService
import edu.ie3.simona.service.load.LoadProfileService.{
  InitLoadProfileServiceStateData,
  LoadProfileInitializedStateData,
  initLoadProfileStore,
}
import edu.ie3.simona.util.TickUtil.{RichZonedDateTime, TickLong}
import edu.ie3.simona.util.{SimonaConstants, TickUtil}
import edu.ie3.util.scala.collection.immutable.SortedDistinctSeq
import org.apache.pekko.actor.{ActorContext, ActorRef, Props}

import java.nio.file.Path
import java.time.{Duration, ZonedDateTime}
import scala.jdk.CollectionConverters.MapHasAsScala
import scala.util.{Failure, Success, Try}

/** Load Profile Service is responsible to register other actors that require
  * load profile information and provide load profile time series information
  * when requested
  */
final case class LoadProfileService(
    override val scheduler: ActorRef,
    private implicit val simulationStartTime: ZonedDateTime,
    simulationEnd: ZonedDateTime,
    private implicit val resolution: Duration = Duration.ofMinutes(15),
) extends SimonaService[LoadProfileInitializedStateData](scheduler) {

  /** Initialize the concrete service implementation using the provided
    * initialization data. This method should perform all heavyweight tasks
    * before the actor becomes ready. The return values are a) the state data of
    * the initialized service and b) optional triggers that should be send to
    * the [[edu.ie3.simona.scheduler.Scheduler]] together with the completion
    * message that is send in response to the trigger that is send to start the
    * initialization process
    *
    * @param initServiceData
    *   the data that should be used for initialization
    * @return
    *   the state data of this service and optional tick that should be included
    *   in the completion message
    */
  override def init(
      initServiceData: InitializeServiceStateData
  ): Try[(LoadProfileInitializedStateData, Option[Long])] =
    initServiceData match {
      case InitLoadProfileServiceStateData(dataSource) =>
        val loadProfileStore = initLoadProfileStore(dataSource)

        /* What is the first tick to be triggered for? And what are further activation ticks */
        val (maybeNextTick, furtherActivationTicks) = SortedDistinctSeq(
          TickUtil
            .getTicksInBetween(
              SimonaConstants.FIRST_TICK_IN_SIMULATION,
              simulationEnd.toTick,
              resolution.toSeconds,
            )
            .toSeq
        ).pop

        val initializedStateData = LoadProfileInitializedStateData(
          loadProfileStore,
          Map.empty,
          activationTicks = furtherActivationTicks,
          maybeNextActivationTick = maybeNextTick,
        )

        Success(
          initializedStateData,
          maybeNextTick,
        )
      case invalidData =>
        Failure(
          new InitializationException(
            s"Provided init data '${invalidData.getClass.getSimpleName}' for load profile service are invalid!"
          )
        )
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
      serviceStateData: LoadProfileInitializedStateData
  ): Try[LoadProfileInitializedStateData] = registrationMessage match {
    case RegisterForLoadProfileService(loadProfile) =>
      Success(handleRegistrationRequest(sender(), loadProfile))
    case invalidMessage =>
      Failure(
        InvalidRegistrationRequestException(
          "Cannot register an agent for load profile service with registration " +
            s"request message '${invalidMessage.getClass.getSimpleName}'!"
        )
      )
  }

  /** Try to register the sending agent with its load profile for load profile
    * value provision
    *
    * @param agentToBeRegistered
    *   the agent that wants to be registered
    * @param loadProfile
    *   of the agent
    * @param serviceStateData
    *   the current service state data of this service
    * @return
    *   an updated state data of this service that contains registration
    *   information if the registration has been carried out successfully
    */
  private def handleRegistrationRequest(
      agentToBeRegistered: ActorRef,
      loadProfile: LoadProfile,
  )(implicit
      serviceStateData: LoadProfileInitializedStateData
  ): LoadProfileInitializedStateData = {

    serviceStateData.profileToRefs.get(loadProfile) match {
      case None =>
        /* The load profile itself is not known yet. Try to figure out, which weather coordinates are relevant */

        if (serviceStateData.loadProfileStore.contains(loadProfile)) {
          // we can provide data for the agent
          agentToBeRegistered ! RegistrationSuccessfulMessage(
            self,
            serviceStateData.maybeNextActivationTick.getOrElse(
              throw new CriticalFailureException(
                "No first data tick for weather service"
              )
            ),
          )

          serviceStateData.copy(profileToRefs =
            serviceStateData.profileToRefs + (loadProfile -> Vector(
              agentToBeRegistered
            ))
          )
        } else {
          // we cannot provide data for the agent
          log.error(
            s"Unable to obtain necessary information to register for load profile '${loadProfile.getKey}'."
          )

          sender() ! RegistrationFailedMessage(self)
          serviceStateData
        }

      case Some(actorRefs) if !actorRefs.contains(agentToBeRegistered) =>
        // load profile is already known (= we have data for it), but this actor is not registered yet
        agentToBeRegistered ! RegistrationSuccessfulMessage(
          self,
          serviceStateData.maybeNextActivationTick.getOrElse(
            throw new CriticalFailureException(
              "No first data tick for weather service"
            )
          ),
        )

        serviceStateData.copy(
          profileToRefs =
            serviceStateData.profileToRefs + (loadProfile -> (actorRefs :+ agentToBeRegistered))
        )

      case Some(actorRefs) if actorRefs.contains(agentToBeRegistered) =>
        // actor is already registered, do nothing
        log.warning(
          "Sending actor {} is already registered",
          agentToBeRegistered,
        )
        serviceStateData

      case _ =>
        // actor is not registered and we don't have data for it
        // inform the agentToBeRegistered that the registration failed as we don't have data for it
        agentToBeRegistered ! RegistrationFailedMessage(self)
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
  override protected def announceInformation(tick: Long)(implicit
      serviceStateData: LoadProfileInitializedStateData,
      ctx: ActorContext,
  ): (LoadProfileInitializedStateData, Option[Long]) = {

    /* Pop the next activation tick and update the state data */
    val (
      maybeNextTick: Option[Long],
      updatedStateData: LoadProfileInitializedStateData,
    ) = {
      val (nextTick, remainderTicks) = serviceStateData.activationTicks.pop
      (nextTick, serviceStateData.copy(activationTicks = remainderTicks))
    }

    val time = tick.toDateTime(simulationStartTime)
    val loadProfileStore = serviceStateData.loadProfileStore

    serviceStateData.profileToRefs.foreach { case (loadProfile, actorRefs) =>
      loadProfileStore.entry(time, loadProfile) match {
        case Some(averagePower) =>
          /* Sending the found value to the requester */
          actorRefs.foreach(recipient =>
            recipient ! DataProvision(
              tick,
              self,
              LoadData(averagePower),
              maybeNextTick,
            )
          )
        case None =>
          /* There is no data available in the source. */
          log.warning(
            s"No power value found for load profile {} for time: {}",
            loadProfile,
            time,
          )
      }
    }

    (updatedStateData, maybeNextTick)
  }
}

object LoadProfileService {

  def props(
      scheduler: ActorRef,
      startDateTime: ZonedDateTime,
      simulationEnd: ZonedDateTime,
  ): Props = Props(
    new LoadProfileService(
      scheduler,
      startDateTime,
      simulationEnd,
    )
  )

  /** @param loadProfileStore
    *   that stores that contains all load profiles
    * @param profileToRefs
    *   map: actor ref to [[LoadProfile]]
    * @param maybeNextActivationTick
    *   the next tick, when this actor is triggered by scheduler
    * @param activationTicks
    *   sorted set of ticks, that yet have been sent to the scheduler (w\o next
    *   tick)
    */
  final case class LoadProfileInitializedStateData(
      loadProfileStore: LoadProfileStore,
      profileToRefs: Map[LoadProfile, Vector[ActorRef]] = Map.empty,
      override val maybeNextActivationTick: Option[Long],
      override val activationTicks: SortedDistinctSeq[Long],
  ) extends ServiceActivationBaseStateData

  /** Load profile service state data used for initialization of the load
    * profile sources
    * @param sourceDefinition
    *   the definition of the sources to use
    */
  final case class InitLoadProfileServiceStateData(
      sourceDefinition: Datasource
  ) extends InitializeServiceStateData

  /** Initializes the load profile sources.
    * @param cfg
    *   configuration
    * @return
    *   the option for the build in [[LoadProfileTimeSeries]] as well as a map:
    *   load profile to source
    */
  def initLoadProfileStore(
      cfg: Datasource
  ): LoadProfileStore = {
    val definedSources = Vector(
      cfg.csvParams,
      cfg.sqlParams,
    ).find(_.isDefined).flatten

    val otherSources: Map[LoadProfile, LoadProfileSource[_, _]] =
      definedSources match {
        case Some(BaseCsvParams(csvSep, directoryPath, _)) =>
          // initializing a csv load profile source
          readCsvSources(csvSep, Path.of(directoryPath))

        case Some(sqlParams: SqlParams) =>
          // initializing a sql load profile source
          val sqlConnector = new SqlConnector(
            sqlParams.jdbcUrl,
            sqlParams.userName,
            sqlParams.password,
          )

          readSqlSources(sqlConnector, sqlParams.schemaName)
        case _ =>
          Map.empty[LoadProfile, LoadProfileSource[_, _]]
      }

    LoadProfileStore(otherSources)
  }

  /** Method to read csv load profiles.
    * @param csvSep
    *   separator of the source
    * @param directoryPath
    *   the path of the source
    * @return
    *   a map: load profile to load profile time series
    */
  private def readCsvSources(
      csvSep: String,
      directoryPath: Path,
  ): Map[LoadProfile, CsvLoadProfileSource[_, _]] = {
    val source =
      new CsvDataSource(csvSep, directoryPath, new FileNamingStrategy())

    val metaInformation = new CsvTimeSeriesMetaInformationSource(
      source
    ).getLoadProfileMetaInformation.asScala.toMap
    val classesAndFactories = getClassesAndFactories(metaInformation.keySet)

    metaInformation.map {
      case (_, information: CsvLoadProfileMetaInformation) =>
        val (profile, entryClass, entryFactory) =
          classesAndFactories(information.getProfile)

        val loadProfileSource =
          new CsvLoadProfileSource[LoadProfile, LoadValues](
            source,
            information,
            entryClass.asInstanceOf,
            entryFactory.asInstanceOf,
          )

        profile -> loadProfileSource
    }
  }

  /** Method to read csv load profiles.
    * @param sqlConnector
    *   to connect to a database
    * @param schemaName
    *   of the load profile schema
    * @return
    *   a map: load profile to load profile time series
    */
  private def readSqlSources(
      sqlConnector: SqlConnector,
      schemaName: String,
  ): Map[LoadProfile, SqlLoadProfileSource[_, _]] = {
    val namingStrategy = new DatabaseNamingStrategy()
    val sqlDataSource: SqlDataSource =
      new SqlDataSource(sqlConnector, schemaName, namingStrategy)

    val metaInformation = new SqlTimeSeriesMetaInformationSource(
      sqlConnector,
      schemaName,
      namingStrategy,
    ).getLoadProfileMetaInformation.asScala.toMap
    val classesAndFactories = getClassesAndFactories(metaInformation.keySet)

    metaInformation.map { case (_, information) =>
      val (profile, entryClass, entryFactory) =
        classesAndFactories(information.getProfile)

      val loadProfileSource =
        new SqlLoadProfileSource[LoadProfile, LoadValues](
          sqlDataSource,
          information,
          entryClass.asInstanceOf,
          entryFactory.asInstanceOf,
        )

      profile -> loadProfileSource
    }
  }

  private def getClassesAndFactories(
      profiles: Set[String]
  ): Map[String, (LoadProfile, Class[_], LoadProfileFactory[_, _])] = {
    profiles.map {
      case profile @ ("G0" | "G1" | "G2" | "G3" | "G4" | "G5" | "G6" | "H0" |
          "I0" | "I1" | "I2") =>
        val factory = new BdewLoadProfileFactory()

        profile -> (
          factory.parseProfile(profile),
          classOf[BdewStandardLoadProfile],
          factory
        )
      case profile @ "random" =>
        profile -> (
          RandomLoadProfile.RANDOM_LOAD_PROFILE,
          classOf[RandomLoadProfile],
          new RandomLoadProfileFactory()
        )
      case other =>
        throw new InitializationException(
          s"No implementation found for load profile $other!"
        )
    }
  }.toMap
}
