/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.load

import edu.ie3.datamodel.models.profile.PowerProfileKey
import edu.ie3.simona.config.InputConfig.LoadProfile.Datasource
import edu.ie3.simona.exceptions.InitializationException
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.ontology.messages.ServiceMessage
import edu.ie3.simona.ontology.messages.ServiceMessage.*
import edu.ie3.simona.service.Data.SecondaryData
import edu.ie3.simona.service.Data.SecondaryData.{
  LoadDataFunction,
  SecondarySeriesData,
}
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceBaseStateData,
}
import edu.ie3.simona.service.{DataTimeType, SimonaService}
import edu.ie3.simona.util.SimonaConstants.FIRST_TICK_IN_SIMULATION
import edu.ie3.simona.util.TickUtil.toDateTime
import edu.ie3.util.scala.collection.immutable.RichMultiMap.*
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.slf4j.Logger

import java.time.ZonedDateTime
import scala.collection.immutable.SortedMap
import scala.util.{Failure, Success, Try}

/** Load Profile Service is responsible to register other actors that require
  * load profile information and provide load profile time series information
  * when requested
  */
object LoadProfileService extends SimonaService {

  override type S = LoadProfileInitializedStateData

  /** Container storing registered actors for a load profile.
    *
    * @param registrantsMap
    *   A map of data time type to registered actors.
    */
  final case class RegistrantsContainer(
      registrantsMap: Map[DataTimeType, Set[
        ActorRef[ServiceMessage.Response]
      ]] = Map.empty
  )

  /** @param loadProfileStore
    *   That stores that contains all load profiles.
    * @param registeredAgents
    *   Registered agents by [[PowerProfileKey]].
    * @param profileResolutions
    *   Map: [[LoadProfile]] to resolution.
    * @param profileToNextActivationTick
    *   Map: [[LoadProfile]] to next activation tick.
    * @param simulationStartTime
    *   Start of the simulation.
    */
  final case class LoadProfileInitializedStateData(
      loadProfileStore: LoadProfileStore,
      registeredAgents: Map[PowerProfileKey, RegistrantsContainer] = Map.empty,
      profileResolutions: Map[PowerProfileKey, Long],
      profileToNextActivationTick: Map[PowerProfileKey, Long],
      simulationStartTime: ZonedDateTime,
  ) extends ServiceBaseStateData

  /** Load profile service state data used for initialization of the load
    * profile sources.
    *
    * @param sourceDefinition
    *   The definition of additional sources. If no definition is given, only
    *   the build in load profiles can be used.
    * @param simulationStartTime
    *   The time the simulation is started.
    */
  final case class InitLoadProfileServiceStateData(
      sourceDefinition: Datasource,
      simulationStartTime: ZonedDateTime,
  ) extends InitializeServiceStateData

  override def init(
      initServiceData: InitializeServiceStateData
  )(using log: Logger): Try[(LoadProfileInitializedStateData, Option[Long])] =
    initServiceData match {
      case InitLoadProfileServiceStateData(
            dataSource,
            simStartTime,
          ) =>
        val loadProfileStore = LoadProfileStore(dataSource)

        val profileResolutions = loadProfileStore.getProfileResolutions
        val profiles = profileResolutions.keySet

        val profileToNextActivationTick =
          profiles.map(_ -> FIRST_TICK_IN_SIMULATION).toMap

        val initializedStateData = LoadProfileInitializedStateData(
          loadProfileStore,
          Map.empty,
          profileResolutions,
          profileToNextActivationTick,
          simStartTime,
        )

        Success(
          initializedStateData,
          Some(FIRST_TICK_IN_SIMULATION),
        )
      case invalidData =>
        Failure(
          new InitializationException(
            s"Provided init data '${invalidData.getClass.getSimpleName}' for load profile service are invalid!"
          )
        )
    }

  override protected def handleRegistrationRequest(
      registrationMessage: ServiceRegistrationMessage
  )(using
      serviceStateData: LoadProfileInitializedStateData,
      ctx: ActorContext[Message],
  ): Try[LoadProfileInitializedStateData] = registrationMessage match {
    case SecondaryServiceRegistrationMessage(
          requestingActor,
          dataTimeType,
          powerProfileKey: PowerProfileKey,
        ) =>
      Success(
        handleRegistrationRequest(
          requestingActor,
          powerProfileKey,
          dataTimeType,
        )
      )
    case invalidMessage =>
      Failure(
        InvalidRegistrationRequestException(
          "Cannot register an agent for load profile service with registration " +
            s"request message '${invalidMessage.getClass.getSimpleName}'!"
        )
      )
  }

  /** Try to register the sending agent with its load profile for load profile
    * value provision.
    *
    * @param agentToBeRegistered
    *   The agent that wants to be registered.
    * @param powerProfileKey
    *   The load profile that the agent wants to receive data for.
    * @param dataTimeType
    *   The data time type that the agent wants to receive data for.
    * @param serviceStateData
    *   The current service state data of this service.
    * @return
    *   An updated state data of this service that contains registration
    *   information if the registration has been carried out successfully.
    */
  private def handleRegistrationRequest(
      agentToBeRegistered: ActorRef[ServiceMessage.Response],
      powerProfileKey: PowerProfileKey,
      dataTimeType: DataTimeType,
  )(using
      serviceStateData: LoadProfileInitializedStateData,
      ctx: ActorContext[Message],
  ): LoadProfileInitializedStateData = {

    getRegistrantsContainer(powerProfileKey) match {
      case Success(registrants) =>
        if registrants.registrantsMap.contains(
            dataTimeType,
            agentToBeRegistered,
          )
        then
          ctx.log.warn(
            "Sending actor {} is already registered",
            agentToBeRegistered,
          )
        else
          agentToBeRegistered ! RegistrationSuccessfulMessage(
            ctx.self,
            FIRST_TICK_IN_SIMULATION,
            serviceStateData.loadProfileStore.getProfileLoadFactoryData(
              powerProfileKey
            ),
          )

        val updatedRegistrants =
          registrants.copy(registrantsMap =
            registrants.registrantsMap.added(dataTimeType, agentToBeRegistered)
          )

        serviceStateData.copy(registeredAgents =
          serviceStateData.registeredAgents
            .updated(powerProfileKey, updatedRegistrants)
        )

      case Failure(exception) =>
        ctx.log.error(
          s"Unable to register for load profile '$powerProfileKey'.",
          exception,
        )

        agentToBeRegistered ! RegistrationFailedMessage(ctx.self)
        serviceStateData
    }

  }

  /** Retrieves or creates the [[RegistrantsContainer]] for given load profile.
    */
  private def getRegistrantsContainer(loadProfile: PowerProfileKey)(using
      serviceStateData: LoadProfileInitializedStateData
  ): Try[RegistrantsContainer] =
    serviceStateData.registeredAgents.get(loadProfile) match {
      case None =>
        if serviceStateData.loadProfileStore.contains(loadProfile) then
          Success(RegistrantsContainer())
        else
          Failure(
            InvalidRegistrationRequestException(
              s"Cannot register an agent for load profile $loadProfile, which is not available!"
            )
          )
      case Some(container) => Success(container)
    }

  override protected def announceInformation(tick: Long)(using
      serviceStateData: LoadProfileInitializedStateData,
      ctx: ActorContext[Message],
  ): (LoadProfileInitializedStateData, Option[Long]) = {
    given ZonedDateTime = serviceStateData.simulationStartTime
    val time = tick.toDateTime

    val loadProfileStore = serviceStateData.loadProfileStore
    val registeredAgents = serviceStateData.registeredAgents

    /* Calculate the next activation ticks */
    val resolutions = serviceStateData.profileResolutions

    val nextActivations = serviceStateData.profileToNextActivationTick

    val activations = nextActivations
      .filter(_._2 == tick)
      .map { case (profile, tick) =>
        profile -> (tick + resolutions(profile))
      }

    activations.foreach { case (loadProfile, nextTick) =>
      registeredAgents.get(loadProfile).foreach { registrantsContainer =>
        def dataRetrievalFunc(time: ZonedDateTime): SecondaryData =
          LoadDataFunction(loadProfileStore.entryFunc(time, loadProfile))

        registrantsContainer.registrantsMap.foreach {
          case (dataTimeType, actors) =>
            val data = dataTimeType match {
              case DataTimeType.Current =>
                dataRetrievalFunc(time)

              case DataTimeType
                    .CurrentAndForecast(forecastLength, forecastResTime) =>
                val profileRes = resolutions(loadProfile)
                val forecastRes = forecastResTime.toSeconds.toLong
                val endTick = tick + forecastLength.toSeconds.toLong

                // if forecast resolution is a multiple of profile resolution,
                // we can use forecast resolution instead
                val adaptedRes =
                  if forecastRes % profileRes == 0 then forecastRes
                  else profileRes

                // profile time series is forwarded as forecast without adding noise
                val series =
                  Range.Long
                    .inclusive(tick, endTick, adaptedRes)
                    .map { tickIt =>
                      val timeIt = tickIt.toDateTime
                      val data = dataRetrievalFunc(timeIt)

                      tickIt -> data
                    }
                    .to(SortedMap)

                SecondarySeriesData(
                  reduceTimeSeriesResolution(series, forecastResTime)
                )

            }

            /* Sending the found value to the requester */
            actors.foreach(
              _ ! DataProvision(
                tick,
                ctx.self,
                data,
                Some(nextTick),
              )
            )
        }
      }

    }

    val updatedActivations = nextActivations ++ activations

    (
      serviceStateData.copy(profileToNextActivationTick = updatedActivations),
      updatedActivations.values.minOption,
    )
  }

}
