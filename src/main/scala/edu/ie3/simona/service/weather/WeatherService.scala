/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.weather

import edu.ie3.simona.config.InputConfig
import edu.ie3.simona.exceptions.InitializationException
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.ontology.messages.ServiceMessage
import edu.ie3.simona.ontology.messages.ServiceMessage.*
import edu.ie3.simona.service.Data.SecondaryData.SecondarySeriesData
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceBaseStateData,
}
import edu.ie3.simona.service.{DataTimeType, SimonaService}
import edu.ie3.simona.service.weather.WeatherSource.WeightedCoordinates
import edu.ie3.simona.util.TickUtil.toTick
import edu.ie3.simona.util.{Coordinate, SimonaConstants}
import edu.ie3.util.scala.collection.immutable.ActivationTickQueue
import edu.ie3.util.scala.collection.immutable.RichMultiMap.*
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.slf4j.Logger

import java.time.ZonedDateTime
import scala.util.{Failure, Success, Try}

/** Weather Service is responsible to register other actors that require weather
  * information and provide weather information when requested
  *
  * @version 0.1
  * @since 2019-07-28
  */
object WeatherService extends SimonaService {

  override type S = WeatherBaseStateData

  /** Data required for registration with the weather service.
    *
    * @param coordinate
    *   The coordinate to register weather data for.
    * @param registrantKey
    *   An optional key that is echoed back in [[DataProvision]] messages,
    *   allowing the registrant to attribute data to a specific registration.
    */
  final case class WeatherRegistrationData(
      coordinate: Coordinate,
      registrantKey: Option[String] = None,
  )

  /** Container storing registered actors for a coordinate.
    *
    * @param registrantsMap
    *   A map of data time type to registered actors and their optional keys.
    * @param coordinateWeights
    *   Weights mapping surrounding coordinates onto the registered coordinate.
    */
  final case class RegistrantsContainer(
      registrantsMap: Map[DataTimeType, Map[ActorRef[
        ServiceMessage.Response
      ], Option[String]]],
      coordinateWeights: WeightedCoordinates,
  )

  /** State data of an initialized weather service.
    *
    * @param weatherSource
    *   The weather source to retrieve information from.
    * @param registeredAgents
    *   A map of the requested coords to their receiving actor references.
    * @param activationTicks
    *   A queue of future ticks that the service will be activated at.
    * @param startDateTime
    *   The simulation time at which simulation started.
    * @param amountOfInterpolationCoords
    *   The amount of coordinates to be interpolated for any agent coordinate.
    */
  final case class WeatherBaseStateData(
      weatherSource: WeatherSource,
      registeredAgents: Map[Coordinate, RegistrantsContainer] = Map.empty,
      activationTicks: ActivationTickQueue,
      startDateTime: ZonedDateTime,
      amountOfInterpolationCoords: Int = 4,
  ) extends ServiceBaseStateData

  /** Weather service state data used for initialization of the weather service
    *
    * @param sourceDefinition
    *   the definition of the source to use
    */
  final case class InitWeatherServiceStateData(
      sourceDefinition: InputConfig.WeatherDatasource,
      startDateTime: ZonedDateTime,
      simulationEnd: ZonedDateTime,
  ) extends InitializeServiceStateData

  override def init(
      initServiceData: InitializeServiceStateData
  )(using log: Logger): Try[(WeatherBaseStateData, Option[Long])] =
    initServiceData match {
      case InitWeatherServiceStateData(
            sourceDefinition,
            startDateTime,
            simulationEnd,
          ) =>
        given simulationStart: ZonedDateTime = startDateTime

        val weatherSource = WeatherSource(sourceDefinition)

        /* What is the first tick to be triggered for? And what are further activation ticks */
        val activationTicks = ActivationTickQueue(
          weatherSource
            .getDataTicks(
              SimonaConstants.FIRST_TICK_IN_SIMULATION,
              simulationEnd.toTick,
            )
            .toSeq
        )

        val weatherInitializedStateData = WeatherBaseStateData(
          weatherSource,
          activationTicks = activationTicks,
          startDateTime = startDateTime,
        )

        Success(
          weatherInitializedStateData,
          activationTicks.nextTick,
        )

      case invalidData =>
        Failure(
          new InitializationException(
            s"Provided init data '${invalidData.getClass.getSimpleName}' for weather service are invalid!"
          )
        )
    }

  override def handleRegistrationRequest(
      registrationMessage: ServiceRegistrationMessage
  )(using
      serviceStateData: WeatherBaseStateData,
      ctx: ActorContext[Message],
  ): Try[WeatherBaseStateData] =
    registrationMessage match {
      case SecondaryServiceRegistrationMessage(
            agentToBeRegistered,
            dataTimeType,
            WeatherRegistrationData(coordinate, registrantKey),
          ) =>
        Success(
          handleRegistrationRequest(
            agentToBeRegistered,
            coordinate,
            dataTimeType,
            registrantKey,
          )
        )
      case invalidMessage =>
        Failure(
          InvalidRegistrationRequestException(
            "Cannot register an agent for weather service with registration " +
              s"request message '$invalidMessage'!"
          )
        )
    }

  /** Try to register the sending agent with its coordinate and weather data
    * type for weather provision.
    *
    * @param agentToBeRegistered
    *   The agent that wants to be registered.
    * @param coordinate
    *   The coordinate of the agent to be registered.
    * @param dataTimeType
    *   The data time type that the agent wants to receive data for.
    * @param registrantKey
    *   Optional key for per-registration attribution in DataProvision.
    * @param serviceStateData
    *   The current service state data of this service.
    * @param ctx
    *   The actor context.
    * @return
    *   An updated state data of this service that contains registration
    *   information if the registration has been carried out successfully.
    */
  private def handleRegistrationRequest(
      agentToBeRegistered: ActorRef[ServiceMessage.Response],
      coordinate: Coordinate,
      dataTimeType: DataTimeType,
      registrantKey: Option[String] = None,
  )(using
      serviceStateData: WeatherBaseStateData,
      ctx: ActorContext[Message],
  ): WeatherBaseStateData = {
    ctx.log.debug(
      "Received weather registration from {} for [Lat:{}, Long:{}]",
      agentToBeRegistered.path.name,
      coordinate.latitude,
      coordinate.longitude,
    )

    getRegistrantsContainer(coordinate) match {
      case Success(registrants) =>
        val existingAgents =
          registrants.registrantsMap.getOrElse(dataTimeType, Map.empty)
        if existingAgents.contains(agentToBeRegistered)
        then
          ctx.log.warn(
            "Sending actor {} is already registered",
            agentToBeRegistered,
          )
        else {
          val registrationResponse = serviceStateData.activationTicks.nextTick
            .map(RegistrationSuccessfulMessage(ctx.self, _))
            .getOrElse(RegistrationFailedMessage(ctx.self))

          agentToBeRegistered ! registrationResponse
        }

        val updatedRegistrants =
          registrants.copy(registrantsMap =
            registrants.registrantsMap.updated(
              dataTimeType,
              existingAgents + (agentToBeRegistered -> registrantKey),
            )
          )

        serviceStateData.copy(registeredAgents =
          serviceStateData.registeredAgents
            .updated(coordinate, updatedRegistrants)
        )
      case Failure(exception) =>
        ctx.log.error(
          s"Unable to register for coordinate $coordinate.",
          exception,
        )
        agentToBeRegistered ! RegistrationFailedMessage(ctx.self)
        serviceStateData
    }
  }

  /** Retrieves or creates the [[RegistrantsContainer]] for given coordinate.
    */
  private def getRegistrantsContainer(coordinate: Coordinate)(using
      serviceStateData: WeatherBaseStateData
  ): Try[RegistrantsContainer] =
    serviceStateData.registeredAgents.get(coordinate) match {
      case None =>
        serviceStateData.weatherSource
          .getWeightedCoordinates(
            coordinate,
            serviceStateData.amountOfInterpolationCoords,
          )
          .transform(
            weightedCoordinates =>
              Success(
                RegistrantsContainer(
                  registrantsMap = Map.empty,
                  coordinateWeights = weightedCoordinates,
                )
              ),
            exception =>
              Failure(
                InvalidRegistrationRequestException(
                  s"Unable to obtain necessary information to register for coordinate $coordinate.",
                  exception,
                )
              ),
          )
      case Some(container) => Success(container)
    }

  override protected def announceInformation(tick: Long)(using
      serviceStateData: WeatherBaseStateData,
      ctx: ActorContext[Message],
  ): (WeatherBaseStateData, Option[Long]) = {

    given simulationStart: ZonedDateTime = serviceStateData.startDateTime

    /* Pop the next activation tick and update the state data */
    val remainingTicks = serviceStateData.activationTicks.dropFirst
    val maybeNextTick = remainingTicks.nextTick
    val updatedStateData =
      serviceStateData.copy(activationTicks = remainingTicks)

    // get the weather and send it to the subscribed agents
    // no sanity check needed here as we can assume that we always have weather available
    // when we announce it. Otherwise, the registration would have failed already!
    updatedStateData.registeredAgents.foreach { case (_, container) =>
      val coordinateWeights = container.coordinateWeights

      container.registrantsMap.foreach { case (dataTimeType, actors) =>
        val weatherData = dataTimeType match {
          case DataTimeType.Current =>
            updatedStateData.weatherSource.getWeather(tick, coordinateWeights)
          case DataTimeType.CurrentAndForecast(length, resolution) =>
            val endTick = tick + length.toSeconds.toLong
            // weather time series is forwarded as forecast without adding noise
            val series = updatedStateData.weatherSource
              .getWeather(
                tick,
                endTick,
                coordinateWeights,
              )
              .map { case (time, data) =>
                time.toTick -> data
              }
            SecondarySeriesData(reduceTimeSeriesResolution(series, resolution))
        }

        actors.foreach { case (actor, key) =>
          actor ! DataProvision(
            tick,
            ctx.self,
            weatherData,
            maybeNextTick,
            key,
          )
        }
      }
    }

    (
      updatedStateData,
      maybeNextTick,
    )
  }

}
