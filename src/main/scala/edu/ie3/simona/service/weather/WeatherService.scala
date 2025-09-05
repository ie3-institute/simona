/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.weather

import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.ParticipantAgent.{
  DataProvision,
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.config.InputConfig
import edu.ie3.simona.exceptions.InitializationException
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceBaseStateData,
}
import edu.ie3.simona.service.SimonaService
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  SecondaryServiceRegistrationMessage,
  ServiceRegistrationMessage,
}
import edu.ie3.simona.service.Data.SecondaryData.WeatherSeriesData
import edu.ie3.simona.service.weather.WeatherSource.WeightedCoordinates
import edu.ie3.simona.util.{Coordinate, SimonaConstants}
import edu.ie3.simona.util.TickUtil.RichZonedDateTime
import edu.ie3.util.scala.collection.immutable.SortedDistinctSeq
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
    * @param dataType
    *   The type of weather data to register for.
    */
  final case class WeatherRegistrationData(
      coordinate: Coordinate,
      dataType: WeatherDataType,
  )

  /** Container storing registered actors for a coordinate.
    *
    * @param registeredActors
    *   A map of weather data type to registered actors.
    * @param coordinateWeights
    *   Weights mapping surrounding coordinates onto the registered coordinate.
    */
  final case class CoordinateData(
      registeredActors: Map[WeatherDataType, Set[
        ActorRef[ParticipantAgent.Request]
      ]],
      coordinateWeights: WeightedCoordinates,
  )

  /** State data of an initialized weather service.
    *
    * @param weatherSource
    *   The weather source to retrieve information from.
    * @param coordinateData
    *   A map of the requested coords to their receiving actor references.
    * @param maybeNextActivationTick
    *   The next tick at which the service wants to be activated.
    * @param activationTicks
    *   A sorted set of ticks, that yet have been sent to the scheduler (w\o
    *   next tick).
    */
  final case class WeatherBaseStateData(
      weatherSource: WeatherSource,
      coordinateData: Map[Coordinate, CoordinateData] = Map.empty,
      maybeNextActivationTick: Option[Long],
      activationTicks: SortedDistinctSeq[Long] = SortedDistinctSeq.empty,
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
        val (maybeNextTick, furtherActivationTicks) = SortedDistinctSeq(
          weatherSource
            .getDataTicks(
              SimonaConstants.FIRST_TICK_IN_SIMULATION,
              simulationEnd.toTick,
            )
            .toSeq
        ).pop

        val weatherInitializedStateData = WeatherBaseStateData(
          weatherSource,
          activationTicks = furtherActivationTicks,
          maybeNextActivationTick = maybeNextTick,
        )

        Success(
          weatherInitializedStateData,
          maybeNextTick,
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
            WeatherRegistrationData(coordinate, dataType),
          ) =>
        Success(
          handleRegistrationRequest(
            agentToBeRegistered,
            coordinate,
            dataType,
          )
        )
      case invalidMessage =>
        Failure(
          InvalidRegistrationRequestException(
            "Cannot register an agent for weather service with registration " +
              s"request message '${invalidMessage.getClass.getSimpleName}'!"
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
    * @param dataType
    *   The weather data type that the agent wants to receive.
    * @param serviceStateData
    *   The current service state data of this service.
    * @return
    *   An updated state data of this service that contains registration
    *   information if the registration has been carried out successfully.
    */
  private def handleRegistrationRequest(
      agentToBeRegistered: ActorRef[ParticipantAgent.Request],
      coordinate: Coordinate,
      dataType: WeatherDataType,
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

    // collate the provided coordinates into a single entity
    val registrationResponse = serviceStateData.maybeNextActivationTick
      .map(RegistrationSuccessfulMessage(ctx.self, _))
      .getOrElse(RegistrationFailedMessage(ctx.self))

    serviceStateData.coordinateData.get(coordinate) match {
      case None =>
        /* The coordinate itself is not known yet. Try to figure out, which weather coordinates are relevant */
        serviceStateData.weatherSource.getWeightedCoordinates(
          coordinate,
          serviceStateData.amountOfInterpolationCoords,
        ) match {
          case Success(weightedCoordinates) =>
            agentToBeRegistered ! registrationResponse

            val coordinateData = CoordinateData(
              registeredActors = Map(dataType -> Set(agentToBeRegistered)),
              coordinateWeights = weightedCoordinates,
            )

            /* Enhance the mapping from agent coordinate to requesting actor's ActorRef as well as the necessary
             * weather coordinates for later averaging. */
            serviceStateData.copy(
              coordinateData =
                serviceStateData.coordinateData + (coordinate -> coordinateData)
            )
          case Failure(exception) =>
            ctx.log.error(
              s"Unable to obtain necessary information to register for coordinate $coordinate.",
              exception,
            )
            agentToBeRegistered ! RegistrationFailedMessage(ctx.self)
            serviceStateData
        }

      case Some(coordinateData) =>
        val registeredActors =
          coordinateData.registeredActors.getOrElse(dataType, Set.empty)

        if registeredActors.contains(agentToBeRegistered) then
          ctx.log.warn(
            "Sending actor {} is already registered",
            agentToBeRegistered,
          )
        else agentToBeRegistered ! registrationResponse

        val adaptedCoordinateData = coordinateData.copy(registeredActors =
          coordinateData.registeredActors +
            (dataType -> registeredActors.incl(agentToBeRegistered))
        )

        serviceStateData.copy(
          coordinateData =
            serviceStateData.coordinateData + (coordinate -> adaptedCoordinateData)
        )

    }
  }

  override protected def announceInformation(tick: Long)(using
      serviceStateData: WeatherBaseStateData,
      ctx: ActorContext[Message],
  ): (WeatherBaseStateData, Option[Long]) = {

    /* Pop the next activation tick and update the state data */
    val (
      maybeNextTick: Option[Long],
      updatedStateData: WeatherBaseStateData,
    ) = {
      val (nextTick, remainderTicks) = serviceStateData.activationTicks.pop
      (nextTick, serviceStateData.copy(activationTicks = remainderTicks))
    }

    // get the weather and send it to the subscribed agents
    // no sanity check needed here as we can assume that we always have weather available
    // when we announce it. Otherwise, the registration would have failed already!
    updatedStateData.coordinateData.foreach { case (_, coordinateData) =>
      val coordinateWeights = coordinateData.coordinateWeights

      coordinateData.registeredActors.foreach { case (dataType, actors) =>
        val weatherData = dataType match {
          case WeatherDataType.Current =>
            updatedStateData.weatherSource.getWeather(tick, coordinateWeights)
          case WeatherDataType.CurrentAndForecast(length, interval) =>
            val endTick = tick + length.toSeconds.toLong
            // weather time series is forwarded without adding error
            val series = updatedStateData.weatherSource.getWeather(
              tick,
              endTick,
              coordinateWeights,
            )
            WeatherSeriesData(series)
        }

        actors.foreach {
          _ ! DataProvision(
            tick,
            ctx.self,
            weatherData,
            maybeNextTick,
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
