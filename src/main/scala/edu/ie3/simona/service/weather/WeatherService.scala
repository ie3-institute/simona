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

  override type S = WeatherInitializedStateData

  final case class WeatherRegistrationData(
      coordinate: Coordinate,
      dataType: WeatherDataType,
  )

  final case class CoordinateData(
      registeredActors: Map[ActorRef[
        ParticipantAgent.Request
      ], WeatherDataType],
      coordinateWeights: WeightedCoordinates,
  )

  /** @param weatherSource
    *   weather source to receive information from
    * @param coordinateDataMap
    *   mapping of the requested coords to their receiving actor references
    * @param maybeNextActivationTick
    *   the next tick, when this actor is triggered by scheduler
    * @param activationTicks
    *   sorted set of ticks, that yet have been sent to the scheduler (w\o next
    *   tick)
    */
  final case class WeatherInitializedStateData(
      weatherSource: WeatherSource,
      coordinateDataMap: Map[Coordinate, CoordinateData] = Map.empty,
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
  )(using log: Logger): Try[(WeatherInitializedStateData, Option[Long])] =
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

        val weatherInitializedStateData = WeatherInitializedStateData(
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
  override def handleRegistrationRequest(
      registrationMessage: ServiceRegistrationMessage
  )(using
      serviceStateData: WeatherInitializedStateData,
      ctx: ActorContext[Message],
  ): Try[WeatherInitializedStateData] =
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

  /** Try to register the sending agent with its latitude and longitude values
    * for weather provision.
    *
    * @param agentToBeRegistered
    *   The agent that wants to be registered.
    * @param coordinate
    *   The coordinate of the agent to be registered.
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
      serviceStateData: WeatherInitializedStateData,
      ctx: ActorContext[Message],
  ): WeatherInitializedStateData = {
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

    serviceStateData.coordinateDataMap.get(coordinate) match {
      case None =>
        /* The coordinate itself is not known yet. Try to figure out, which weather coordinates are relevant */
        serviceStateData.weatherSource.getWeightedCoordinates(
          coordinate,
          serviceStateData.amountOfInterpolationCoords,
        ) match {
          case Success(weightedCoordinates) =>
            agentToBeRegistered ! registrationResponse

            val coordinateData = CoordinateData(
              registeredActors = Map(agentToBeRegistered -> dataType),
              coordinateWeights = weightedCoordinates,
            )

            /* Enhance the mapping from agent coordinate to requesting actor's ActorRef as well as the necessary
             * weather coordinates for later averaging. */
            serviceStateData.copy(
              coordinateDataMap =
                serviceStateData.coordinateDataMap + (coordinate -> coordinateData)
            )
          case Failure(exception) =>
            ctx.log.error(
              s"Unable to obtain necessary information to register for coordinate $coordinate.",
              exception,
            )
            agentToBeRegistered ! RegistrationFailedMessage(ctx.self)
            serviceStateData
        }

      case Some(coordinateData)
          if !coordinateData.registeredActors.contains(agentToBeRegistered) =>
        // coordinate is already known (= we have data for it), but this actor is not registered yet
        agentToBeRegistered ! registrationResponse

        val adaptedCoordinateData = coordinateData.copy(registeredActors =
          coordinateData.registeredActors + (agentToBeRegistered -> dataType)
        )

        serviceStateData.copy(
          coordinateDataMap =
            serviceStateData.coordinateDataMap + (coordinate -> adaptedCoordinateData)
        )

      case Some(_) =>
        // actor is already registered, do nothing
        ctx.log.warn(
          "Sending actor {} is already registered",
          agentToBeRegistered,
        )
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
    *   with updated values) together with the completion message that is sent
    *   in response to the trigger that was sent to start this announcement
    */
  override protected def announceInformation(tick: Long)(using
      serviceStateData: WeatherInitializedStateData,
      ctx: ActorContext[Message],
  ): (WeatherInitializedStateData, Option[Long]) = {

    /* Pop the next activation tick and update the state data */
    val (
      maybeNextTick: Option[Long],
      updatedStateData: WeatherInitializedStateData,
    ) = {
      val (nextTick, remainderTicks) = serviceStateData.activationTicks.pop
      (nextTick, serviceStateData.copy(activationTicks = remainderTicks))
    }

    val coordinateWeights = updatedStateData.coordinateDataMap.map {
      case (coordinate, CoordinateData(_, weights)) =>
        coordinate -> weights
    }

    // get the weather and send it to the subscribed agents
    // no sanity check needed here as we can assume that we always have weather available
    // when we announce it. Otherwise, the registration would have failed already!
    updatedStateData.weatherSource
      .getWeather(tick, coordinateWeights)
      .foreach { case coordinate -> weatherResult =>
        updatedStateData.coordinateDataMap
          .get(coordinate)
          .foreach { case CoordinateData(actors, _) =>
            actors.keys.foreach {
              _ ! DataProvision(
                tick,
                ctx.self,
                weatherResult,
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
