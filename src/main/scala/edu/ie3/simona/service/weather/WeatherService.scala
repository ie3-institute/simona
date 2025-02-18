/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.weather

import edu.ie3.simona.agent.participant2.ParticipantAgent.{DataProvision, RegistrationFailedMessage, RegistrationSuccessfulMessage}
import edu.ie3.simona.config.InputConfig
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.exceptions.{CriticalFailureException, InitializationException}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.WeatherMessage._
import edu.ie3.simona.service.ServiceStateData.{InitializeServiceStateData, ServiceActivationBaseStateData}
import edu.ie3.simona.service.SimonaService
import edu.ie3.simona.service.weather.WeatherService.{InitWeatherServiceStateData, WeatherInitializedStateData}
import edu.ie3.simona.service.weather.WeatherSource.{AgentCoordinates, WeightedCoordinates}
import edu.ie3.simona.util.SimonaConstants
import edu.ie3.simona.util.TickUtil.RichZonedDateTime
import edu.ie3.util.scala.collection.immutable.SortedDistinctSeq
import org.apache.pekko.actor.{ActorContext, ActorRef, Props}

import java.time.ZonedDateTime
import scala.util.{Failure, Success, Try}

object WeatherService {

  def props(
      scheduler: ActorRef,
      startDateTime: ZonedDateTime,
      simulationEnd: ZonedDateTime,
      amountOfInterpolationCoordinates: Int = 4,
  ): Props =
    Props(
      new WeatherService(
        scheduler,
        startDateTime,
        simulationEnd,
        amountOfInterpolationCoordinates,
      )
    )

  /** @param weatherSource
    *   weather source to receive information from
    * @param coordsToActorRefMap
    *   mapping of the requested coords to their receiving actor references
    * @param weightedWeatherCoordinates
    *   Maps the requested agent coordinates onto the surrounding weather
    *   coordinates, including their weighting
    * @param maybeNextActivationTick
    *   the next tick, when this actor is triggered by scheduler
    * @param activationTicks
    *   sorted set of ticks, that yet have been sent to the scheduler (w\o next
    *   tick)
    */
  final case class WeatherInitializedStateData(
      weatherSource: WeatherSource,
      coordsToActorRefMap: Map[AgentCoordinates, Vector[ActorRef]] =
        Map.empty[AgentCoordinates, Vector[ActorRef]],
      weightedWeatherCoordinates: Map[AgentCoordinates, WeightedCoordinates] =
        Map.empty[AgentCoordinates, WeightedCoordinates],
      override val maybeNextActivationTick: Option[Long],
      override val activationTicks: SortedDistinctSeq[Long] =
        SortedDistinctSeq.empty,
  ) extends ServiceActivationBaseStateData

  /** Weather service state data used for initialization of the weather service
    *
    * @param sourceDefinition
    *   the definition of the source to use
    */
  final case class InitWeatherServiceStateData(
      sourceDefinition: InputConfig.WeatherDatasource
  ) extends InitializeServiceStateData

  val FALLBACK_WEATHER_STEM_DISTANCE = 3600L
}

/** Weather Service is responsible to register other actors that require weather
  * information and provide weather information when requested
  *
  * @version 0.1
  * @since 2019-07-28
  */
final case class WeatherService(
    override val scheduler: ActorRef,
    private implicit val simulationStart: ZonedDateTime,
    simulationEnd: ZonedDateTime,
    private val amountOfInterpolationCoords: Int,
) extends SimonaService[
      WeatherInitializedStateData
    ](scheduler) {

  /** Initialize the concrete service implementation using the provided
    * initialization data. This method should perform all heavyweight tasks
    * before the actor becomes ready. The return values are a) the state data of
    * the initialized service and b) optional triggers that should be sent to
    * the [[edu.ie3.simona.scheduler.Scheduler]] together with the completion
    * message that is sent in response to the trigger that is sent to start the
    * initialization process
    *
    * @param initServiceData
    *   the data that should be used for initialization
    * @return
    *   the state data of this service and optional triggers that should be
    *   included in the completion message
    */
  override def init(
      initServiceData: InitializeServiceStateData
  ): Try[(WeatherInitializedStateData, Option[Long])] =
    initServiceData match {
      case InitWeatherServiceStateData(sourceDefinition) =>
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
  )(implicit
      serviceStateData: WeatherInitializedStateData
  ): Try[WeatherInitializedStateData] =
    registrationMessage match {
      case RegisterForWeatherMessage(latitude, longitude) =>
        Success(handleRegistrationRequest(sender(), latitude, longitude))
      case invalidMessage =>
        Failure(
          InvalidRegistrationRequestException(
            "Cannot register an agent for weather service with registration " +
              s"request message '${invalidMessage.getClass.getSimpleName}'!"
          )
        )
    }

  /** Try to register the sending agent with its latitude and longitude values
    * for weather provision
    *
    * @param agentToBeRegistered
    *   the agent that wants to be registered
    * @param latitude
    *   the latitude of this agent
    * @param longitude
    *   the longitude of this agent
    * @param serviceStateData
    *   the current service state data of this service
    * @return
    *   an updated state data of this service that contains registration
    *   information if the registration has been carried out successfully
    */
  private def handleRegistrationRequest(
      agentToBeRegistered: ActorRef,
      latitude: Double,
      longitude: Double,
  )(implicit
      serviceStateData: WeatherInitializedStateData
  ): WeatherInitializedStateData = {
    log.debug(
      "Received weather registration from {} for [Lat:{}, Long:{}]",
      agentToBeRegistered.path.name,
      latitude,
      longitude,
    )

    // collate the provided coordinates into a single entity
    val agentCoord = AgentCoordinates(
      latitude,
      longitude,
    )

    serviceStateData.coordsToActorRefMap.get(agentCoord) match {
      case None =>
        /* The coordinate itself is not known yet. Try to figure out, which weather coordinates are relevant */
        serviceStateData.weatherSource.getWeightedCoordinates(
          agentCoord,
          amountOfInterpolationCoords,
        ) match {
          case Success(weightedCoordinates) =>
            agentToBeRegistered ! RegistrationSuccessfulMessage(
              self,
              serviceStateData.maybeNextActivationTick.getOrElse(
                throw new CriticalFailureException(
                  "No first data tick for weather service"
                )
              ),
            )

            /* Enhance the mapping from agent coordinate to requesting actor's ActorRef as well as the necessary
             * weather coordinates for later averaging. */
            serviceStateData.copy(
              coordsToActorRefMap =
                serviceStateData.coordsToActorRefMap + (agentCoord -> Vector(
                  agentToBeRegistered
                )),
              weightedWeatherCoordinates =
                serviceStateData.weightedWeatherCoordinates + (agentCoord -> weightedCoordinates),
            )
          case Failure(exception) =>
            log.error(
              exception,
              s"Unable to obtain necessary information to register for coordinate $agentCoord.",
            )
            sender() ! RegistrationFailedMessage(self)
            serviceStateData
        }

      case Some(actorRefs) if !actorRefs.contains(agentToBeRegistered) =>
        // coordinate is already known (= we have data for it), but this actor is not registered yet
        agentToBeRegistered ! RegistrationSuccessfulMessage(
          self,
          serviceStateData.maybeNextActivationTick.getOrElse(
            throw new CriticalFailureException(
              "No first data tick for weather service"
            )
          ),
        )

        serviceStateData.copy(
          coordsToActorRefMap =
            serviceStateData.coordsToActorRefMap + (agentCoord -> (actorRefs :+ agentToBeRegistered))
        )

      case Some(actorRefs) if actorRefs.contains(agentToBeRegistered) =>
        // actor is already registered, do nothing
        log.warning(
          "Sending actor {} is already registered",
          agentToBeRegistered,
        )
        serviceStateData

      case _ =>
        // actor is not registered, and we don't have data for it
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
    *   with updated values) together with the completion message that is sent
    *   in response to the trigger that was sent to start this announcement
    */
  override protected def announceInformation(tick: Long)(implicit
      serviceStateData: WeatherInitializedStateData,
      ctx: ActorContext,
  ): (WeatherInitializedStateData, Option[Long]) = {

    /* Pop the next activation tick and update the state data */
    val (
      maybeNextTick: Option[Long],
      updatedStateData: WeatherInitializedStateData,
    ) = {
      val (nextTick, remainderTicks) = serviceStateData.activationTicks.pop
      (nextTick, serviceStateData.copy(activationTicks = remainderTicks))
    }

    // get the weather and send it to the subscribed agents
    // no sanity check needed here as we can assume that we always have weather available
    // when we announce it. Otherwise, the registration would have failed already!
    updatedStateData.weatherSource
      .getWeather(tick, updatedStateData.weightedWeatherCoordinates)
      .foreach { case coordinate -> weatherResult =>
        updatedStateData.coordsToActorRefMap
          .get(coordinate)
          .foreach(recipients =>
            recipients.foreach(
              _ ! DataProvision(
                tick,
                self,
                weatherResult,
                maybeNextTick,
              )
            )
          )
      }

    (
      updatedStateData,
      maybeNextTick,
    )
  }

}
