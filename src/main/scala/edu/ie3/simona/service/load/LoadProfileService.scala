/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.load

import edu.ie3.datamodel.models.profile.LoadProfile
import edu.ie3.simona.agent.participant2.ParticipantAgent
import edu.ie3.simona.agent.participant2.ParticipantAgent.{
  DataProvision,
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.config.InputConfig.LoadProfile.Datasource
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.exceptions.{
  CriticalFailureException,
  InitializationException,
}
import edu.ie3.simona.ontology.messages.services.LoadProfileMessage.{
  LoadData,
  RegisterForLoadProfileService,
}
import edu.ie3.simona.ontology.messages.services.{
  LoadProfileMessage,
  ServiceMessage,
}
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceActivationBaseStateData,
}
import edu.ie3.simona.service.SimonaService
import edu.ie3.simona.util.TickUtil.{RichZonedDateTime, TickLong}
import edu.ie3.simona.util.{SimonaConstants, TickUtil}
import edu.ie3.util.scala.collection.immutable.SortedDistinctSeq
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext

import java.time.ZonedDateTime
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.{Failure, Success, Try}

/** Load Profile Service is responsible to register other actors that require
  * load profile information and provide load profile time series information
  * when requested
  */
object LoadProfileService extends SimonaService[LoadProfileMessage] {

  override type S = LoadProfileInitializedStateData

  /** @param loadProfileStore
    *   That stores that contains all load profiles.
    * @param profileToRefs
    *   Map: actor ref to [[LoadProfile]].
    * @param maybeNextActivationTick
    *   The next tick, when this actor is triggered by scheduler.
    * @param activationTicks
    *   Sorted set of ticks, that yet have been sent to the scheduler (w\o next
    *   tick).
    */
  final case class LoadProfileInitializedStateData(
      loadProfileStore: LoadProfileStore,
      profileToRefs: Map[LoadProfile, Vector[
        ActorRef[ParticipantAgent.Request]
      ]] = Map.empty,
      override val maybeNextActivationTick: Option[Long],
      override val activationTicks: SortedDistinctSeq[Long],
      simulationStartTime: ZonedDateTime,
  ) extends ServiceActivationBaseStateData

  /** Load profile service state data used for initialization of the load
    * profile sources.
    *
    * @param sourceDefinition
    *   The definition of additional sources. If no definition is given, only
    *   the build in load profiles can be used.
    * @param simulationStartTime
    *   The time the simulation is started.
    * @param simulationEnd
    *   The time the simulation ends.
    * @param resolution
    *   The resolution used for the load profiles.
    */
  final case class InitLoadProfileServiceStateData(
      sourceDefinition: Datasource,
      simulationStartTime: ZonedDateTime,
      simulationEnd: ZonedDateTime,
      resolution: FiniteDuration = 15.minutes,
  ) extends InitializeServiceStateData

  /** Initialize the concrete service implementation using the provided
    * initialization data. This method should perform all heavyweight tasks
    * before the actor becomes ready. The return values are a) the state data of
    * the initialized service and b) optional triggers that should be send to
    * the [[edu.ie3.simona.scheduler.Scheduler]] together with the completion
    * message that is send in response to the trigger that is send to start the
    * initialization process
    *
    * @param initServiceData
    *   The data that should be used for initialization.
    * @return
    *   The state data of this service and optional tick that should be included
    *   in the completion message.
    */
  override def init(
      initServiceData: InitializeServiceStateData
  ): Try[(LoadProfileInitializedStateData, Option[Long])] =
    initServiceData match {
      case InitLoadProfileServiceStateData(
            dataSource,
            simulationStartTime,
            simulationEnd,
            resolution,
          ) =>
        implicit val simulationStart: ZonedDateTime = simulationStartTime

        val loadProfileStore = LoadProfileStore(dataSource)

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
          simulationStartTime = simulationStartTime,
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
      serviceStateData: LoadProfileInitializedStateData,
      ctx: ActorContext[LoadProfileMessage],
  ): Try[LoadProfileInitializedStateData] = registrationMessage match {
    case RegisterForLoadProfileService(requestingActor, loadProfile) =>
      Success(handleRegistrationRequest(requestingActor, loadProfile))
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
      agentToBeRegistered: ActorRef[ParticipantAgent.Request],
      loadProfile: LoadProfile,
  )(implicit
      serviceStateData: LoadProfileInitializedStateData,
      ctx: ActorContext[LoadProfileMessage],
  ): LoadProfileInitializedStateData = {

    serviceStateData.profileToRefs.get(loadProfile) match {
      case None =>
        /* The load profile itself is not known yet. Try to figure out, which weather coordinates are relevant */

        if (serviceStateData.loadProfileStore.contains(loadProfile)) {
          // we can provide data for the agent
          agentToBeRegistered ! RegistrationSuccessfulMessage(
            ctx.self,
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
          ctx.log.error(
            s"Unable to obtain necessary information to register for load profile '${loadProfile.getKey}'."
          )

          agentToBeRegistered ! RegistrationFailedMessage(ctx.self)
          serviceStateData
        }

      case Some(actorRefs) if !actorRefs.contains(agentToBeRegistered) =>
        // load profile is already known (= we have data for it), but this actor is not registered yet
        agentToBeRegistered ! RegistrationSuccessfulMessage(
          ctx.self,
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
        ctx.log.warn(
          "Sending actor {} is already registered",
          agentToBeRegistered,
        )
        serviceStateData

      case _ =>
        // actor is not registered and we don't have data for it
        // inform the agentToBeRegistered that the registration failed as we don't have data for it
        agentToBeRegistered ! RegistrationFailedMessage(ctx.self)
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
      ctx: ActorContext[LoadProfileMessage],
  ): (LoadProfileInitializedStateData, Option[Long]) = {

    /* Pop the next activation tick and update the state data */
    val (
      maybeNextTick: Option[Long],
      updatedStateData: LoadProfileInitializedStateData,
    ) = {
      val (nextTick, remainderTicks) = serviceStateData.activationTicks.pop
      (nextTick, serviceStateData.copy(activationTicks = remainderTicks))
    }

    val time = tick.toDateTime(serviceStateData.simulationStartTime)
    val loadProfileStore = serviceStateData.loadProfileStore

    serviceStateData.profileToRefs.foreach { case (loadProfile, actorRefs) =>
      loadProfileStore.entry(time, loadProfile) match {
        case Some(averagePower) =>
          /* Sending the found value to the requester */
          actorRefs.foreach(recipient =>
            recipient ! DataProvision(
              tick,
              ctx.self,
              LoadData(averagePower),
              maybeNextTick,
            )
          )
        case None =>
          /* There is no data available in the source. */
          ctx.log.warn(
            s"No power value found for load profile {} for time: {}",
            loadProfile,
            time,
          )
      }
    }

    (updatedStateData, maybeNextTick)
  }

}
