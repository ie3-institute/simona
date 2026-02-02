/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.load

import edu.ie3.datamodel.models.profile.LoadProfile
import edu.ie3.simona.config.InputConfig.LoadProfile.Datasource
import edu.ie3.simona.exceptions.InitializationException
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.ontology.messages.ServiceMessage
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  DataProvision,
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
  SecondaryServiceRegistrationMessage,
  ServiceRegistrationMessage,
}
import edu.ie3.simona.service.Data.SecondaryData.{LoadData, LoadDataFunction}
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceBaseStateData,
}
import edu.ie3.simona.service.{DataTimeType, SimonaService}
import edu.ie3.simona.util.SimonaConstants.FIRST_TICK_IN_SIMULATION
import edu.ie3.simona.util.TickUtil.TickLong
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.slf4j.Logger

import java.time.ZonedDateTime
import scala.util.{Failure, Success, Try}

/** Load Profile Service is responsible to register other actors that require
  * load profile information and provide load profile time series information
  * when requested
  */
object LoadProfileService extends SimonaService {

  override type S = LoadProfileInitializedStateData

  /** @param loadProfileStore
    *   That stores that contains all load profiles.
    * @param profileToRefs
    *   Map: actor ref to [[LoadProfile]].
    * @param profileResolutions
    *   Map: [[LoadProfile]] to resolution.
    * @param profileToNextActivationTick
    *   Map: [[LoadProfile]] to next activation tick..
    * @param simulationStartTime
    *   Start of the simulation.
    */
  final case class LoadProfileInitializedStateData(
      loadProfileStore: LoadProfileStore,
      profileToRefs: Map[LoadProfile, Seq[ActorRef[ServiceMessage.Response]]] =
        Map.empty,
      profileResolutions: Map[LoadProfile, Long],
      profileToNextActivationTick: Map[LoadProfile, Long],
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
          dataType,
          loadProfile: LoadProfile,
        ) =>
      if dataType != DataTimeType.Current then
        ctx.log.warn(s"Data time type $dataType is currently not supported.")

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
    * value provision.
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
      agentToBeRegistered: ActorRef[ServiceMessage.Response],
      loadProfile: LoadProfile,
  )(using
      serviceStateData: LoadProfileInitializedStateData,
      ctx: ActorContext[Message],
  ): LoadProfileInitializedStateData = {

    serviceStateData.profileToRefs.get(loadProfile) match {
      case None =>
        /* The load profile itself is not known yet. Try to figure out, which load profile is relevant */

        if serviceStateData.loadProfileStore.contains(loadProfile) then {
          // we can provide data for the agent
          agentToBeRegistered ! RegistrationSuccessfulMessage(
            ctx.self,
            FIRST_TICK_IN_SIMULATION,
            serviceStateData.loadProfileStore.getProfileLoadFactoryData(
              loadProfile
            ),
          )

          serviceStateData.copy(profileToRefs =
            serviceStateData.profileToRefs + (loadProfile -> Seq(
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
          FIRST_TICK_IN_SIMULATION,
          serviceStateData.loadProfileStore.getProfileLoadFactoryData(
            loadProfile
          ),
        )

        serviceStateData.copy(
          profileToRefs =
            serviceStateData.profileToRefs + (loadProfile -> (actorRefs :+ agentToBeRegistered))
        )

      case _ =>
        // actor is already registered, do nothing
        ctx.log.warn(
          "Sending actor {} is already registered",
          agentToBeRegistered,
        )
        serviceStateData
    }
  }

  override protected def announceInformation(tick: Long)(using
      serviceStateData: LoadProfileInitializedStateData,
      ctx: ActorContext[Message],
  ): (LoadProfileInitializedStateData, Option[Long]) = {
    given ZonedDateTime = serviceStateData.simulationStartTime
    val time = tick.toDateTime

    val loadProfileStore = serviceStateData.loadProfileStore
    val profileToRefs = serviceStateData.profileToRefs

    /* Calculate the next activation ticks */
    val resolutions = serviceStateData.profileResolutions

    val nextActivations = serviceStateData.profileToNextActivationTick

    val activations = nextActivations
      .filter(_._2 == tick)
      .map { case (profile, tick) =>
        profile -> (tick + resolutions(profile))
      }

    activations.foreach { case (loadProfile, nextTick) =>
      profileToRefs.get(loadProfile).foreach { actorRefs =>
        loadProfile match {
          case LoadProfile.RandomLoadProfile.RANDOM_LOAD_PROFILE =>
            val loadFunction = loadProfileStore.randomEntrySupplier(time)

            /* Providing random load value function to the requester */
            actorRefs.foreach(recipient =>
              recipient ! DataProvision(
                tick,
                ctx.self,
                LoadDataFunction(loadFunction),
                Some(nextTick),
              )
            )

          case _ =>
            loadProfileStore.entry(time, loadProfile) match {
              case Some(averagePower) =>
                /* Sending the found value to the requester */
                actorRefs.foreach(recipient =>
                  recipient ! DataProvision(
                    tick,
                    ctx.self,
                    LoadData(averagePower),
                    Some(nextTick),
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
      }

    }

    val updatedActivations = nextActivations ++ activations

    (
      serviceStateData.copy(profileToNextActivationTick = updatedActivations),
      updatedActivations.values.minOption,
    )
  }

}
