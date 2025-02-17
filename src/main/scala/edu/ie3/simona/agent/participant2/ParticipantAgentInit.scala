/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant2

import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.participant2.ParticipantAgent._
import edu.ie3.simona.config.RuntimeConfig.BaseRuntimeConfig
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant2.ParticipantModelShell
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage._
import edu.ie3.simona.ontology.messages.services.EvMessage.RegisterForEvDataMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.WeatherMessage.RegisterForWeatherMessage
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.service.ServiceType
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.{ActorRef => ClassicRef}
import squants.Dimensionless

import java.time.ZonedDateTime

/** This class helps collect all information required for the initialization of
  * a [[ParticipantAgent]]. When initialization succeeds, a [[ParticipantAgent]]
  * behavior is returned, having the first simulation activation already
  * scheduled with the corresponding [[edu.ie3.simona.scheduler.Scheduler]] or
  * [[edu.ie3.simona.agent.em.EmAgent]].
  */
object ParticipantAgentInit {

  /** Container class that gathers references to relevant actors.
    *
    * @param gridAgent
    *   Reference to the grid agent.
    * @param primaryServiceProxy
    *   Reference to the primary service proxy.
    * @param services
    *   References to services by service type.
    * @param resultListener
    *   Reference to the result listeners.
    */
  final case class ParticipantRefs(
      gridAgent: ActorRef[GridAgent.Request],
      primaryServiceProxy: ClassicRef,
      services: Map[ServiceType, ClassicRef],
      resultListener: Iterable[ActorRef[ResultEvent]],
  )

  /** Container class that holds parameters related to the simulation.
    *
    * @param expectedPowerRequestTick
    *   The tick at which the first power request message is expected from
    *   [[GridAgent]].
    * @param requestVoltageDeviationTolerance
    *   The voltage request deviation tolerance, outside which reactive power
    *   has to be recalculated.
    * @param simulationStart
    *   The simulation start date and time.
    * @param simulationEnd
    *   The simulation end date and time.
    */
  final case class SimulationParameters(
      expectedPowerRequestTick: Long,
      requestVoltageDeviationTolerance: Dimensionless,
      simulationStart: ZonedDateTime,
      simulationEnd: ZonedDateTime,
  )

  /** Starts the initialization process of a [[ParticipantAgent]].
    *
    * @param participantInput
    *   The system participant model input that represents the physical model at
    *   the core of the agent.
    * @param config
    *   Runtime configuration that has to match the participant type.
    * @param participantRefs
    *   A collection of actor references to actors required for initialization
    *   and operation.
    * @param simulationParams
    *   Some parameters required for simulation.
    * @param parent
    *   The parent actor scheduling or controlling this participant, i.e. either
    *   a [[edu.ie3.simona.scheduler.Scheduler]] or an
    *   [[edu.ie3.simona.agent.em.EmAgent]].
    */
  def apply(
      participantInput: SystemParticipantInput,
      config: BaseRuntimeConfig,
      participantRefs: ParticipantRefs,
      simulationParams: SimulationParameters,
      parent: Either[ActorRef[SchedulerMessage], ActorRef[FlexResponse]],
  ): Behavior[Request] = Behaviors.setup { ctx =>
    val parentData = parent
      .map { em =>
        val flexAdapter = ctx.messageAdapter[FlexRequest](Flex)

        em ! RegisterControlledAsset(
          flexAdapter,
          participantInput,
        )

        em ! ScheduleFlexActivation(
          participantInput.getUuid,
          INIT_SIM_TICK,
        )

        FlexControlledData(em, flexAdapter)
      }
      .left
      .map { scheduler =>
        {
          val activationAdapter = ctx.messageAdapter[Activation] { msg =>
            ParticipantActivation(msg.tick)
          }

          scheduler ! ScheduleActivation(
            activationAdapter,
            INIT_SIM_TICK,
          )

          SchedulerData(scheduler, activationAdapter)
        }
      }

    uninitialized(
      participantInput,
      config,
      participantRefs,
      simulationParams,
      parentData,
    )
  }

  /** Waiting for an [[Activation]] message to start the initialization.
    */
  private def uninitialized(
      participantInput: SystemParticipantInput,
      config: BaseRuntimeConfig,
      participantRefs: ParticipantRefs,
      simulationParams: SimulationParameters,
      parentData: Either[SchedulerData, FlexControlledData],
  ): Behavior[Request] = Behaviors.receiveMessagePartial {

    case activation: ActivationRequest if activation.tick == INIT_SIM_TICK =>
      // first, check whether we're just supposed to replay primary data time series
      participantRefs.primaryServiceProxy ! PrimaryServiceRegistrationMessage(
        participantInput.getUuid
      )

      waitingForPrimaryProxy(
        participantInput,
        config,
        participantRefs,
        simulationParams,
        parentData,
      )

  }

  /** Waits for the primary proxy to respond, which decides whether this
    * participant uses model calculations or just replays primary data.
    */
  private def waitingForPrimaryProxy(
      participantInput: SystemParticipantInput,
      config: BaseRuntimeConfig,
      participantRefs: ParticipantRefs,
      simulationParams: SimulationParameters,
      parentData: Either[SchedulerData, FlexControlledData],
  ): Behavior[Request] = Behaviors.receivePartial {

    case (
          _,
          PrimaryRegistrationSuccessfulMessage(
            serviceRef,
            firstDataTick,
            primaryDataExtra,
          ),
        ) =>
      // we're supposed to replay primary data, initialize accordingly
      val expectedFirstData = Map(serviceRef -> firstDataTick)

      completeInitialization(
        ParticipantModelShell.createForPrimaryData(
          participantInput,
          config,
          primaryDataExtra,
          simulationParams.simulationStart,
          simulationParams.simulationEnd,
        ),
        expectedFirstData,
        participantRefs,
        simulationParams,
        parentData,
      )

    case (_, RegistrationFailedMessage(_)) =>
      // we're _not_ supposed to replay primary data, thus initialize the physical model
      val modelShell = ParticipantModelShell.createForPhysicalModel(
        participantInput,
        config,
        simulationParams.simulationStart,
        simulationParams.simulationEnd,
      )

      val requiredServiceTypes = modelShell.requiredServices.toSet

      if (requiredServiceTypes.isEmpty) {
        // not requiring any secondary services, thus we're ready to go
        completeInitialization(
          modelShell,
          Map.empty,
          participantRefs,
          simulationParams,
          parentData,
        )
      } else {
        // requiring at least one secondary service, thus send out registrations and wait for replies
        val requiredServices = requiredServiceTypes
          .map(serviceType =>
            serviceType -> participantRefs.services.getOrElse(
              serviceType,
              throw new CriticalFailureException(
                s"${modelShell.identifier}: Service of type $serviceType is not available."
              ),
            )
          )
          .toMap

        requiredServices.foreach { case (serviceType, serviceRef) =>
          registerForService(
            participantInput,
            modelShell,
            serviceType,
            serviceRef,
          )
        }

        waitingForServices(
          modelShell,
          participantRefs,
          simulationParams,
          requiredServices.values.toSet,
          parentData = parentData,
        )
      }
  }

  private def registerForService(
      participantInput: SystemParticipantInput,
      modelShell: ParticipantModelShell[_, _],
      serviceType: ServiceType,
      serviceRef: ClassicRef,
  ): Unit =
    serviceType match {
      case ServiceType.WeatherService =>
        val geoPosition = participantInput.getNode.getGeoPosition

        Option(geoPosition.getY).zip(Option(geoPosition.getX)) match {
          case Some((lat, lon)) =>
            serviceRef ! RegisterForWeatherMessage(lat, lon)
          case _ =>
            throw new CriticalFailureException(
              s"${modelShell.identifier} cannot register for weather information at " +
                s"node ${participantInput.getNode.getId} (${participantInput.getNode.getUuid}), " +
                s"because the geo position (${geoPosition.getY}, ${geoPosition.getX}) is invalid."
            )
        }

      case ServiceType.PriceService =>
        throw new CriticalFailureException(
          s"${modelShell.identifier} is trying to register for a ${ServiceType.PriceService}, " +
            s"which is currently not supported."
        )

      case ServiceType.EvMovementService =>
        serviceRef ! RegisterForEvDataMessage(modelShell.uuid)
    }

  /** Waiting for replies from secondary services. If all replies have been
    * received, we complete the initialization.
    */
  private def waitingForServices(
      modelShell: ParticipantModelShell[_, _],
      participantRefs: ParticipantRefs,
      simulationParams: SimulationParameters,
      expectedRegistrations: Set[ClassicRef],
      expectedFirstData: Map[ClassicRef, Long] = Map.empty,
      parentData: Either[SchedulerData, FlexControlledData],
  ): Behavior[Request] =
    Behaviors.receivePartial {
      case (_, RegistrationSuccessfulMessage(serviceRef, nextDataTick)) =>
        // received registration success message from secondary service
        if (!expectedRegistrations.contains(serviceRef))
          throw new CriticalFailureException(
            s"${modelShell.identifier}: Registration response from $serviceRef was not expected!"
          )

        val newExpectedRegistrations = expectedRegistrations.excl(serviceRef)
        val newExpectedFirstData =
          expectedFirstData.updated(serviceRef, nextDataTick)

        if (newExpectedRegistrations.isEmpty)
          // all secondary services set up, ready to go
          completeInitialization(
            modelShell,
            newExpectedFirstData,
            participantRefs,
            simulationParams,
            parentData,
          )
        else
          // there's at least one more service to go, let's wait for confirmation
          waitingForServices(
            modelShell,
            participantRefs,
            simulationParams,
            newExpectedRegistrations,
            newExpectedFirstData,
            parentData,
          )
    }

  /** Completes initialization, sends a completion message and creates actual
    * [[ParticipantAgent]]
    */
  private def completeInitialization(
      modelShell: ParticipantModelShell[_, _],
      expectedData: Map[ClassicRef, Long],
      participantRefs: ParticipantRefs,
      simulationParams: SimulationParameters,
      parentData: Either[SchedulerData, FlexControlledData],
  ): Behavior[Request] = {

    val inputHandler = ParticipantInputHandler(expectedData)

    // get first overall activation tick
    val firstTick = inputHandler.getDataCompletedTick.orElse(
      modelShell
        .getChangeIndicator(currentTick = -1, None)
        .changesAtTick
    )

    if (firstTick.isEmpty)
      throw new CriticalFailureException(
        s"${modelShell.identifier}: No new first activation tick determined with expected data $expectedData"
      )

    parentData.fold(
      schedulerData =>
        schedulerData.scheduler ! Completion(
          schedulerData.activationAdapter,
          firstTick,
        ),
      _.emAgent ! FlexCompletion(
        modelShell.uuid,
        requestAtNextActivation = false,
        firstTick,
      ),
    )

    ParticipantAgent(
      modelShell,
      inputHandler,
      ParticipantGridAdapter(
        participantRefs.gridAgent,
        simulationParams.expectedPowerRequestTick,
        simulationParams.requestVoltageDeviationTolerance,
      ),
      participantRefs.resultListener,
      parentData,
    )
  }
}
