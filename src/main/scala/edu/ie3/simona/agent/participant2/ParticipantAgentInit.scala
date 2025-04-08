/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant2

import edu.ie3.datamodel.models.input.system.{LoadInput, SystemParticipantInput}
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.InputModelContainer
import edu.ie3.simona.agent.participant2.ParticipantAgent._
import edu.ie3.simona.config.RuntimeConfig.BaseRuntimeConfig
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant2.ParticipantModel.{
  AdditionalFactoryData,
  ModelState,
  ParticipantModelFactory,
}
import edu.ie3.simona.model.participant2.{
  ParticipantModelInit,
  ParticipantModelShell,
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage._
import edu.ie3.simona.ontology.messages.services.LoadProfileMessage.RegisterForLoadProfileService
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  PrimaryServiceRegistrationMessage,
  RegisterForEvDataMessage,
}
import edu.ie3.simona.ontology.messages.services.WeatherMessage.RegisterForWeatherMessage
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.service.ServiceType
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
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
      primaryServiceProxy: ActorRef[ServiceMessage],
      services: Map[ServiceType, ActorRef[_ >: ServiceMessage]],
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
    * @param inputContainer
    *   The input container holding the system participant model input that
    *   represents the physical model at the core of the agent.
    * @param runtimeConfig
    *   Runtime configuration that has to match the participant type.
    * @param notifierConfig
    *   The result configuration.
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
      inputContainer: InputModelContainer[_ <: SystemParticipantInput],
      runtimeConfig: BaseRuntimeConfig,
      notifierConfig: NotifierConfig,
      participantRefs: ParticipantRefs,
      simulationParams: SimulationParameters,
      parent: Either[ActorRef[SchedulerMessage], ActorRef[FlexResponse]],
      scheduleKey: ScheduleKey,
  ): Behavior[Request] = Behaviors.setup { ctx =>
    val parentData = parent
      .map { em =>
        val flexAdapter = ctx.messageAdapter[FlexRequest](Flex)

        em ! RegisterControlledAsset(
          flexAdapter,
          inputContainer.electricalInputModel,
        )

        em ! ScheduleFlexActivation(
          inputContainer.electricalInputModel.getUuid,
          INIT_SIM_TICK,
          Some(scheduleKey),
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
            Some(scheduleKey),
          )

          SchedulerData(scheduler, activationAdapter)
        }
      }

    uninitialized(
      inputContainer,
      runtimeConfig,
      notifierConfig,
      participantRefs,
      simulationParams,
      parentData,
    )
  }

  /** Waiting for an [[Activation]] message to start the initialization.
    */
  private def uninitialized(
      inputContainer: InputModelContainer[_ <: SystemParticipantInput],
      runtimeConfig: BaseRuntimeConfig,
      notifierConfig: NotifierConfig,
      participantRefs: ParticipantRefs,
      simulationParams: SimulationParameters,
      parentData: Either[SchedulerData, FlexControlledData],
  ): Behavior[Request] = Behaviors.receivePartial {

    case (ctx, activation: ActivationRequest)
        if activation.tick == INIT_SIM_TICK =>
      // first, check whether we're just supposed to replay primary data time series
      participantRefs.primaryServiceProxy ! PrimaryServiceRegistrationMessage(
        ctx.self,
        inputContainer.electricalInputModel.getUuid,
      )

      waitingForPrimaryProxy(
        inputContainer,
        runtimeConfig,
        notifierConfig,
        participantRefs,
        simulationParams,
        parentData,
      )

  }

  /** Waits for the primary proxy to respond, which decides whether this
    * participant uses model calculations or just replays primary data.
    */
  private def waitingForPrimaryProxy(
      inputContainer: InputModelContainer[_ <: SystemParticipantInput],
      runtimeConfig: BaseRuntimeConfig,
      notifierConfig: NotifierConfig,
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
      val expectedFirstData: Map[ActorRef[_ >: ServiceMessage], Long] =
        Map(serviceRef -> firstDataTick)

      completeInitialization(
        ParticipantModelInit.getPrimaryModelFactory(
          inputContainer,
          runtimeConfig,
          primaryDataExtra,
        ),
        inputContainer.electricalInputModel,
        notifierConfig,
        expectedFirstData,
        participantRefs,
        simulationParams,
        parentData,
      )

    case (ctx, RegistrationFailedMessage(_)) =>
      // we're _not_ supposed to replay primary data, thus initialize the physical model
      val modelFactory = ParticipantModelInit.getPhysicalModelFactory(
        inputContainer,
        runtimeConfig,
      )
      val requiredServiceTypes = modelFactory.getRequiredSecondaryServices

      if (requiredServiceTypes.isEmpty) {
        // not requiring any secondary services, thus we're ready to go
        completeInitialization(
          modelFactory,
          inputContainer.electricalInputModel,
          notifierConfig,
          Map.empty,
          participantRefs,
          simulationParams,
          parentData,
        )
      } else {
        // requiring at least one secondary service, thus send out registrations and wait for replies
        val requiredServices = requiredServiceTypes
          .map(serviceType =>
            serviceType -> participantRefs.services
              .getOrElse(
                serviceType,
                throw new CriticalFailureException(
                  s"${inputContainer.electricalInputModel.identifier}: Service of type $serviceType is not available."
                ),
              )
          )
          .toMap

        requiredServices.foreach { case (serviceType, serviceRef) =>
          registerForService(
            inputContainer.electricalInputModel,
            ctx.self,
            serviceType,
            serviceRef,
          )
        }

        waitingForServices(
          modelFactory,
          inputContainer.electricalInputModel,
          notifierConfig,
          participantRefs,
          simulationParams,
          requiredServices.values.toSet,
          parentData = parentData,
        )
      }
  }

  private def registerForService(
      participantInput: SystemParticipantInput,
      participantRef: ActorRef[Request],
      serviceType: ServiceType,
      serviceRef: ActorRef[_ >: ServiceMessage],
  ): Unit =
    serviceType match {
      case ServiceType.WeatherService =>
        val geoPosition = participantInput.getNode.getGeoPosition

        Option(geoPosition.getY).zip(Option(geoPosition.getX)) match {
          case Some((lat, lon)) =>
            serviceRef ! RegisterForWeatherMessage(participantRef, lat, lon)
          case _ =>
            throw new CriticalFailureException(
              s"${participantInput.identifier} cannot register for weather information at " +
                s"node ${participantInput.getNode.getId} (${participantInput.getNode.getUuid}), " +
                s"because the geo position (${geoPosition.getY}, ${geoPosition.getX}) is invalid."
            )
        }

      case ServiceType.PriceService =>
        throw new CriticalFailureException(
          s"${participantInput.identifier} is trying to register for a ${ServiceType.PriceService}, " +
            s"which is currently not supported."
        )

      case ServiceType.EvMovementService =>
        serviceRef ! RegisterForEvDataMessage(
          participantRef,
          participantInput.getUuid,
        )

      case ServiceType.LoadProfileService =>
        participantInput match {
          case load: LoadInput =>
            serviceRef ! RegisterForLoadProfileService(
              participantRef,
              load.getLoadProfile,
            )

          case _ =>
            throw new CriticalFailureException(
              s"${participantInput.identifier} cannot register for load profile service!"
            )
        }
    }

  /** Waiting for replies from secondary services. If all replies have been
    * received, we complete the initialization.
    */
  private def waitingForServices(
      modelFactory: ParticipantModelFactory[_ <: ModelState],
      participantInput: SystemParticipantInput,
      notifierConfig: NotifierConfig,
      participantRefs: ParticipantRefs,
      simulationParams: SimulationParameters,
      expectedRegistrations: Set[ActorRef[_ >: ServiceMessage]],
      expectedFirstData: Map[ActorRef[_ >: ServiceMessage], Long] = Map.empty,
      parentData: Either[SchedulerData, FlexControlledData],
  ): Behavior[Request] =
    Behaviors.receivePartial {
      case (
            _,
            RegistrationSuccessfulMessage(
              serviceRef,
              nextDataTick,
              additionalData,
            ),
          ) =>
        // received registration success message from secondary service
        if (!expectedRegistrations.contains(serviceRef))
          throw new CriticalFailureException(
            s"${participantInput.identifier}: Registration response from $serviceRef was not expected!"
          )

        val newExpectedRegistrations = expectedRegistrations.excl(serviceRef)
        val newExpectedFirstData =
          expectedFirstData.updated(serviceRef, nextDataTick)

        val updatedFactory = additionalData match {
          case Some(data: AdditionalFactoryData) => modelFactory.update(data)
          case None                              => modelFactory
        }

        if (newExpectedRegistrations.isEmpty)
          // all secondary services set up, ready to go
          completeInitialization(
            updatedFactory,
            participantInput,
            notifierConfig,
            newExpectedFirstData,
            participantRefs,
            simulationParams,
            parentData,
          )
        else
          // there's at least one more service to go, let's wait for confirmation
          waitingForServices(
            updatedFactory,
            participantInput,
            notifierConfig,
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
      modelFactory: ParticipantModelFactory[_ <: ModelState],
      participantInput: SystemParticipantInput,
      notifierConfig: NotifierConfig,
      expectedData: Map[ActorRef[_ >: ServiceMessage], Long],
      participantRefs: ParticipantRefs,
      simulationParams: SimulationParameters,
      parentData: Either[SchedulerData, FlexControlledData],
  ): Behavior[Request] = {

    val modelShell = ParticipantModelShell.create(
      modelFactory,
      participantInput.getOperationTime,
      simulationParams.simulationStart,
      simulationParams.simulationEnd,
    )

    val inputHandler = ParticipantInputHandler(expectedData)

    val firstTick = modelShell.operationStart
    val dataCompletedTick = inputHandler.getDataCompletedTick

    dataCompletedTick.foreach { dataCompleted =>
      if (dataCompleted > firstTick)
        throw new CriticalFailureException(
          s"${modelShell.identifier}: Input data will only be fully received at tick $dataCompleted. " +
            s"It needs to be available with operation start $firstTick though."
        )
    }

    parentData.fold(
      schedulerData =>
        schedulerData.scheduler ! Completion(
          schedulerData.activationAdapter,
          Some(firstTick),
        ),
      _.emAgent ! FlexCompletion(
        modelShell.uuid,
        requestAtNextActivation = false,
        Some(firstTick),
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
      ParticipantResultHandler(
        participantRefs.resultListener,
        notifierConfig,
      ),
      parentData,
    )
  }

  implicit class RichSystemParticipantInput(spi: SystemParticipantInput) {

    def identifier: String =
      s"${spi.getClass.getSimpleName}[${spi.getId}/${spi.getUuid}]"
  }
}
