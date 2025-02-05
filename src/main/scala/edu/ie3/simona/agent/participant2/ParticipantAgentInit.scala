/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant2

import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.participant2.ParticipantAgent._
import edu.ie3.simona.config.SimonaConfig.BaseRuntimeConfig
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

import java.time.ZonedDateTime

object ParticipantAgentInit {

  /** Container class, that gather together reference to relevant entities, that
    * represent the environment in the simulation
    *
    * @param gridAgent
    *   Reference to the grid agent
    * @param primaryServiceProxy
    *   Reference to the primary service proxy
    * @param services
    *   References to services by service type
    * @param resultListener
    *   Reference to the result listeners
    */
  final case class ParticipantRefs(
      gridAgent: ActorRef[GridAgent.Request],
      primaryServiceProxy: ClassicRef,
      services: Map[ServiceType, ClassicRef],
      resultListener: Iterable[ActorRef[ResultEvent]],
  )

  def apply(
      participantInput: SystemParticipantInput,
      config: BaseRuntimeConfig,
      participantRefs: ParticipantRefs,
      expectedPowerRequestTick: Long,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      parent: Either[ActorRef[SchedulerMessage], ActorRef[FlexResponse]],
  ): Behavior[Request] = Behaviors.setup { ctx =>
    val parentData = parent
      .map { em =>
        val flexAdapter = ctx.messageAdapter[FlexRequest](Flex)

        em ! RegisterControlledAsset(
          participantInput.getUuid,
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
      expectedPowerRequestTick,
      simulationStartDate,
      simulationEndDate,
      parentData,
    )
  }

  private def uninitialized(
      participantInput: SystemParticipantInput,
      config: BaseRuntimeConfig,
      participantRefs: ParticipantRefs,
      expectedPowerRequestTick: Long,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      parentData: Either[SchedulerData, FlexControlledData],
  ): Behavior[Request] = Behaviors.receiveMessagePartial {

    case activation: ActivationRequest if activation.tick == INIT_SIM_TICK =>
      participantRefs.primaryServiceProxy ! PrimaryServiceRegistrationMessage(
        participantInput.getUuid
      )

      waitingForPrimaryProxy(
        participantInput,
        config,
        participantRefs,
        expectedPowerRequestTick,
        simulationStartDate,
        simulationEndDate,
        parentData,
      )

  }

  private def waitingForPrimaryProxy(
      participantInput: SystemParticipantInput,
      config: BaseRuntimeConfig,
      participantRefs: ParticipantRefs,
      expectedPowerRequestTick: Long,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      parentData: Either[SchedulerData, FlexControlledData],
  ): Behavior[Request] = Behaviors.receivePartial {

    case (
          _,
          PrimaryRegistrationSuccessfulMessage(
            serviceRef,
            firstDataTick,
            primaryDataMeta,
          ),
        ) =>
      val expectedFirstData = Map(serviceRef -> firstDataTick)

      completeInitialization(
        ParticipantModelShell.createForPrimaryData(
          participantInput,
          config,
          primaryDataMeta,
          simulationStartDate,
          simulationEndDate,
        ),
        expectedFirstData,
        participantRefs,
        expectedPowerRequestTick,
        parentData,
      )

    case (_, RegistrationFailedMessage(_)) =>
      val modelShell = ParticipantModelShell.createForModel(
        participantInput,
        config,
        simulationStartDate,
        simulationEndDate,
      )

      val requiredServiceTypes = modelShell.requiredServices.toSet

      if (requiredServiceTypes.isEmpty) {
        completeInitialization(
          modelShell,
          Map.empty,
          participantRefs,
          expectedPowerRequestTick,
          parentData,
        )
      } else {

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
          expectedPowerRequestTick,
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

  private def waitingForServices(
      modelShell: ParticipantModelShell[_, _],
      participantRefs: ParticipantRefs,
      expectedPowerRequestTick: Long,
      expectedRegistrations: Set[ClassicRef],
      expectedFirstData: Map[ClassicRef, Long] = Map.empty,
      parentData: Either[SchedulerData, FlexControlledData],
  ): Behavior[Request] =
    Behaviors.receivePartial {
      case (_, RegistrationSuccessfulMessage(serviceRef, nextDataTick)) =>
        if (!expectedRegistrations.contains(serviceRef))
          throw new CriticalFailureException(
            s"${modelShell.identifier}: Registration response from $serviceRef was not expected!"
          )

        val newExpectedRegistrations = expectedRegistrations.excl(serviceRef)
        val newExpectedFirstData =
          expectedFirstData.updated(serviceRef, nextDataTick)

        if (newExpectedRegistrations.isEmpty) {
          completeInitialization(
            modelShell,
            newExpectedFirstData,
            participantRefs,
            expectedPowerRequestTick,
            parentData,
          )
        } else
          waitingForServices(
            modelShell,
            participantRefs,
            expectedPowerRequestTick,
            newExpectedRegistrations,
            newExpectedFirstData,
            parentData,
          )
    }

  /** Completes initialization activation and creates actual
    * [[ParticipantAgent]]
    */
  private def completeInitialization(
      modelShell: ParticipantModelShell[_, _],
      expectedData: Map[ClassicRef, Long],
      participantRefs: ParticipantRefs,
      expectedPowerRequestTick: Long,
      parentData: Either[SchedulerData, FlexControlledData],
  ): Behavior[Request] = {

    val inputHandler = ParticipantInputHandler(expectedData)

    // get first overall activation tick
    val firstTick = inputHandler.getLastActivationTick.orElse(
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
        expectedPowerRequestTick,
      ),
      participantRefs.resultListener,
      parentData,
    )
  }
}
