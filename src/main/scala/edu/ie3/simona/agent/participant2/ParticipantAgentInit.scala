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
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.service.ServiceType
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.{ActorRef => ClassicRef}

import java.time.ZonedDateTime

object ParticipantAgentInit {

  // todo also register with GridAgent,
  // wait for reply and then create
  // GridAdapter

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

        em ! RegisterParticipant(
          participantInput.getUuid,
          flexAdapter,
          participantInput,
        )

        em ! ScheduleFlexRequest(
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
        firstDataTick,
      )

    case (_, RegistrationFailedMessage(_)) =>
      val modelShell = ParticipantModelShell.createForModel(
        participantInput,
        config,
        simulationStartDate,
        simulationEndDate,
      )

      val requiredServiceTypes =
        modelShell.model.getRequiredSecondaryServices.toSet

      if (requiredServiceTypes.isEmpty) {
        // Models that do not use secondary data always start at tick 0
        val firstTick = 0L

        completeInitialization(
          modelShell,
          Map.empty,
          participantRefs,
          expectedPowerRequestTick,
          parentData,
          firstTick,
        )
      } else {
        val requiredServices = requiredServiceTypes.map(serviceType =>
          participantRefs.services.getOrElse(
            serviceType,
            throw new CriticalFailureException(
              s"Service of type $serviceType is not available."
            ),
          )
        )

        waitingForServices(
          modelShell,
          participantRefs,
          expectedPowerRequestTick,
          requiredServices,
          parentData = parentData,
        )
      }
  }

  private def waitingForServices(
      modelShell: ParticipantModelShell[_, _, _],
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
            s"Registration response from $serviceRef was not expected!"
          )

        val newExpectedRegistrations = expectedRegistrations.excl(serviceRef)
        val newExpectedFirstData =
          expectedFirstData.updated(serviceRef, nextDataTick)

        if (newExpectedRegistrations.isEmpty) {
          val firstTick = expectedFirstData
            .map { case (_, nextTick) =>
              nextTick
            }
            .minOption
            .getOrElse(
              throw new CriticalFailureException("No expected data registered.")
            )

          completeInitialization(
            modelShell,
            newExpectedFirstData,
            participantRefs,
            expectedPowerRequestTick,
            parentData,
            firstTick,
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
      modelShell: ParticipantModelShell[_, _, _],
      expectedData: Map[ClassicRef, Long],
      participantRefs: ParticipantRefs,
      expectedPowerRequestTick: Long,
      parentData: Either[SchedulerData, FlexControlledData],
      firstTick: Long,
  ): Behavior[Request] = {

    parentData.fold(
      schedulerData =>
        schedulerData.scheduler ! Completion(
          schedulerData.activationAdapter,
          Some(firstTick),
        ),
      _.emAgent ! FlexCompletion(
        modelShell.model.uuid,
        requestAtNextActivation = false,
        Some(firstTick),
      ),
    )

    ParticipantAgent(
      modelShell,
      ParticipantInputHandler(expectedData),
      ParticipantGridAdapter(
        participantRefs.gridAgent,
        expectedPowerRequestTick,
      ),
      participantRefs.resultListener,
      parentData,
    )
  }
}
