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
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant2.ParticipantModelShell
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage._
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.{ActorRef => ClassicRef}

import java.time.ZonedDateTime

object ParticipantAgentInit {

  // todo also register with GridAgent,
  // wait for reply and then create
  // GridAdapter

  def apply(
      participantInput: SystemParticipantInput,
      config: BaseRuntimeConfig,
      primaryServiceProxy: ClassicRef,
      gridAgentRef: ActorRef[GridAgent.Request],
      expectedPowerRequestTick: Long,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      parent: Either[ActorRef[SchedulerMessage], ActorRef[FlexResponse]],
  ): Behavior[Request] = Behaviors.setup { ctx =>
    val parentData = parent
      .map { parentEm =>
        val flexAdapter = ctx.messageAdapter[FlexRequest](Flex)

        parentEm ! RegisterParticipant(
          participantInput.getUuid,
          flexAdapter,
          participantInput,
        )

        parentEm ! ScheduleFlexRequest(
          participantInput.getUuid,
          INIT_SIM_TICK,
        )

        FlexControlledData(parentEm, flexAdapter)
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
      primaryServiceProxy,
      gridAgentRef,
      expectedPowerRequestTick,
      simulationStartDate,
      simulationEndDate,
      parentData,
    )
  }

  private def uninitialized(
      participantInput: SystemParticipantInput,
      config: BaseRuntimeConfig,
      primaryServiceProxy: ClassicRef,
      gridAgentRef: ActorRef[GridAgent.Request],
      expectedPowerRequestTick: Long,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      parentData: Either[SchedulerData, FlexControlledData],
  ): Behavior[Request] = Behaviors.receiveMessagePartial {

    case activation: ActivationRequest if activation.tick == INIT_SIM_TICK =>
      primaryServiceProxy ! PrimaryServiceRegistrationMessage(
        participantInput.getUuid
      )

      waitingForPrimaryProxy(
        participantInput,
        config,
        gridAgentRef,
        expectedPowerRequestTick,
        simulationStartDate,
        simulationEndDate,
        parentData,
      )

  }

  private def waitingForPrimaryProxy(
      participantInput: SystemParticipantInput,
      config: BaseRuntimeConfig,
      gridAgentRef: ActorRef[GridAgent.Request],
      expectedPowerRequestTick: Long,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      parentData: Either[SchedulerData, FlexControlledData],
  ): Behavior[Request] = Behaviors.receivePartial {

    case (_, RegistrationSuccessfulMessage(serviceRef, firstDataTick)) =>
      val expectedFirstData = Map(serviceRef -> firstDataTick)

      completeInitialization(
        ParticipantModelShell.createForPrimaryData(
          participantInput,
          config,
          simulationStartDate,
          simulationEndDate,
        ),
        expectedFirstData,
        gridAgentRef,
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
        modelShell.model.getRequiredSecondaryServices.toSeq

      if (requiredServiceTypes.isEmpty) {
        // Models that do not use secondary data always start at tick 0
        val firstTick = 0L

        completeInitialization(
          modelShell,
          Map.empty,
          gridAgentRef,
          expectedPowerRequestTick,
          parentData,
          firstTick,
        )
      } else {
        // TODO request service actorrefs
        val requiredServices = ???

        waitingForServices(
          modelShell,
          gridAgentRef,
          expectedPowerRequestTick,
          requiredServices,
          parentData = parentData,
        )
      }
  }

  private def waitingForServices(
      modelShell: ParticipantModelShell[_, _, _],
      gridAgentRef: ActorRef[GridAgent.Request],
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
            gridAgentRef,
            expectedPowerRequestTick,
            parentData,
            firstTick,
          )
        } else
          waitingForServices(
            modelShell,
            gridAgentRef,
            expectedPowerRequestTick,
            newExpectedRegistrations,
            newExpectedFirstData,
            parentData,
          )
    }

  /** Completes initialization activation and creates actual
    * [[ParticipantAgent]]
    *
    * @param modelShell
    * @param expectedData
    * @param gridAgentRef
    * @param expectedPowerRequestTick
    * @param parentData
    * @param firstTick
    * @return
    */
  private def completeInitialization(
      modelShell: ParticipantModelShell[_, _, _],
      expectedData: Map[ClassicRef, Long],
      gridAgentRef: ActorRef[GridAgent.Request],
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
      ParticipantGridAdapter(gridAgentRef, expectedPowerRequestTick),
      parentData,
    )
  }
}
