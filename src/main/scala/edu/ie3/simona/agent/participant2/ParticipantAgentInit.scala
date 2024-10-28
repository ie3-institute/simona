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

    case (_, RegistrationSuccessfulMessage(serviceRef, nextDataTick)) =>
      parentData.fold(
        schedulerData =>
          schedulerData.scheduler ! Completion(
            schedulerData.activationAdapter,
            Some(nextDataTick),
          ),
        _.emAgent ! FlexCompletion(
          participantInput.getUuid,
          requestAtNextActivation = false,
          Some(nextDataTick),
        ),
      )

      val expectedFirstData = Map(serviceRef -> nextDataTick)

      createAgent(
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
        createAgent(
          modelShell,
          Map.empty,
          gridAgentRef,
          expectedPowerRequestTick,
          parentData,
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
          val earliestNextTick = expectedFirstData.map { case (_, nextTick) =>
            nextTick
          }.minOption

          parentData.fold(
            schedulerData =>
              schedulerData.scheduler ! Completion(
                schedulerData.activationAdapter,
                earliestNextTick,
              ),
            _.emAgent ! FlexCompletion(
              modelShell.model.uuid,
              requestAtNextActivation = false,
              earliestNextTick,
            ),
          )

          createAgent(
            modelShell,
            newExpectedFirstData,
            gridAgentRef,
            expectedPowerRequestTick,
            parentData,
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

  def createAgent(
      modelShell: ParticipantModelShell[_, _, _],
      expectedData: Map[ClassicRef, Long],
      gridAgentRef: ActorRef[GridAgent.Request],
      expectedPowerRequestTick: Long,
      parentData: Either[SchedulerData, FlexControlledData],
  ): Behavior[Request] =
    ParticipantAgent(
      modelShell,
      ParticipantInputHandler(expectedData),
      ParticipantGridAdapter(gridAgentRef, expectedPowerRequestTick),
      parentData,
    )
}
