/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant2

import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.agent.participant2.ParticipantAgent._
import edu.ie3.simona.config.SimonaConfig.BaseRuntimeConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant2.{
  ParticipantModel,
  ParticipantModelInit,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexCompletion,
  FlexRequest,
  FlexResponse,
  ProvideFlexOptions,
  RegisterParticipant,
  ScheduleFlexRequest,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.typed.scaladsl.Behaviors
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
      simulationStartDate,
      simulationEndDate,
      parentData,
    )
  }

  private def uninitialized(
      participantInput: SystemParticipantInput,
      config: BaseRuntimeConfig,
      primaryServiceProxy: ClassicRef,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      parentData: Either[SchedulerData, FlexControlledData],
  ): Behavior[Request] = Behaviors.receivePartial {
    case (ctx, activation: ActivationRequest)
        if activation.tick == INIT_SIM_TICK =>
      primaryServiceProxy ! PrimaryServiceRegistrationMessage(
        participantInput.getUuid
      )
      waitingForProxy(
        participantInput,
        config,
        simulationStartDate,
        simulationEndDate,
        parentData,
      )
  }

  private def waitingForProxy(
      participantInput: SystemParticipantInput,
      config: BaseRuntimeConfig,
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

      primaryData()

    case (_, RegistrationFailedMessage(serviceRef)) =>
      val model = ParticipantModelInit.createModel(
        participantInput,
        config.scaling,
        simulationStartDate,
        simulationEndDate,
      )
      val requiredServices = model.getRequiredServices.toSeq
      if (requiredServices.isEmpty) {
        ParticipantAgent(model)
      } else {
        waitingForServices(model)
      }
  }

  private def waitingForServices(
      model: ParticipantModel[_, _, _],
      expectedRegistrations: Set[ClassicRef],
      expectedFirstData: Map[ClassicRef, Long],
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
          expectedFirstData + (serviceRef, nextDataTick)

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
              model.uuid,
              requestAtNextActivation = false,
              earliestNextTick,
            ),
          )

          ParticipantAgent(model, newExpectedFirstData, parentData)
        } else
          waitingForServices(
            model,
            newExpectedRegistrations,
            newExpectedFirstData,
            parentData,
          )
    }

}
