/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import edu.ie3.simona.agent.participant.ParticipantAgent.{
  Flex,
  FlexControlledData,
  ParticipantActivation,
  Request,
  SchedulerData,
}
import edu.ie3.simona.model.participant.ParticipantModelShell
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexRequest,
  FlexResponse,
}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.typed.scaladsl.Behaviors

object ParticipantAgentMockFactory {

  /** Creates a [[ParticipantAgent]] behavior with given parameters. This detour
    * is needed because normally, [[ParticipantAgentInit]] creates adapters that
    * are handed over to [[ParticipantAgent]].
    */
  def create(
      modelShell: ParticipantModelShell[_, _],
      inputHandler: ParticipantInputHandler,
      gridAdapter: ParticipantGridAdapter,
      resultHandler: ParticipantResultHandler,
      parent: Either[
        (ActorRef[SchedulerMessage], ActorRef[ActorRef[Activation]]),
        (ActorRef[FlexResponse], ActorRef[ActorRef[FlexRequest]]),
      ],
  ): Behavior[Request] = Behaviors.setup { ctx =>
    val parentData = parent
      .map { case (em, adapterReply) =>
        val flexAdapter = ctx.messageAdapter[FlexRequest](Flex)
        adapterReply ! flexAdapter
        FlexControlledData(em, flexAdapter)
      }
      .left
      .map { case (scheduler, adapterReply) =>
        val activationAdapter = ctx.messageAdapter[Activation] { msg =>
          ParticipantActivation(msg.tick)
        }
        adapterReply ! activationAdapter
        SchedulerData(scheduler, activationAdapter)
      }

    ParticipantAgent(
      modelShell,
      inputHandler,
      gridAdapter,
      resultHandler,
      parentData,
    )
  }
}
