/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.em

import edu.ie3.datamodel.models.input.EmInput
import edu.ie3.simona.agent.em.EmAgent.{EmData, Message}
import edu.ie3.simona.agent.{DataInputHandler, SecondaryServiceRegistration}
import edu.ie3.simona.config.RuntimeConfig.EmRuntimeConfig
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.model.em.EmModelShell
import edu.ie3.simona.model.participant.ParticipantModel.AdditionalFactoryData
import edu.ie3.simona.ontology.messages.ServiceMessage.EmServiceRegistration
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexResponse,
  RegisterControlledAsset,
}
import edu.ie3.simona.ontology.messages.{SchedulerMessage, ServiceMessage}
import edu.ie3.simona.service.ServiceType
import edu.ie3.simona.service.em.ExtEmDataService
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

import java.time.ZonedDateTime
import scala.jdk.OptionConverters.RichOptional

/** This class helps collect all information required for the setup of an
  * [[EmAgent]]. When this succeeds, an [[EmAgent]] behavior is returned,
  * waiting for the activation message starting initialization of connected
  * asset agents.
  */
object EmAgentInit extends SecondaryServiceRegistration[Message, Unit] {

  /** Creates the initial [[Behavior]] for an [[EmAgent]].
    *
    * @param inputModel
    *   The model for this agent.
    * @param modelConfig
    *   Configuration for this type of model.
    * @param outputConfig
    *   Config for the output behavior of simulation results.
    * @param simulationStartDate
    *   Date of the very first tick in the simulation.
    * @param parent
    *   Either a [[Right]] with a reference to the parent [[EmAgent]] if this
    *   agent is em-controlled, or a [[Left]] with a reference to the scheduler
    *   that is activating this agent.
    * @param services
    *   References to services by service type.
    * @param listener
    *   A listener for result events.
    * @param emDataService
    *   An energy management service.
    */
  def apply(
      inputModel: EmInput,
      modelConfig: EmRuntimeConfig,
      outputConfig: NotifierConfig,
      simulationStartDate: ZonedDateTime,
      parent: Either[ActorRef[SchedulerMessage], ActorRef[FlexResponse]],
      services: Map[ServiceType, ActorRef[ServiceMessage]] = Map.empty,
      listener: ActorRef[ResultEvent],
      emDataService: Option[ActorRef[ExtEmDataService.Message]] = None,
  ): Behavior[Message] = Behaviors.setup[Message] { ctx =>

    val parentData = emDataService match {
      case Some(service) =>
        // since we have a service, it will replace the default agent communication
        given ActorContext[Message] = ctx

        val uuid = inputModel.getUuid

        // given to the parent
        val requestAdapter = ExtEmDataService.emServiceRequestAdapter(
          service,
          ctx.self,
        )

        val adaptedParent = parent match {
          case Left(_) =>
            uuid
          case Right(value) =>
            value
        }

        // used by this agent
        val responseAdapter = ExtEmDataService.emServiceResponseAdapter(
          service,
          adaptedParent,
        )

        parent.map {
          _ ! RegisterControlledAsset(
            requestAdapter,
            inputModel,
          )
        }

        service ! EmServiceRegistration(
          ctx.self,
          uuid,
          parent.toOption,
          inputModel.getControllingEm.toScala.map(_.getUuid),
        )

        Right(responseAdapter)

      case None =>
        parent.map {
          _ ! RegisterControlledAsset(
            ctx.self,
            inputModel,
          )
        }

        parent
    }

    val constantData = EmData(
      outputConfig,
      simulationStartDate,
      parentData,
      listener,
      emDataService
    )

    val modelShell = EmModelShell(
      inputModel.getUuid,
      inputModel.getId,
      inputModel.getControlStrategy,
      modelConfig,
    )

    // Since we do not expect any additional data, create a dummy consumer
    val dummyFactoryUpdater: AdditionalDataConsumer =
      new AdditionalDataConsumer {

        override def update(
            data: AdditionalFactoryData
        ): Unit = ()

        override def unchanged: Unit = ()
      }

    val completionBehavior =
      (mf: Unit, expectedServices: Map[ActorRef[ServiceMessage], Long]) =>
        EmAgent.inactive(
          constantData,
          modelShell,
          DataInputHandler(expectedServices),
          EmDataCore.create(using simulationStartDate),
        )

    startRegistration(
      inputModel,
      dummyFactoryUpdater,
      completionBehavior,
      modelShell.modelStrategy.getServiceRegistrationData,
      services,
    )

  }
}
