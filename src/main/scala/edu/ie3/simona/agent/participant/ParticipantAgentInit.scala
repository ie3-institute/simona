/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.agent.participant.ParticipantAgent.*
import edu.ie3.simona.agent.{DataInputHandler, SecondaryServiceRegistration}
import edu.ie3.simona.config.RuntimeConfig.BaseRuntimeConfig
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.InputModelContainer
import edu.ie3.simona.model.participant.ParticipantModel.{
  AdditionalFactoryData,
  ModelState,
  ParticipantModelFactory,
}
import edu.ie3.simona.model.participant.{
  ParticipantModelInit,
  ParticipantModelShell,
}
import edu.ie3.simona.ontology.messages.AgentMessage.{ActivationRequest, tick}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  PrimaryRegistrationSuccessfulMessage,
  PrimaryServiceRegistrationMessage,
  RegistrationFailedMessage,
}
import edu.ie3.simona.ontology.messages.flex.FlexType
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.ontology.messages.{
  Activation,
  SchedulerMessage,
  ServiceMessage,
}
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.service.results.ResultServiceProxy.{
  ExpectResult,
  NoResult,
}
import edu.ie3.simona.service.{
  DataTimeType,
  ServiceRegistrationData,
  ServiceType,
}
import edu.ie3.simona.util.InputUtils.identifier
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
object ParticipantAgentInit
    extends SecondaryServiceRegistration[Message, ParticipantModelFactory[
      ? <: ModelState
    ]] {

  /** Container class that gathers references to relevant actors.
    *
    * @param primaryServiceProxy
    *   Reference to the primary service proxy.
    * @param resultServiceProxy
    *   Reference to the result service proxy.
    * @param services
    *   References to services by service type.
    */
  final case class ParticipantRefs(
      primaryServiceProxy: ActorRef[ServiceMessage],
      resultServiceProxy: ActorRef[ResultEvent | ExpectResult | NoResult],
      services: Map[ServiceType, ActorRef[ServiceMessage]],
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
    *   Date of the very first tick in the simulation.
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
    * @param participantRefs
    *   A collection of actor references to actors required for initialization
    *   and operation.
    * @param simulationParams
    *   Some parameters required for simulation.
    * @param parent
    *   The parent actor scheduling or controlling this participant, i.e. either
    *   a [[edu.ie3.simona.scheduler.Scheduler]] or an
    *   [[edu.ie3.simona.agent.em.EmAgent]].
    * @param runtimeConfig
    *   Runtime configuration that has to match the participant type.
    * @param notifierConfig
    *   The result configuration.
    */
  def apply(
      inputContainer: InputModelContainer[? <: SystemParticipantInput],
      runtimeConfig: BaseRuntimeConfig,
      notifierConfig: NotifierConfig,
      parent: Either[ActorRef[SchedulerMessage], ActorRef[FlexResponse]],
      scheduleKey: ScheduleKey,
  )(using
      participantRefs: ParticipantRefs,
      simulationParams: SimulationParameters,
  ): Behavior[Message] = Behaviors.setup { ctx =>
    parent match {
      case Right(em) =>
        em ! RegisterControlledAsset(
          ctx.self,
          inputContainer.electricalInputModel,
        )

        em ! ScheduleFlexActivation(
          inputContainer.electricalInputModel.getUuid,
          INIT_SIM_TICK,
          Some(scheduleKey),
        )
      case Left(scheduler) =>
        scheduler ! ScheduleActivation(
          ctx.self,
          INIT_SIM_TICK,
          Some(scheduleKey),
        )
    }

    uninitialized(using
      inputContainer,
      runtimeConfig,
      notifierConfig,
      participantRefs,
      simulationParams,
      parent,
    )
  }

  /** Waiting for an [[ActivationRequest]] message to start the initialization.
    */
  private def uninitialized(using
      inputContainer: InputModelContainer[? <: SystemParticipantInput],
      runtimeConfig: BaseRuntimeConfig,
      notifierConfig: NotifierConfig,
      participantRefs: ParticipantRefs,
      simulationParams: SimulationParameters,
      parent: Either[ActorRef[SchedulerMessage], ActorRef[FlexResponse]],
  ): Behavior[Message] = Behaviors.receivePartial {

    case (ctx, activation: ActivationRequest)
        if activation.tick == INIT_SIM_TICK =>
      val (flexType, dataTimeType) = activation match {
        case _: Activation =>
          // If we're not EM-controlled, we're only interested in calculating
          // for the current point in simulation time
          (None, DataTimeType.Current)
        case FlexInit(ft, timeType) =>
          (Some(ft), timeType)
        case _ =>
          throw new CriticalFailureException(
            s"${inputContainer.electricalInputModel.identifier}: Unexpected initial activation $activation"
          )
      }

      // first, check whether we're just supposed to replay primary data time series
      participantRefs.primaryServiceProxy ! PrimaryServiceRegistrationMessage(
        ctx.self,
        inputContainer.electricalInputModel.getUuid,
      )

      waitingForPrimaryProxy(flexType, dataTimeType)

  }

  /** Waits for the primary proxy to respond, which decides whether this
    * participant uses model calculations or just replays primary data.
    *
    * @param flexType
    *   The flexibility type that the controlling EM demands flex options for,
    *   if applicable.
    * @param dataTimeType
    *   The data time type of flex options (if applicable) and operating points
    *   to be calculated.
    */
  private def waitingForPrimaryProxy(
      flexType: Option[FlexType],
      dataTimeType: DataTimeType,
  )(using
      inputContainer: InputModelContainer[? <: SystemParticipantInput],
      participantRefs: ParticipantRefs,
      simulationParams: SimulationParameters,
      runtimeConfig: BaseRuntimeConfig,
      notifierConfig: NotifierConfig,
      parent: Either[ActorRef[SchedulerMessage], ActorRef[FlexResponse]],
  ): Behavior[Message] = Behaviors.receivePartial {

    case (
          ctx,
          PrimaryRegistrationSuccessfulMessage(
            serviceRef,
            firstDataTick,
            primaryDataExtra,
          ),
        ) =>
      // we're supposed to replay primary data, initialize accordingly
      val expectedFirstData: Map[ActorRef[ServiceMessage], Long] =
        Map(serviceRef -> firstDataTick)

      completeInitialization(
        ParticipantModelInit.getPrimaryModelFactory(
          inputContainer,
          runtimeConfig,
          primaryDataExtra,
        ),
        flexType.map((_, dataTimeType)),
        inputContainer.electricalInputModel,
        expectedFirstData,
        ctx.self,
      )

    case (ctx, RegistrationFailedMessage(_)) =>
      // we're _not_ supposed to replay primary data, thus initialize the physical model
      val modelFactory = ParticipantModelInit.getPhysicalModelFactory(
        inputContainer,
        runtimeConfig,
      )
      val factoryUpdater: AdditionalDataConsumer =
        new AdditionalDataConsumer {

          override def update(
              data: AdditionalFactoryData
          ): ParticipantModelFactory[? <: ModelState] =
            modelFactory.update(data)

          override def unchanged: ParticipantModelFactory[? <: ModelState] =
            modelFactory
        }

      val completionBehavior = (mf, expectedServices) =>
        completeInitialization(
          mf,
          flexType.map((_, dataTimeType)),
          inputContainer.electricalInputModel,
          expectedServices,
          ctx.self,
        )

      startRegistration(
        inputContainer.electricalInputModel,
        factoryUpdater,
        completionBehavior,
        ServiceRegistrationData(
          modelFactory.getRequiredSecondaryServices,
          dataTimeType,
        ),
        participantRefs.services,
      )
  }

  /** Completes initialization, sends a completion message and creates actual
    * [[ParticipantAgent]]
    */
  private def completeInitialization(
      modelFactory: ParticipantModelFactory[? <: ModelState],
      flexParams: Option[(FlexType, DataTimeType)],
      participantInput: SystemParticipantInput,
      expectedData: Map[ActorRef[ServiceMessage], Long],
      self: ActorRef[Message],
  )(using
      participantRefs: ParticipantRefs,
      simulationParams: SimulationParameters,
      notifierConfig: NotifierConfig,
      parent: Either[ActorRef[SchedulerMessage], ActorRef[FlexResponse]],
  ): Behavior[Message] = {

    val modelShell = ParticipantModelShell.create(
      modelFactory,
      flexParams,
      participantInput.getOperationTime,
      simulationParams.simulationStart,
      simulationParams.simulationEnd,
    )

    val inputHandler = DataInputHandler(expectedData)

    val firstTick = modelShell.operationStart
    val dataCompletedTick = inputHandler.getDataUpdatedTick

    dataCompletedTick.foreach { dataCompleted =>
      if dataCompleted > firstTick then
        throw new CriticalFailureException(
          s"${modelShell.getIdentifier}: Input data will only be fully received at tick $dataCompleted. " +
            s"It needs to be available with operation start $firstTick though."
        )
    }

    parent.fold(
      _ ! Completion(
        actor = self,
        newTick = Some(firstTick),
      ),
      _ ! FlexCompletion(
        modelUuid = modelShell.uuid,
        requestAtTick = Some(firstTick),
      ),
    )

    ParticipantAgent(
      modelShell,
      inputHandler,
      ParticipantGridAdapter(
        simulationParams.expectedPowerRequestTick,
        simulationParams.requestVoltageDeviationTolerance,
      ),
      ParticipantResultHandler(
        participantRefs.resultServiceProxy,
        notifierConfig,
      ),
    )
  }

}
