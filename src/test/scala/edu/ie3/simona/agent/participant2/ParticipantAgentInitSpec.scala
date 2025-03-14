/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant2

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ActivePowerExtra
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.SimpleInputContainer
import edu.ie3.simona.agent.participant2.ParticipantAgent.{
  PrimaryRegistrationSuccessfulMessage,
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.agent.participant2.ParticipantAgentInit.{
  ParticipantRefs,
  SimulationParameters,
}
import edu.ie3.simona.config.RuntimeConfig.{LoadRuntimeConfig, PvRuntimeConfig}
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexActivation,
  FlexCompletion,
  FlexResponse,
  RegisterControlledAsset,
  ScheduleFlexActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.WeatherMessage.RegisterForWeatherMessage
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.service.ServiceType
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.{LoadInputTestData, PvInputTestData}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.simona.util.TickUtil.TickLong
import org.apache.pekko.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import squants.Each

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit

/** Testing [[ParticipantAgentInit]], which means testing the complete
  * initialization process of [[ParticipantAgent]] up until the first tick
  */
class ParticipantAgentInitSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with LoadInputTestData
    with PvInputTestData {

  private implicit val simulationStart: ZonedDateTime = defaultSimulationStart

  private val simulationParams = SimulationParameters(
    3600,
    Each(1e-14),
    simulationStart,
    defaultSimulationStart.plus(2, ChronoUnit.DAYS),
  )

  "A ParticipantAgent that is not depending on external services" when {

    val runtimeConfig = LoadRuntimeConfig()

    val operationStart = 10 * 3600L

    val mockInput = SimpleInputContainer(
      loadInput.copy
        .operationTime(
          OperationTime.builder
            .withStart(operationStart.toDateTime)
            .build()
        )
        .build()
    )

    "not controlled by EM" should {

      "initialize correctly when not replaying primary data" in {

        val scheduler = createTestProbe[SchedulerMessage]()

        val gridAgent = createTestProbe[GridAgent.Request]()
        val primaryService = createTestProbe[Any]()
        val resultListener = createTestProbe[ResultEvent]()

        val refs = ParticipantRefs(
          gridAgent = gridAgent.ref,
          primaryServiceProxy = primaryService.ref.toClassic,
          services = Map.empty,
          resultListener = Iterable(resultListener.ref),
        )

        val participantAgent = spawn(
          ParticipantAgentInit(
            mockInput,
            runtimeConfig,
            mock[NotifierConfig],
            refs,
            simulationParams,
            Left(scheduler.ref),
          )
        )

        val scheduleMsg = scheduler.expectMessageType[ScheduleActivation]
        scheduleMsg.tick shouldBe INIT_SIM_TICK
        val activationRef = scheduleMsg.actor

        activationRef ! Activation(INIT_SIM_TICK)

        primaryService.expectMessage(
          PrimaryServiceRegistrationMessage(
            participantAgent.ref.toClassic,
            mockInput.electricalInputModel.getUuid,
          )
        )

        participantAgent ! RegistrationFailedMessage(
          primaryService.ref.toClassic
        )

        scheduler.expectMessage(Completion(activationRef, Some(operationStart)))

      }

      "initialize correctly when replaying primary data" in {

        val scheduler = createTestProbe[SchedulerMessage]()

        val gridAgent = createTestProbe[GridAgent.Request]()
        val primaryService = createTestProbe[Any]()
        val resultListener = createTestProbe[ResultEvent]()

        val refs = ParticipantRefs(
          gridAgent = gridAgent.ref,
          primaryServiceProxy = primaryService.ref.toClassic,
          services = Map.empty,
          resultListener = Iterable(resultListener.ref),
        )

        val participantAgent = spawn(
          ParticipantAgentInit(
            mockInput,
            runtimeConfig,
            mock[NotifierConfig],
            refs,
            simulationParams,
            Left(scheduler.ref),
          )
        )

        val scheduleMsg = scheduler.expectMessageType[ScheduleActivation]
        scheduleMsg.tick shouldBe INIT_SIM_TICK
        val activationRef = scheduleMsg.actor

        activationRef ! Activation(INIT_SIM_TICK)

        primaryService.expectMessage(
          PrimaryServiceRegistrationMessage(
            participantAgent.ref.toClassic,
            mockInput.electricalInputModel.getUuid,
          )
        )

        participantAgent ! PrimaryRegistrationSuccessfulMessage(
          primaryService.ref.toClassic,
          15 * 3600L,
          ActivePowerExtra,
        )

        scheduler.expectMessage(Completion(activationRef, Some(15 * 3600L)))

      }

    }

    "controlled by EM" should {

      "initialize correctly when not replaying primary data" in {

        val em = createTestProbe[FlexResponse]()

        val gridAgent = createTestProbe[GridAgent.Request]()
        val primaryService = createTestProbe[Any]()
        val resultListener = createTestProbe[ResultEvent]()

        val refs = ParticipantRefs(
          gridAgent = gridAgent.ref,
          primaryServiceProxy = primaryService.ref.toClassic,
          services = Map.empty,
          resultListener = Iterable(resultListener.ref),
        )

        val participantAgent = spawn(
          ParticipantAgentInit(
            mockInput,
            runtimeConfig,
            mock[NotifierConfig],
            refs,
            simulationParams,
            Right(em.ref),
          )
        )

        val emRegistrationMsg = em.expectMessageType[RegisterControlledAsset]
        emRegistrationMsg.modelUuid shouldBe mockInput.electricalInputModel.getUuid
        emRegistrationMsg.inputModel shouldBe mockInput.electricalInputModel
        val activationRef = emRegistrationMsg.participant

        em.expectMessage(
          ScheduleFlexActivation(
            mockInput.electricalInputModel.getUuid,
            INIT_SIM_TICK,
          )
        )

        activationRef ! FlexActivation(INIT_SIM_TICK)

        primaryService.expectMessage(
          PrimaryServiceRegistrationMessage(
            participantAgent.ref.toClassic,
            mockInput.electricalInputModel.getUuid,
          )
        )

        participantAgent ! RegistrationFailedMessage(
          primaryService.ref.toClassic
        )

        em.expectMessage(
          FlexCompletion(
            mockInput.electricalInputModel.getUuid,
            requestAtTick = Some(operationStart),
          )
        )

      }

      "initialize correctly when replaying primary data" in {

        val em = createTestProbe[FlexResponse]()

        val gridAgent = createTestProbe[GridAgent.Request]()
        val primaryService = createTestProbe[Any]()
        val resultListener = createTestProbe[ResultEvent]()

        val refs = ParticipantRefs(
          gridAgent = gridAgent.ref,
          primaryServiceProxy = primaryService.ref.toClassic,
          services = Map.empty,
          resultListener = Iterable(resultListener.ref),
        )

        val participantAgent = spawn(
          ParticipantAgentInit(
            mockInput,
            runtimeConfig,
            mock[NotifierConfig],
            refs,
            simulationParams,
            Right(em.ref),
          )
        )

        val emRegistrationMsg = em.expectMessageType[RegisterControlledAsset]
        emRegistrationMsg.modelUuid shouldBe mockInput.electricalInputModel.getUuid
        emRegistrationMsg.inputModel shouldBe mockInput.electricalInputModel
        val activationRef = emRegistrationMsg.participant

        em.expectMessage(
          ScheduleFlexActivation(
            mockInput.electricalInputModel.getUuid,
            INIT_SIM_TICK,
          )
        )

        activationRef ! FlexActivation(INIT_SIM_TICK)

        primaryService.expectMessage(
          PrimaryServiceRegistrationMessage(
            participantAgent.ref.toClassic,
            mockInput.electricalInputModel.getUuid,
          )
        )

        participantAgent ! PrimaryRegistrationSuccessfulMessage(
          primaryService.ref.toClassic,
          15 * 3600L,
          ActivePowerExtra,
        )

        em.expectMessage(
          FlexCompletion(
            mockInput.electricalInputModel.getUuid,
            requestAtTick = Some(15 * 3600L),
          )
        )
      }

    }

  }

  "A ParticipantAgent that is depending on an external service" when {

    val operationStart = 10 * 3600L

    val mockInput = SimpleInputContainer(
      pvInput.copy
        .operationTime(
          OperationTime.builder
            .withStart(operationStart.toDateTime)
            .build()
        )
        .build()
    )

    val runtimeConfig = PvRuntimeConfig()

    "not controlled by EM" should {

      "initialize correctly when not replaying primary data" in {

        val scheduler = createTestProbe[SchedulerMessage]()

        val gridAgent = createTestProbe[GridAgent.Request]()
        val primaryService = createTestProbe[Any]()
        val resultListener = createTestProbe[ResultEvent]()
        val service = createTestProbe[Any]()

        val refs = ParticipantRefs(
          gridAgent = gridAgent.ref,
          primaryServiceProxy = primaryService.ref.toClassic,
          services = Map(ServiceType.WeatherService -> service.ref.toClassic),
          resultListener = Iterable(resultListener.ref),
        )

        val participantAgent = spawn(
          ParticipantAgentInit(
            mockInput,
            runtimeConfig,
            mock[NotifierConfig],
            refs,
            simulationParams,
            Left(scheduler.ref),
          )
        )

        val scheduleMsg = scheduler.expectMessageType[ScheduleActivation]
        scheduleMsg.tick shouldBe INIT_SIM_TICK
        val activationRef = scheduleMsg.actor

        activationRef ! Activation(INIT_SIM_TICK)

        primaryService.expectMessage(
          PrimaryServiceRegistrationMessage(
            participantAgent.ref.toClassic,
            mockInput.electricalInputModel.getUuid,
          )
        )

        participantAgent ! RegistrationFailedMessage(
          primaryService.ref.toClassic
        )

        service.expectMessage(
          RegisterForWeatherMessage(
            participantAgent.toClassic,
            mockInput.electricalInputModel.getNode.getGeoPosition.getY,
            mockInput.electricalInputModel.getNode.getGeoPosition.getX,
          )
        )

        participantAgent ! RegistrationSuccessfulMessage(
          service.ref.toClassic,
          12 * 3600L,
        )

        scheduler.expectMessage(Completion(activationRef, Some(12 * 3600L)))

      }

      "initialize correctly when replaying primary data" in {

        val scheduler = createTestProbe[SchedulerMessage]()

        val gridAgent = createTestProbe[GridAgent.Request]()
        val primaryService = createTestProbe[Any]()
        val resultListener = createTestProbe[ResultEvent]()
        val service = createTestProbe[Any]()

        val refs = ParticipantRefs(
          gridAgent = gridAgent.ref,
          primaryServiceProxy = primaryService.ref.toClassic,
          services = Map(ServiceType.WeatherService -> service.ref.toClassic),
          resultListener = Iterable(resultListener.ref),
        )

        val participantAgent = spawn(
          ParticipantAgentInit(
            mockInput,
            runtimeConfig,
            mock[NotifierConfig],
            refs,
            simulationParams,
            Left(scheduler.ref),
          )
        )

        val scheduleMsg = scheduler.expectMessageType[ScheduleActivation]
        scheduleMsg.tick shouldBe INIT_SIM_TICK
        val activationRef = scheduleMsg.actor

        activationRef ! Activation(INIT_SIM_TICK)

        primaryService.expectMessage(
          PrimaryServiceRegistrationMessage(
            participantAgent.ref.toClassic,
            mockInput.electricalInputModel.getUuid,
          )
        )

        participantAgent ! PrimaryRegistrationSuccessfulMessage(
          primaryService.ref.toClassic,
          // no activation expected for this tick, since it is
          // outside the operation interval
          15 * 3600L,
          ActivePowerExtra,
        )

        scheduler.expectMessage(Completion(activationRef, Some(15 * 3600L)))

        // service should not be called at all
        service.expectNoMessage()
      }

    }

    "controlled by EM" should {

      "initialize correctly when not replaying primary data" in {

        val em = createTestProbe[FlexResponse]()

        val gridAgent = createTestProbe[GridAgent.Request]()
        val primaryService = createTestProbe[Any]()
        val resultListener = createTestProbe[ResultEvent]()
        val service = createTestProbe[Any]()

        val refs = ParticipantRefs(
          gridAgent = gridAgent.ref,
          primaryServiceProxy = primaryService.ref.toClassic,
          services = Map(ServiceType.WeatherService -> service.ref.toClassic),
          resultListener = Iterable(resultListener.ref),
        )

        val participantAgent = spawn(
          ParticipantAgentInit(
            mockInput,
            runtimeConfig,
            mock[NotifierConfig],
            refs,
            simulationParams,
            Right(em.ref),
          )
        )

        val emRegistrationMsg = em.expectMessageType[RegisterControlledAsset]
        emRegistrationMsg.modelUuid shouldBe mockInput.electricalInputModel.getUuid
        emRegistrationMsg.inputModel shouldBe mockInput.electricalInputModel
        val activationRef = emRegistrationMsg.participant

        em.expectMessage(
          ScheduleFlexActivation(
            mockInput.electricalInputModel.getUuid,
            INIT_SIM_TICK,
          )
        )

        activationRef ! FlexActivation(INIT_SIM_TICK)

        primaryService.expectMessage(
          PrimaryServiceRegistrationMessage(
            participantAgent.ref.toClassic,
            mockInput.electricalInputModel.getUuid,
          )
        )

        participantAgent ! RegistrationFailedMessage(
          primaryService.ref.toClassic
        )

        service.expectMessage(
          RegisterForWeatherMessage(
            participantAgent.toClassic,
            mockInput.electricalInputModel.getNode.getGeoPosition.getY,
            mockInput.electricalInputModel.getNode.getGeoPosition.getX,
          )
        )

        participantAgent ! RegistrationSuccessfulMessage(
          service.ref.toClassic,
          12 * 3600L,
        )

        em.expectMessage(
          FlexCompletion(
            mockInput.electricalInputModel.getUuid,
            requestAtTick = Some(12 * 3600L),
          )
        )
      }

      "initialize correctly when replaying primary data" in {

        val em = createTestProbe[FlexResponse]()

        val gridAgent = createTestProbe[GridAgent.Request]()
        val primaryService = createTestProbe[Any]()
        val resultListener = createTestProbe[ResultEvent]()
        val service = createTestProbe[Any]()

        val refs = ParticipantRefs(
          gridAgent = gridAgent.ref,
          primaryServiceProxy = primaryService.ref.toClassic,
          services = Map(ServiceType.WeatherService -> service.ref.toClassic),
          resultListener = Iterable(resultListener.ref),
        )

        val participantAgent = spawn(
          ParticipantAgentInit(
            mockInput,
            runtimeConfig,
            mock[NotifierConfig],
            refs,
            simulationParams,
            Right(em.ref),
          )
        )

        val emRegistrationMsg = em.expectMessageType[RegisterControlledAsset]
        emRegistrationMsg.modelUuid shouldBe mockInput.electricalInputModel.getUuid
        emRegistrationMsg.inputModel shouldBe mockInput.electricalInputModel
        val activationRef = emRegistrationMsg.participant

        em.expectMessage(
          ScheduleFlexActivation(
            mockInput.electricalInputModel.getUuid,
            INIT_SIM_TICK,
          )
        )

        activationRef ! FlexActivation(INIT_SIM_TICK)

        primaryService.expectMessage(
          PrimaryServiceRegistrationMessage(
            participantAgent.ref.toClassic,
            mockInput.electricalInputModel.getUuid,
          )
        )

        participantAgent ! PrimaryRegistrationSuccessfulMessage(
          primaryService.ref.toClassic,
          // no activation expected for this tick, since it is
          // outside the operation interval
          15 * 3600L,
          ActivePowerExtra,
        )

        em.expectMessage(
          FlexCompletion(
            mockInput.electricalInputModel.getUuid,
            requestAtTick = Some(15 * 3600L),
          )
        )

        // service should not be called at all
        service.expectNoMessage()
      }

    }

  }

}
