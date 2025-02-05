/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant2

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ActivePowerMeta
import edu.ie3.simona.agent.participant2.ParticipantAgent.{
  PrimaryRegistrationSuccessfulMessage,
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.agent.participant2.ParticipantAgentInit.ParticipantRefs
import edu.ie3.simona.config.SimonaConfig.{LoadRuntimeConfig, PvRuntimeConfig}
import edu.ie3.simona.event.ResultEvent
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
import edu.ie3.util.TimeUtil
import org.apache.pekko.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps

import java.time.ZonedDateTime

class ParticipantAgentInitSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with LoadInputTestData
    with PvInputTestData {

  private implicit val simulationStart: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2025-01-01T00:00:00Z")
  private val simulationEnd: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2025-03-01T00:00:00Z")

  "A ParticipantAgent that is not depending on external services" when {

    val config = LoadRuntimeConfig(
      calculateMissingReactivePowerWithModel = false,
      scaling = 1.0,
      uuids = List.empty,
      modelBehaviour = "fix",
      reference = "power",
    )

    val operationStart = 10 * 3600L

    val mockInput = loadInput.copy
      .operationTime(
        OperationTime.builder
          .withStart(operationStart.toDateTime)
          .build()
      )
      .build()

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
            config,
            refs,
            3600,
            simulationStart,
            simulationEnd,
            Left(scheduler.ref),
          )
        )

        val scheduleMsg = scheduler.expectMessageType[ScheduleActivation]
        scheduleMsg.tick shouldBe INIT_SIM_TICK
        val activationRef = scheduleMsg.actor

        activationRef ! Activation(INIT_SIM_TICK)

        primaryService.expectMessage(
          PrimaryServiceRegistrationMessage(mockInput.getUuid)
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
            config,
            refs,
            3600,
            simulationStart,
            simulationEnd,
            Left(scheduler.ref),
          )
        )

        val scheduleMsg = scheduler.expectMessageType[ScheduleActivation]
        scheduleMsg.tick shouldBe INIT_SIM_TICK
        val activationRef = scheduleMsg.actor

        activationRef ! Activation(INIT_SIM_TICK)

        primaryService.expectMessage(
          PrimaryServiceRegistrationMessage(mockInput.getUuid)
        )

        participantAgent ! PrimaryRegistrationSuccessfulMessage(
          primaryService.ref.toClassic,
          15 * 3600L,
          ActivePowerMeta,
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
            config,
            refs,
            3600,
            simulationStart,
            simulationEnd,
            Right(em.ref),
          )
        )

        val emRegistrationMsg = em.expectMessageType[RegisterControlledAsset]
        emRegistrationMsg.modelUuid shouldBe mockInput.getUuid
        emRegistrationMsg.inputModel shouldBe mockInput
        val activationRef = emRegistrationMsg.participant

        em.expectMessage(
          ScheduleFlexActivation(mockInput.getUuid, INIT_SIM_TICK)
        )

        activationRef ! FlexActivation(INIT_SIM_TICK)

        primaryService.expectMessage(
          PrimaryServiceRegistrationMessage(mockInput.getUuid)
        )

        participantAgent ! RegistrationFailedMessage(
          primaryService.ref.toClassic
        )

        em.expectMessage(
          FlexCompletion(
            mockInput.getUuid,
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
            config,
            refs,
            3600,
            simulationStart,
            simulationEnd,
            Right(em.ref),
          )
        )

        val emRegistrationMsg = em.expectMessageType[RegisterControlledAsset]
        emRegistrationMsg.modelUuid shouldBe mockInput.getUuid
        emRegistrationMsg.inputModel shouldBe mockInput
        val activationRef = emRegistrationMsg.participant

        em.expectMessage(
          ScheduleFlexActivation(mockInput.getUuid, INIT_SIM_TICK)
        )

        activationRef ! FlexActivation(INIT_SIM_TICK)

        primaryService.expectMessage(
          PrimaryServiceRegistrationMessage(mockInput.getUuid)
        )

        participantAgent ! PrimaryRegistrationSuccessfulMessage(
          primaryService.ref.toClassic,
          15 * 3600L,
          ActivePowerMeta,
        )

        em.expectMessage(
          FlexCompletion(mockInput.getUuid, requestAtTick = Some(15 * 3600L))
        )
      }

    }

  }

  "A ParticipantAgent that is depending on an external service" when {

    val operationStart = 10 * 3600L

    val mockInput = pvInput.copy
      .operationTime(
        OperationTime.builder
          .withStart(operationStart.toDateTime)
          .build()
      )
      .build()

    val config = PvRuntimeConfig(
      calculateMissingReactivePowerWithModel = false,
      scaling = 1.0,
      uuids = List.empty,
    )

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
            config,
            refs,
            3600,
            simulationStart,
            simulationEnd,
            Left(scheduler.ref),
          )
        )

        val scheduleMsg = scheduler.expectMessageType[ScheduleActivation]
        scheduleMsg.tick shouldBe INIT_SIM_TICK
        val activationRef = scheduleMsg.actor

        activationRef ! Activation(INIT_SIM_TICK)

        primaryService.expectMessage(
          PrimaryServiceRegistrationMessage(mockInput.getUuid)
        )

        participantAgent ! RegistrationFailedMessage(
          primaryService.ref.toClassic
        )

        service.expectMessage(
          RegisterForWeatherMessage(
            mockInput.getNode.getGeoPosition.getY,
            mockInput.getNode.getGeoPosition.getX,
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
            config,
            refs,
            3600,
            simulationStart,
            simulationEnd,
            Left(scheduler.ref),
          )
        )

        val scheduleMsg = scheduler.expectMessageType[ScheduleActivation]
        scheduleMsg.tick shouldBe INIT_SIM_TICK
        val activationRef = scheduleMsg.actor

        activationRef ! Activation(INIT_SIM_TICK)

        primaryService.expectMessage(
          PrimaryServiceRegistrationMessage(mockInput.getUuid)
        )

        participantAgent ! PrimaryRegistrationSuccessfulMessage(
          primaryService.ref.toClassic,
          // no activation expected for this tick, since it is
          // outside the operation interval
          15 * 3600L,
          ActivePowerMeta,
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
            config,
            refs,
            3600,
            simulationStart,
            simulationEnd,
            Right(em.ref),
          )
        )

        val emRegistrationMsg = em.expectMessageType[RegisterControlledAsset]
        emRegistrationMsg.modelUuid shouldBe mockInput.getUuid
        emRegistrationMsg.inputModel shouldBe mockInput
        val activationRef = emRegistrationMsg.participant

        em.expectMessage(
          ScheduleFlexActivation(mockInput.getUuid, INIT_SIM_TICK)
        )

        activationRef ! FlexActivation(INIT_SIM_TICK)

        primaryService.expectMessage(
          PrimaryServiceRegistrationMessage(mockInput.getUuid)
        )

        participantAgent ! RegistrationFailedMessage(
          primaryService.ref.toClassic
        )

        service.expectMessage(
          RegisterForWeatherMessage(
            mockInput.getNode.getGeoPosition.getY,
            mockInput.getNode.getGeoPosition.getX,
          )
        )

        participantAgent ! RegistrationSuccessfulMessage(
          service.ref.toClassic,
          12 * 3600L,
        )

        em.expectMessage(
          FlexCompletion(mockInput.getUuid, requestAtTick = Some(12 * 3600L))
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
            config,
            refs,
            3600,
            simulationStart,
            simulationEnd,
            Right(em.ref),
          )
        )

        val emRegistrationMsg = em.expectMessageType[RegisterControlledAsset]
        emRegistrationMsg.modelUuid shouldBe mockInput.getUuid
        emRegistrationMsg.inputModel shouldBe mockInput
        val activationRef = emRegistrationMsg.participant

        em.expectMessage(
          ScheduleFlexActivation(mockInput.getUuid, INIT_SIM_TICK)
        )

        activationRef ! FlexActivation(INIT_SIM_TICK)

        primaryService.expectMessage(
          PrimaryServiceRegistrationMessage(mockInput.getUuid)
        )

        participantAgent ! PrimaryRegistrationSuccessfulMessage(
          primaryService.ref.toClassic,
          // no activation expected for this tick, since it is
          // outside the operation interval
          15 * 3600L,
          ActivePowerMeta,
        )

        em.expectMessage(
          FlexCompletion(mockInput.getUuid, requestAtTick = Some(15 * 3600L))
        )

        // service should not be called at all
        service.expectNoMessage()
      }

    }

  }

}
