/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.system.{LoadInput, PvInput, StorageInput}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.em.EmAgent
import edu.ie3.simona.agent.participant.em.EmAgent.EmAgentInitializeStateData
import edu.ie3.simona.agent.participant.load.LoadAgent.FixedLoadAgent
import edu.ie3.simona.agent.participant.pv.PVAgent
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.agent.participant.storage.StorageAgent
import edu.ie3.simona.config.SimonaConfig.{
  LoadRuntimeConfig,
  PvRuntimeConfig,
  StorageRuntimeConfig
}
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.{
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage
}
import edu.ie3.simona.ontology.messages.services.WeatherMessage.{
  ProvideWeatherMessage,
  RegisterForWeatherMessage,
  WeatherData
}
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeParticipantAgentTrigger
}
import edu.ie3.simona.test.ParticipantAgentSpec
import edu.ie3.simona.test.common.input.EmInputTestData
import org.scalatestplus.mockito.MockitoSugar
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.{CELSIUS, METRE_PER_SECOND}

class EmAgentIT
    extends ParticipantAgentSpec(
      ActorSystem(
        "EmAgentIT",
        ConfigFactory
          .parseString("""
          |akka.loggers =["akka.event.slf4j.Slf4jLogger"]
          |akka.loglevel="DEBUG"
""".stripMargin)
      )
    )
    with EmInputTestData
    with MockitoSugar {

  private val resolution =
    simonaConfig.simona.powerflow.resolution.getSeconds

  private val outputConfigOn = ParticipantNotifierConfig(
    simulationResultInfo = true,
    powerRequestReply = false
  )

  private val outputConfigOff = ParticipantNotifierConfig(
    simulationResultInfo = false,
    powerRequestReply = false
  )

  private val tolerance = 1e-10d

  "An em agent and connected load, pv and storage agents" should {
    "be initialized correctly and run through some activations" in {
      val resultsListener = TestProbe("ResultListener")
      val primaryServiceProxy = TestProbe("PrimaryServiceProxy")
      val weatherService = TestProbe("WeatherService")

      val emAgent = TestActorRef(
        new EmAgent(
          scheduler = scheduler.ref,
          listener = Iterable(resultsListener.ref)
        ),
        "EmAgent"
      )

      val initId = 0

      val loadAgent = TestActorRef(
        new FixedLoadAgent(
          scheduler = emAgent,
          listener = Iterable(resultsListener.ref)
        ),
        "LoadAgent"
      )
      val pvAgent = TestActorRef(
        new PVAgent(
          scheduler = emAgent,
          listener = Iterable(resultsListener.ref)
        ),
        "PvAgent"
      )
      val storageAgent = TestActorRef(
        new StorageAgent(
          scheduler = emAgent,
          listener = Iterable(resultsListener.ref)
        ),
        "StorageAgent"
      )

      val loadAgentInit =
        InitializeParticipantAgentTrigger[
          ApparentPower,
          ParticipantInitializeStateData[
            LoadInput,
            LoadRuntimeConfig,
            ApparentPower
          ]
        ](
          ParticipantInitializeStateData(
            loadInput,
            LoadRuntimeConfig(
              calculateMissingReactivePowerWithModel = true,
              scaling = 1d,
              modelBehaviour = "fix",
              reference = "power",
              uuids = List.empty
            ),
            primaryServiceProxy.ref,
            None,
            simulationStartDate,
            simulationEndDate,
            resolution,
            simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
            outputConfigOff,
            Some(emAgent)
          )
        )
      val pvAgentInit =
        InitializeParticipantAgentTrigger[
          ApparentPower,
          ParticipantInitializeStateData[
            PvInput,
            PvRuntimeConfig,
            ApparentPower
          ]
        ](
          ParticipantInitializeStateData(
            pvInput,
            PvRuntimeConfig(
              calculateMissingReactivePowerWithModel = true,
              scaling = 1d,
              uuids = List.empty
            ),
            primaryServiceProxy.ref,
            Some(Vector(ActorWeatherService(weatherService.ref))),
            simulationStartDate,
            simulationEndDate,
            resolution,
            simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
            outputConfigOff,
            Some(emAgent)
          )
        )
      val storageAgentInit =
        InitializeParticipantAgentTrigger[
          ApparentPower,
          ParticipantInitializeStateData[
            StorageInput,
            StorageRuntimeConfig,
            ApparentPower
          ]
        ](
          ParticipantInitializeStateData(
            storageInput,
            StorageRuntimeConfig(
              calculateMissingReactivePowerWithModel = true,
              scaling = 1d,
              uuids = List.empty
            ),
            primaryServiceProxy.ref,
            None,
            simulationStartDate,
            simulationEndDate,
            resolution,
            simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
            outputConfigOff,
            Some(emAgent)
          )
        )

      val connectedAgents = Seq(
        (loadAgent, loadAgentInit, loadInput),
        (pvAgent, pvAgentInit, pvInput),
        (storageAgent, storageAgentInit, storageInput)
      )

      scheduler.send(
        emAgent,
        TriggerWithIdMessage(
          InitializeParticipantAgentTrigger[
            ApparentPower,
            EmAgentInitializeStateData
          ](
            EmAgentInitializeStateData(
              inputModel = emInput,
              modelConfig = modelConfig,
              secondaryDataServices = None,
              simulationStartDate = simulationStartDate,
              simulationEndDate = simulationEndDate,
              resolution = resolution,
              requestVoltageDeviationThreshold =
                simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfig = outputConfigOn,
              primaryServiceProxy = primaryServiceProxy.ref,
              connectedAgents = connectedAgents
            )
          ),
          initId,
          emAgent
        )
      )

      // receive and deal with primary registration messages
      Range(0, 3)
        .map(_ =>
          primaryServiceProxy.expectMsgType[PrimaryServiceRegistrationMessage]
        )
        .map(
          _.inputModelUuid
        ) should contain allOf (loadInput.getUuid, pvInput.getUuid, storageInput.getUuid)

      primaryServiceProxy.send(loadAgent, RegistrationFailedMessage)
      primaryServiceProxy.send(pvAgent, RegistrationFailedMessage)
      primaryServiceProxy.send(storageAgent, RegistrationFailedMessage)

      // deal with weather service registration
      weatherService.expectMsg(
        RegisterForWeatherMessage(
          pvInput.getNode.getGeoPosition.getY,
          pvInput.getNode.getGeoPosition.getX
        )
      )

      weatherService.send(pvAgent, RegistrationSuccessfulMessage(Some(0L)))

      scheduler.expectMsg(
        CompletionMessage(
          initId,
          Some(Seq(ScheduleTriggerMessage(ActivityStartTrigger(0L), emAgent)))
        )
      )

      // TICK 0

      val activationId1 = 1

      scheduler.send(
        emAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(0L),
          activationId1,
          emAgent
        )
      )

      weatherService.send(
        pvAgent,
        ProvideWeatherMessage(
          0L,
          WeatherData(
            Quantities.getQuantity(200d, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(100d, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(0d, CELSIUS),
            Quantities.getQuantity(0d, METRE_PER_SECOND)
          ),
          Some(900L)
        )
      )

      scheduler.expectMsg(
        CompletionMessage(
          activationId1,
          Some(List(ScheduleTriggerMessage(ActivityStartTrigger(900), emAgent)))
        )
      )

    }
  }
}
