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
import edu.ie3.datamodel.models.result.system.EmResult
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.em.EmAgent
import edu.ie3.simona.agent.participant.em.EmAgent.EmAgentInitializeStateData
import edu.ie3.simona.agent.participant.load.LoadAgent.FixedLoadAgent
import edu.ie3.simona.agent.participant.pv.PvAgent
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.agent.participant.storage.StorageAgent
import edu.ie3.simona.config.SimonaConfig.{
  LoadRuntimeConfig,
  PvRuntimeConfig,
  StorageRuntimeConfig
}
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.model.participant.em.PrioritizedFlexStrat
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
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.scalatestplus.mockito.MockitoSugar
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.{CELSIUS, METRE_PER_SECOND}

import java.time.ZonedDateTime

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

  // start a bit later so the sun is up
  protected implicit val simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01 10:00:00")
  protected val simulationEndDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-02 02:00:00")

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
        new PvAgent(
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
            Some(emAgent),
            scheduleTriggerEmFunc(loadAgent, emAgent)
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
              scaling = 2d,
              uuids = List.empty
            ),
            primaryServiceProxy.ref,
            Some(Vector(ActorWeatherService(weatherService.ref))),
            simulationStartDate,
            simulationEndDate,
            resolution,
            simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
            outputConfigOff,
            Some(emAgent),
            scheduleTriggerEmFunc(pvAgent, emAgent)
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
            householdStorageInput,
            StorageRuntimeConfig(
              calculateMissingReactivePowerWithModel = true,
              scaling = 1d,
              uuids = List.empty,
              initialSoc = 0d
            ),
            primaryServiceProxy.ref,
            None,
            simulationStartDate,
            simulationEndDate,
            resolution,
            simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
            outputConfigOff,
            Some(emAgent),
            scheduleTriggerEmFunc(storageAgent, emAgent)
          )
        )

      val connectedAgents = Seq(
        (loadAgent, loadAgentInit, loadInput),
        (pvAgent, pvAgentInit, pvInput),
        (storageAgent, storageAgentInit, householdStorageInput)
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
              connectedAgents = connectedAgents,
              modelStrategy = PrioritizedFlexStrat
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
        ) should contain allOf (loadInput.getUuid, pvInput.getUuid, householdStorageInput.getUuid)

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

      /* TICK 0
         LOAD: 0.000269 MW
         PV:  -0.005685 MW
         STORAGE: SOC 0 %
         -> charge with 5 kW
         -> remaining -0.0004161 MW
       */

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
            Quantities.getQuantity(400d, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(200d, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(0d, CELSIUS),
            Quantities.getQuantity(0d, METRE_PER_SECOND)
          ),
          Some(7200L)
        )
      )

      resultsListener.expectMsgType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 0L.toDateTime
          emResult.getP should equalWithTolerance(
            (-0.000416087825).asMegaWatt,
            tolerance
          )
          emResult.getQ should equalWithTolerance(
            0.0000882855367.asMegaVar,
            tolerance
          )
      }

      scheduler.expectMsg(
        CompletionMessage(
          activationId1,
          Some(
            List(ScheduleTriggerMessage(ActivityStartTrigger(7200L), emAgent))
          )
        )
      )

      /* TICK 7200
         LOAD: 0.000269 MW (unchanged)
         PV:  -0.003797 MW
         STORAGE: SOC 63.3 %
         -> charge with 3.5282 kW
         -> remaining 0 MW
       */

      val activationId2 = 2

      scheduler.send(
        emAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(7200L),
          activationId2,
          emAgent
        )
      )

      weatherService.send(
        pvAgent,
        ProvideWeatherMessage(
          7200L,
          WeatherData(
            Quantities.getQuantity(300d, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(500d, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(0d, CELSIUS),
            Quantities.getQuantity(0d, METRE_PER_SECOND)
          ),
          Some(14400L)
        )
      )

      resultsListener.expectMsgType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 7200L.toDateTime
          emResult.getP should equalWithTolerance(
            0.asMegaWatt,
            tolerance
          )
          emResult.getQ should equalWithTolerance(
            0.0000882855367.asMegaVar,
            tolerance
          )
      }

      scheduler.expectMsg(
        CompletionMessage(
          activationId2,
          Some(
            List(ScheduleTriggerMessage(ActivityStartTrigger(13107L), emAgent))
          )
        )
      )

      /* TICK 13107
         LOAD: 0.000269 MW (unchanged)
         PV:  -0.003797 MW (unchanged)
         STORAGE: SOC 100 %
         -> charge with 0 kW
         -> remaining -0.003528 MW
       */

      val activationId3 = 3

      scheduler.send(
        emAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(13107L),
          activationId3,
          emAgent
        )
      )

      resultsListener.expectMsgType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 13107L.toDateTime
          emResult.getP should equalWithTolerance(
            (-0.0035281545552).asMegaWatt,
            tolerance
          )
          emResult.getQ should equalWithTolerance(
            0.0000882855367.asMegaVar,
            tolerance
          )
      }

      scheduler.expectMsg(
        CompletionMessage(
          activationId3,
          Some(
            List(ScheduleTriggerMessage(ActivityStartTrigger(14400L), emAgent))
          )
        )
      )

      /* TICK 14400
         LOAD: 0.000269 MW (unchanged)
         PV:  -0.000066 MW
         STORAGE: SOC 100 %
         -> charge with -0.202956 kW
         -> remaining 0 MW
       */

      val activationId4 = 4

      scheduler.send(
        emAgent,
        TriggerWithIdMessage(
          ActivityStartTrigger(14400L),
          activationId4,
          emAgent
        )
      )

      // it got cloudy now...
      weatherService.send(
        pvAgent,
        ProvideWeatherMessage(
          14400L,
          WeatherData(
            Quantities.getQuantity(5d, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(5d, StandardUnits.SOLAR_IRRADIANCE),
            Quantities.getQuantity(0d, CELSIUS),
            Quantities.getQuantity(0d, METRE_PER_SECOND)
          ),
          Some(21600L)
        )
      )

      resultsListener.expectMsgType[ParticipantResultEvent] match {
        case ParticipantResultEvent(emResult: EmResult) =>
          emResult.getInputModel shouldBe emInput.getUuid
          emResult.getTime shouldBe 14400L.toDateTime
          emResult.getP should equalWithTolerance(
            0.asMegaWatt,
            tolerance
          )
          emResult.getQ should equalWithTolerance(
            0.000088285537.asMegaVar,
            tolerance
          )
      }

      scheduler.expectMsg(
        CompletionMessage(
          activationId4,
          Some(
            List(ScheduleTriggerMessage(ActivityStartTrigger(21600L), emAgent))
          )
        )
      )
    }
  }
}
