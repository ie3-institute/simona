/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.em

import edu.ie3.datamodel.models.result.system.EmResult
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.hp.HpAgent
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.agent.participant2.ParticipantAgent.{
  DataProvision,
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.agent.participant2.ParticipantAgentInit
import edu.ie3.simona.agent.participant2.ParticipantAgentInit.{
  ParticipantRefs,
  SimulationParameters,
}
import edu.ie3.simona.config.RuntimeConfig._
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.WeatherMessage.{
  RegisterForWeatherMessage,
  WeatherData,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.service.ServiceType
import edu.ie3.simona.test.common.input.EmInputTestData
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.QuantityMatchers.equalWithTolerance
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.WattsPerSquareMeter
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.scaladsl.adapter._
import org.apache.pekko.testkit.TestActorRef
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.mockito.MockitoSugar
import squants.Each
import squants.motion.MetersPerSecond
import squants.thermal.Celsius

import java.time.ZonedDateTime

class EmAgentIT
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with should.Matchers
    with EmInputTestData
    with MockitoSugar {
  // start a bit later so the sun is up
  protected implicit val simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T10:00:00Z")
  protected val simulationEndDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-02T02:00:00Z")

  private val resolution =
    simonaConfig.simona.powerflow.resolution.toSeconds

  private val simulationParams = SimulationParameters(
    expectedPowerRequestTick = Long.MaxValue,
    requestVoltageDeviationTolerance = Each(1e-14d),
    simulationStart = simulationStartDate,
    simulationEnd = simulationEndDate,
  )

  private val outputConfigOn = NotifierConfig(
    simulationResultInfo = true,
    powerRequestReply = false,
    flexResult = false,
  )

  private val outputConfigOff = NotifierConfig(
    simulationResultInfo = false,
    powerRequestReply = false,
    flexResult = false,
  )

  override protected val modelConfig: EmRuntimeConfig = EmRuntimeConfig(
    uuids = List("default"),
    aggregateFlex = "SELF_OPT",
  )

  private implicit val quantityTolerance: Double = 1e-10d

  private implicit val classicSystem: ActorSystem = system.toClassic

  "An em agent" when {
    "having load, pv and storage agents connected" should {
      "be initialized correctly and run through some activations" in {
        val gridAgent = TestProbe[GridAgent.Request]("GridAgent")
        val resultListener = TestProbe[ResultEvent]("ResultListener")
        val primaryServiceProxy =
          TestProbe[ServiceMessage]("PrimaryServiceProxy")
        val weatherService = TestProbe[ServiceMessage]("WeatherService")
        val scheduler = TestProbe[SchedulerMessage]("Scheduler")

        val participantRefs = ParticipantRefs(
          gridAgent = gridAgent.ref,
          primaryServiceProxy = primaryServiceProxy.ref.toClassic,
          services =
            Map(ServiceType.WeatherService -> weatherService.ref.toClassic),
          resultListener = Iterable(resultListener.ref),
        )

        val emAgent = spawn(
          EmAgent(
            emInput,
            modelConfig,
            outputConfigOn,
            "PRIORITIZED",
            simulationStartDate,
            parent = Left(scheduler.ref),
            listener = Iterable(resultListener.ref),
          ),
          "EmAgent",
        )

        val loadAgent = spawn(
          ParticipantAgentInit(
            loadInputContainer,
            LoadRuntimeConfig(),
            outputConfigOff,
            participantRefs,
            simulationParams,
            Right(emAgent),
          ),
          "LoadAgent",
        )
        val pvAgent = spawn(
          ParticipantAgentInit(
            pvInputContainer,
            PvRuntimeConfig(),
            outputConfigOff,
            participantRefs,
            simulationParams,
            Right(emAgent),
          ),
          "PvAgent",
        )
        val storageAgent = spawn(
          ParticipantAgentInit(
            storageInputContainer,
            StorageRuntimeConfig(),
            outputConfigOff,
            participantRefs,
            simulationParams,
            Right(emAgent),
          ),
          "StorageAgent",
        )

        val emInitSchedule = scheduler.expectMessageType[ScheduleActivation]
        emInitSchedule.tick shouldBe INIT_SIM_TICK
        val emAgentActivation = emInitSchedule.actor

        /* INIT */

        emAgentActivation ! Activation(INIT_SIM_TICK)

        primaryServiceProxy.receiveMessages(3) should contain allOf (
          PrimaryServiceRegistrationMessage(
            loadAgent.toClassic,
            loadInput.getUuid,
          ),
          PrimaryServiceRegistrationMessage(
            pvAgent.toClassic,
            pvInput.getUuid,
          ),
          PrimaryServiceRegistrationMessage(
            storageAgent.toClassic,
            storageInput.getUuid,
          )
        )

        // load
        loadAgent ! RegistrationFailedMessage(primaryServiceProxy.ref.toClassic)

        // pv
        pvAgent ! RegistrationFailedMessage(primaryServiceProxy.ref.toClassic)

        // deal with weather service registration
        weatherService.expectMessage(
          RegisterForWeatherMessage(
            pvAgent.toClassic,
            pvInput.getNode.getGeoPosition.getY,
            pvInput.getNode.getGeoPosition.getX,
          )
        )

        pvAgent ! RegistrationSuccessfulMessage(
          weatherService.ref.toClassic,
          0L,
        )

        // storage
        storageAgent ! RegistrationFailedMessage(
          primaryServiceProxy.ref.toClassic
        )

        scheduler.expectMessage(Completion(emAgentActivation, Some(0)))

        /* TICK 0
         LOAD: 0.269 kW
         PV:  -5.842 kW
         STORAGE: SOC 0 %
         -> charge with 5 kW
         -> remaining -0.573 kW
         */

        emAgentActivation ! Activation(0)

        pvAgent ! DataProvision(
          0,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(200d),
            WattsPerSquareMeter(100d),
            Celsius(0d),
            MetersPerSecond(0d),
          ),
          Some(7200),
        )

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 0L.toDateTime
            emResult.getP should equalWithTolerance(
              -0.00057340027059.asMegaWatt
            )
            emResult.getQ should equalWithTolerance(
              -0.0018318880807426897.asMegaVar
            )
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(7200)))

        /* TICK 7200
         LOAD: 0.269 kW (unchanged)
         PV:  -3.715 kW
         STORAGE: SOC 63.3 %
         -> charge with 3.522 kW
         -> remaining 0 kW
         */

        emAgentActivation ! Activation(7200)

        pvAgent ! DataProvision(
          7200,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(45d),
            WattsPerSquareMeter(140d),
            Celsius(0d),
            MetersPerSecond(0d),
          ),
          Some(14400),
        )

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 7200.toDateTime
            emResult.getP should equalWithTolerance(
              0.0.asMegaWatt
            )
            emResult.getQ should equalWithTolerance(
              -0.001132927019679857.asMegaVar
            )
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(13246)))

        /* TICK 13246
         LOAD: 0.269 kW (unchanged)
         PV:  -3.715 kW (unchanged)
         STORAGE: SOC 100 %
         -> charge with 0 kW
         -> remaining -3.447 kW
         */

        emAgentActivation ! Activation(13246)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 13246.toDateTime
            emResult.getP should equalWithTolerance(
              -0.0034468567291.asMegaWatt
            )
            emResult.getQ should equalWithTolerance(-0.001132927.asMegaVar)
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(14400)))

        /* TICK 14400
         LOAD: 0.269 kW (unchanged)
         PV:  -0.07 kW
         STORAGE: SOC 100 %
         -> discharge with 0.199 kW
         -> remaining 0.0 kW
         */

        // send weather data before activation, which can happen
        // it got cloudy now...
        pvAgent ! DataProvision(
          14400,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(0.5d),
            WattsPerSquareMeter(2d),
            Celsius(0d),
            MetersPerSecond(0d),
          ),
          Some(21600),
        )

        emAgentActivation ! Activation(14400)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 14400.toDateTime
            emResult.getP should equalWithTolerance(
              0.0.asMegaWatt
            )
            emResult.getQ should equalWithTolerance(0.000065375.asMegaVar)
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(21600)))

      }
    }

    "having load, pv and heat pump agents connected" should {
      "be initialized correctly and run through some activations" in {
        val gridAgent = TestProbe[GridAgent.Request]("GridAgent")
        val resultListener = TestProbe[ResultEvent]("ResultListener")
        val primaryServiceProxy =
          TestProbe[ServiceMessage]("PrimaryServiceProxy")
        val weatherService = TestProbe[ServiceMessage]("WeatherService")
        val scheduler = TestProbe[SchedulerMessage]("Scheduler")

        val participantRefs = ParticipantRefs(
          gridAgent = gridAgent.ref,
          primaryServiceProxy = primaryServiceProxy.ref.toClassic,
          services =
            Map(ServiceType.WeatherService -> weatherService.ref.toClassic),
          resultListener = Iterable(resultListener.ref),
        )

        val emAgent = spawn(
          EmAgent(
            emInput,
            modelConfig,
            outputConfigOn,
            "PRIORITIZED",
            simulationStartDate,
            parent = Left(scheduler.ref),
            listener = Iterable(resultListener.ref),
          ),
          "EmAgent1",
        )

        val loadAgent = spawn(
          ParticipantAgentInit(
            loadInputContainer,
            LoadRuntimeConfig(),
            outputConfigOff,
            participantRefs,
            simulationParams,
            Right(emAgent),
          ),
          "LoadAgent1",
        )
        val pvAgent = spawn(
          ParticipantAgentInit(
            pvInputContainer,
            PvRuntimeConfig(),
            outputConfigOff,
            participantRefs,
            simulationParams,
            Right(emAgent),
          ),
          "PvAgent1",
        )
        val heatPumpAgent = TestActorRef(
          new HpAgent(
            scheduler = scheduler.ref.toClassic,
            initStateData = ParticipantInitializeStateData(
              adaptedHpInputModel,
              adaptedThermalGrid,
              HpRuntimeConfig(
                calculateMissingReactivePowerWithModel = true,
                1.0,
                List.empty[String],
              ),
              primaryServiceProxy.ref.toClassic,
              Iterable(ActorWeatherService(weatherService.ref.toClassic)),
              simulationStartDate,
              simulationEndDate,
              resolution,
              simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfigOff,
              Some(emAgent),
            ),
            listener = Iterable(resultListener.ref.toClassic),
          ),
          "HeatPumpAgent1",
        )

        val emInitSchedule = scheduler.expectMessageType[ScheduleActivation]
        emInitSchedule.tick shouldBe INIT_SIM_TICK
        val emAgentActivation = emInitSchedule.actor

        /* INIT */

        emAgentActivation ! Activation(INIT_SIM_TICK)

        primaryServiceProxy.receiveMessages(2) should contain allOf (
          PrimaryServiceRegistrationMessage(
            loadAgent.toClassic,
            loadInput.getUuid,
          ),
          PrimaryServiceRegistrationMessage(
            pvAgent.toClassic,
            pvInput.getUuid,
          )
        )

        // load
        loadAgent ! RegistrationFailedMessage(primaryServiceProxy.ref.toClassic)

        // pv
        pvAgent ! RegistrationFailedMessage(primaryServiceProxy.ref.toClassic)

        // deal with weather service registration
        weatherService.expectMessage(
          RegisterForWeatherMessage(
            pvAgent.toClassic,
            pvInput.getNode.getGeoPosition.getY,
            pvInput.getNode.getGeoPosition.getX,
          )
        )

        pvAgent ! RegistrationSuccessfulMessage(
          weatherService.ref.toClassic,
          0L,
        )

        scheduler.expectMessage(Completion(emAgentActivation, Some(0)))

        // heat pump
        heatPumpAgent ! Activation(INIT_SIM_TICK)

        primaryServiceProxy.expectMessage(
          PrimaryServiceRegistrationMessage(
            heatPumpAgent.ref,
            adaptedHpInputModel.getUuid,
          )
        )
        heatPumpAgent ! RegistrationFailedMessage(
          primaryServiceProxy.ref.toClassic
        )

        weatherService.expectMessage(
          RegisterForWeatherMessage(
            heatPumpAgent.ref,
            adaptedHpInputModel.getNode.getGeoPosition.getY,
            adaptedHpInputModel.getNode.getGeoPosition.getX,
          )
        )

        heatPumpAgent ! RegistrationSuccessfulMessage(
          weatherService.ref.toClassic,
          0L,
        )

        scheduler.expectMessage(Completion(heatPumpAgent))

        val weatherDependentAgents = Seq(pvAgent.toClassic, heatPumpAgent)

        /* TICK 0
         LOAD: 0.269 kW
         PV:  -5.842 kW
         Heat pump: off, can be turned on or stay off
         -> set point ~3.5 kW (bigger than 50 % rated apparent power): turned on
         -> remaining -0.723 kW
         */

        emAgentActivation ! Activation(0)

        weatherDependentAgents.foreach {
          _ ! DataProvision(
            0,
            weatherService.ref.toClassic,
            WeatherData(
              WattsPerSquareMeter(200d),
              WattsPerSquareMeter(100d),
              Celsius(0d),
              MetersPerSecond(0d),
            ),
            Some(7200),
          )
        }

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 0.toDateTime
            emResult.getP should equalWithTolerance(
              -0.0007234002705905523.asMegaWatt
            )
            emResult.getQ should equalWithTolerance(
              -0.0008470535766677697.asMegaVar
            )
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(7200)))

        /* TICK 7200
         LOAD: 0.269 kW (unchanged)
         PV:  -3.715 kW
         Heat pump: running (turned on from last request), can also be turned off
         -> set point ~3.5 kW (bigger than 50 % rated apparent power): stays turned on with unchanged state
         -> remaining 1.403 kW
         */

        emAgentActivation ! Activation(7200)

        weatherDependentAgents.foreach {
          _ ! DataProvision(
            7200,
            weatherService.ref.toClassic,
            WeatherData(
              WattsPerSquareMeter(45d),
              WattsPerSquareMeter(140d),
              Celsius(0d),
              MetersPerSecond(0d),
            ),
            Some(14400),
          )
        }

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 7200.toDateTime
            emResult.getP should equalWithTolerance(
              0.0014031432709.asMegaWatt
            )
            emResult.getQ should equalWithTolerance(
              -0.0001480925156.asMegaVar
            )
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(14400)))

        /* TICK 14400
         LOAD: 0.269 kW (unchanged)
         PV:  -0.07 kW
         Heat pump: Is still running, can still be turned off
         -> flex signal is 0 MW: Heat pump is turned off
         -> remaining 0.199 kW
         */

        emAgentActivation ! Activation(14400)

        // it got cloudy now...
        weatherDependentAgents.foreach {
          _ ! DataProvision(
            14400,
            weatherService.ref.toClassic,
            WeatherData(
              WattsPerSquareMeter(0.5d),
              WattsPerSquareMeter(2d),
              Celsius(0d),
              MetersPerSecond(0d),
            ),
            Some(21600),
          )
        }

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 14400.toDateTime
            emResult.getP should equalWithTolerance(
              0.0001988993578.asMegaWatt
            )
            emResult.getQ should equalWithTolerance(
              0.000065375.asMegaVar
            )
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(21600)))

        /* TICK 21600
         LOAD: 0.269 kW (unchanged)
         PV:  -0.024 kW
         Heat pump: Is not running, can run or stay off
         -> flex signal is 0 MW: Heat pump is turned off
         -> remaining 0.244 kW
         */

        emAgentActivation ! Activation(21600)

        weatherDependentAgents.foreach {
          _ ! DataProvision(
            21600,
            weatherService.ref.toClassic,
            WeatherData(
              // Same irradiation, but different angle of the sun
              WattsPerSquareMeter(2d),
              WattsPerSquareMeter(4d),
              Celsius(0d),
              MetersPerSecond(0d),
            ),
            Some(28800),
          )
        }

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 21600.toDateTime
            emResult.getP should equalWithTolerance(
              0.0002442471208.asMegaWatt
            )
            emResult.getQ should equalWithTolerance(
              0.00008028014.asMegaVar
            )
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(28800)))
      }
    }

    "having a pv and a load agent connected" should {
      "have correct values also for agents with limited operation time" in {
        val gridAgent = TestProbe[GridAgent.Request]("GridAgent")
        val resultListener = TestProbe[ResultEvent]("ResultListener")
        val primaryServiceProxy =
          TestProbe[ServiceMessage]("PrimaryServiceProxy")
        val weatherService = TestProbe[ServiceMessage]("WeatherService")
        val scheduler = TestProbe[SchedulerMessage]("Scheduler")

        val participantRefs = ParticipantRefs(
          gridAgent = gridAgent.ref,
          primaryServiceProxy = primaryServiceProxy.ref.toClassic,
          services =
            Map(ServiceType.WeatherService -> weatherService.ref.toClassic),
          resultListener = Iterable(resultListener.ref),
        )

        val emAgent = spawn(
          EmAgent(
            emInput,
            modelConfig,
            outputConfigOn,
            "PRIORITIZED",
            simulationStartDate,
            parent = Left(scheduler.ref),
            listener = Iterable(resultListener.ref),
          ),
          "EmAgentReactivePower",
        )
        val pvAgent = spawn(
          ParticipantAgentInit(
            pvInputContainerWithQCharacteristicLimitedOperationTime,
            PvRuntimeConfig(calculateMissingReactivePowerWithModel = true),
            outputConfigOff,
            participantRefs,
            simulationParams,
            Right(emAgent),
          ),
          "PvAgentReactivePower",
        )

        val loadAgent = spawn(
          ParticipantAgentInit(
            loadInputContainerWithLimitedOperationTime,
            LoadRuntimeConfig(calculateMissingReactivePowerWithModel = true),
            outputConfigOff,
            participantRefs,
            simulationParams,
            Right(emAgent),
          ),
          "LoadAgentReactivePower",
        )

        val emInitSchedule = scheduler.expectMessageType[ScheduleActivation]
        emInitSchedule.tick shouldBe INIT_SIM_TICK
        val emAgentActivation = emInitSchedule.actor

        /* INIT */

        emAgentActivation ! Activation(INIT_SIM_TICK)

        // load
        loadAgent ! RegistrationFailedMessage(primaryServiceProxy.ref.toClassic)

        // pv
        pvAgent ! RegistrationFailedMessage(primaryServiceProxy.ref.toClassic)

        primaryServiceProxy.receiveMessages(2) should contain allOf (
          PrimaryServiceRegistrationMessage(
            loadAgent.toClassic,
            loadInputWithLimitedOperationTime.getUuid,
          ),
          PrimaryServiceRegistrationMessage(
            pvAgent.toClassic,
            pvInputWithQCharacteristicLimitedOperationTime.getUuid,
          )
        )

        // load
        loadAgent ! RegistrationFailedMessage(primaryServiceProxy.ref.toClassic)

        // pv
        pvAgent ! RegistrationFailedMessage(primaryServiceProxy.ref.toClassic)

        // deal with weather service registration
        weatherService.expectMessage(
          RegisterForWeatherMessage(
            pvAgent.toClassic,
            pvInputWithQCharacteristicLimitedOperationTime.getNode.getGeoPosition.getY,
            pvInputWithQCharacteristicLimitedOperationTime.getNode.getGeoPosition.getX,
          )
        )

        pvAgent ! RegistrationSuccessfulMessage(
          weatherService.ref.toClassic,
          0L,
        )

        scheduler.expectMessage(Completion(emAgentActivation, Some(0)))

        val weatherDependentAgents = Seq(pvAgent)

        /* TICK 0
         Load: 282.74 VA, cosPhi: 0.95, P: 268.603 W, Q: 88.2855 var
         PV:  0 kW (not yet in operation)
         -> expect load p and q values as em p and q values
         */

        emAgentActivation ! Activation(0)

        weatherDependentAgents.foreach {
          _ ! DataProvision(
            0,
            weatherService.ref.toClassic,
            WeatherData(
              WattsPerSquareMeter(0d),
              WattsPerSquareMeter(0d),
              Celsius(0d),
              MetersPerSecond(0d),
            ),
            Some(3600),
          )
        }

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 0.toDateTime
            emResult.getP should equalWithTolerance(
              0.000268603.asMegaWatt
            )
            emResult.getQ should equalWithTolerance(0.0000882855367.asMegaVar)
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(3600)))

        /* TICK 3600
         Load: P: 268.603 W, Q: 88.2855 var (unchanged)
         PV:  P: 0 W  Q: 0 Var (in operation, but no sun)
         -> expect load p and q values as em p and q values
         */

        emAgentActivation ! Activation(3600)

        weatherDependentAgents.foreach {
          _ ! DataProvision(
            3600,
            weatherService.ref.toClassic,
            WeatherData(
              WattsPerSquareMeter(0d),
              WattsPerSquareMeter(0d),
              Celsius(0d),
              MetersPerSecond(0d),
            ),
            Some(7200),
          )
        }

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 3600.toDateTime
            emResult.getP should equalWithTolerance(0.000268603.asMegaWatt)
            emResult.getQ should equalWithTolerance(0.0000882855367.asMegaVar)
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(7200)))

        /* TICK 7200
         Load: P: 268.603 W, Q: 88.2855 var (unchanged)
         PV:  P: -8692.167 W  Q: -2856.98 var
         -> expect P:-8423.564 Q: -2768.69 var
         */

        weatherDependentAgents.foreach {
          _ ! DataProvision(
            7200,
            weatherService.ref.toClassic,
            WeatherData(
              WattsPerSquareMeter(300d),
              WattsPerSquareMeter(200d),
              Celsius(0d),
              MetersPerSecond(0d),
            ),
            Some(21800),
          )
        }

        emAgentActivation ! Activation(7200)
        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 7200.toDateTime
            emResult.getP should equalWithTolerance(-0.008423564.asMegaWatt)
            emResult.getQ should equalWithTolerance(
              -0.0027686916118040607.asMegaVar
            )
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(10800)))

        /* TICK 10800
        LOAD: P: 0 W, Q: 0 var (limited OperationTime)
        PV:  P: -8692.167 W  Q: -2856.98 var
        -> expect P and Q values of PV
         */

        emAgentActivation ! Activation(10800)
        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 10800.toDateTime
            emResult.getP should equalWithTolerance(-0.008692167.asMegaWatt)
            emResult.getQ should equalWithTolerance(
              -0.002856977148.asMegaVar
            )
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(14400)))

        /* TICK 14400
        LOAD: P: 0 W, Q: 0 var (limited OperationTime)
        PV: P: 0 W, Q: 0 var (limited OperationTime)
        -> expect P: 0 W Q: 0 var
         */

        emAgentActivation ! Activation(14400)
        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 14400.toDateTime
            emResult.getP should equalWithTolerance(0.asMegaWatt)
            emResult.getQ should equalWithTolerance(0.asMegaVar)
        }

        scheduler.expectMessage(Completion(emAgentActivation, None))

      }
    }
  }
}
