/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.em

import edu.ie3.datamodel.models.result.system.EmResult
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.hp.HpAgent
import edu.ie3.simona.agent.participant.load.LoadAgent.FixedLoadAgent
import edu.ie3.simona.agent.participant.pv.PvAgent
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.agent.participant.storage.StorageAgent
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
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.{
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.ontology.messages.services.WeatherMessage.{
  ProvideWeatherMessage,
  RegisterForWeatherMessage,
  WeatherData,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
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
import org.scalatest.OptionValues._
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.mockito.MockitoSugar
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
    simonaConfig.simona.powerflow.resolution.getSeconds

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
        val resultListener = TestProbe[ResultEvent]("ResultListener")
        val primaryServiceProxy =
          TestProbe[ServiceMessage]("PrimaryServiceProxy")
        val weatherService = TestProbe[ServiceMessage]("WeatherService")
        val scheduler = TestProbe[SchedulerMessage]("Scheduler")

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

        val loadAgent = TestActorRef(
          new FixedLoadAgent(
            scheduler = scheduler.ref.toClassic,
            initStateData = ParticipantInitializeStateData(
              loadInput,
              LoadRuntimeConfig(
                calculateMissingReactivePowerWithModel = true,
                modelBehaviour = "fix",
                reference = "power",
                uuids = List.empty,
              ),
              primaryServiceProxy.ref.toClassic,
              None,
              simulationStartDate,
              simulationEndDate,
              resolution,
              simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfigOff,
              Some(emAgent),
            ),
            listener = Iterable(resultListener.ref.toClassic),
          ),
          "LoadAgent",
        )
        val pvAgent = TestActorRef(
          new PvAgent(
            scheduler = scheduler.ref.toClassic,
            initStateData = ParticipantInitializeStateData(
              pvInput,
              PvRuntimeConfig(
                calculateMissingReactivePowerWithModel = true,
                uuids = List.empty,
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
          "PvAgent",
        )
        val storageAgent = TestActorRef(
          new StorageAgent(
            scheduler = scheduler.ref.toClassic,
            initStateData = ParticipantInitializeStateData(
              householdStorageInput,
              StorageRuntimeConfig(
                calculateMissingReactivePowerWithModel = true,
                uuids = List.empty,
                targetSoc = None,
              ),
              primaryServiceProxy.ref.toClassic,
              None,
              simulationStartDate,
              simulationEndDate,
              resolution,
              simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfigOff,
              Some(emAgent),
            ),
            listener = Iterable(resultListener.ref.toClassic),
          ),
          "StorageAgent",
        )

        scheduler.expectNoMessage()

        /* INIT */

        // load
        loadAgent ! Activation(INIT_SIM_TICK)

        primaryServiceProxy.expectMessage(
          PrimaryServiceRegistrationMessage(loadInput.getUuid)
        )
        loadAgent ! RegistrationFailedMessage(primaryServiceProxy.ref.toClassic)

        // the order of the two messages is not given
        val emAgentActivation = scheduler
          .receiveMessages(2)
          .flatMap {
            case Completion(ref, maybeNewTick) =>
              ref shouldBe loadAgent.toTyped
              maybeNewTick shouldBe None
              None
            case ScheduleActivation(ref, tick, unlockKey) =>
              // em agent schedules itself
              tick shouldBe 0
              unlockKey shouldBe None
              Some(ref)
            case unexpected =>
              fail(s"Received unexpected message $unexpected")
          }
          .headOption
          .value

        // pv
        pvAgent ! Activation(INIT_SIM_TICK)

        primaryServiceProxy.expectMessage(
          PrimaryServiceRegistrationMessage(pvInput.getUuid)
        )
        pvAgent ! RegistrationFailedMessage(primaryServiceProxy.ref.toClassic)

        // deal with weather service registration
        weatherService.expectMessage(
          RegisterForWeatherMessage(
            pvInput.getNode.getGeoPosition.getY,
            pvInput.getNode.getGeoPosition.getX,
          )
        )

        pvAgent ! RegistrationSuccessfulMessage(
          weatherService.ref.toClassic,
          Some(0L),
        )

        scheduler.expectMessage(Completion(pvAgent))

        // storage
        storageAgent ! Activation(INIT_SIM_TICK)

        primaryServiceProxy.expectMessage(
          PrimaryServiceRegistrationMessage(householdStorageInput.getUuid)
        )
        storageAgent ! RegistrationFailedMessage(
          primaryServiceProxy.ref.toClassic
        )

        scheduler.expectMessage(Completion(storageAgent))

        /* TICK 0
         LOAD: 0.269 kW
         PV:  -5.842 kW
         STORAGE: SOC 0 %
         -> charge with 5 kW
         -> remaining -0.573 kW
         */

        emAgentActivation ! Activation(0)

        pvAgent ! ProvideWeatherMessage(
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
              0.0000882855367033.asMegaVar
            )
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(7200)))

        /* TICK 7200
         LOAD: 0.269 kW (unchanged)
         PV:  -3.791 kW
         STORAGE: SOC 63.3 %
         -> charge with 3.522 kW
         -> remaining 0 kW
         */

        emAgentActivation ! Activation(7200)

        pvAgent ! ProvideWeatherMessage(
          7200,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(50d),
            WattsPerSquareMeter(150d),
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
            emResult.getQ should equalWithTolerance(0.0000882855367.asMegaVar)
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(13115)))

        /* TICK 13115
         LOAD: 0.269 kW (unchanged)
         PV:  -3.791 kW (unchanged)
         STORAGE: SOC 100 %
         -> charge with 0 kW
         -> remaining -3.522 kW
         */

        emAgentActivation ! Activation(13115)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 13115L.toDateTime
            emResult.getP should equalWithTolerance(
              -0.0035233186089842434.asMegaWatt
            )
            emResult.getQ should equalWithTolerance(0.0000882855367.asMegaVar)
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(14400)))

        /* TICK 14400
         LOAD: 0.269 kW (unchanged)
         PV:  -0.069 kW
         STORAGE: SOC 100 %
         -> discharge with 0.2 kW
         -> remaining 0.0 kW
         */

        // send weather data before activation, which can happen
        // it got cloudy now...
        pvAgent ! ProvideWeatherMessage(
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
            emResult.getTime shouldBe 14400L.toDateTime
            emResult.getP should equalWithTolerance(
              0.0.asMegaWatt
            )
            emResult.getQ should equalWithTolerance(0.000088285536.asMegaVar)
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(21600)))

      }
    }

    "having load, pv and heat pump agents connected" should {
      "be initialized correctly and run through some activations" in {
        val resultListener = TestProbe[ResultEvent]("ResultListener")
        val primaryServiceProxy =
          TestProbe[ServiceMessage]("PrimaryServiceProxy")
        val weatherService = TestProbe[ServiceMessage]("WeatherService")
        val scheduler = TestProbe[SchedulerMessage]("Scheduler")

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

        val loadAgent = TestActorRef(
          new FixedLoadAgent(
            scheduler = scheduler.ref.toClassic,
            initStateData = ParticipantInitializeStateData(
              loadInput,
              LoadRuntimeConfig(
                calculateMissingReactivePowerWithModel = true,
                modelBehaviour = "fix",
                reference = "power",
                uuids = List.empty,
              ),
              primaryServiceProxy.ref.toClassic,
              None,
              simulationStartDate,
              simulationEndDate,
              resolution,
              simonaConfig.simona.runtime.participant.requestVoltageDeviationThreshold,
              outputConfigOff,
              Some(emAgent),
            ),
            listener = Iterable(resultListener.ref.toClassic),
          ),
          "LoadAgent1",
        )
        val pvAgent = TestActorRef(
          new PvAgent(
            scheduler = scheduler.ref.toClassic,
            initStateData = ParticipantInitializeStateData(
              pvInput,
              PvRuntimeConfig(
                calculateMissingReactivePowerWithModel = true,
                uuids = List.empty,
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

        scheduler.expectNoMessage()

        /* INIT */

        // load
        loadAgent ! Activation(INIT_SIM_TICK)

        primaryServiceProxy.expectMessage(
          PrimaryServiceRegistrationMessage(loadInput.getUuid)
        )
        loadAgent ! RegistrationFailedMessage(primaryServiceProxy.ref.toClassic)

        // the order of the two messages is not given
        val emAgentActivation = scheduler
          .receiveMessages(2)
          .flatMap {
            case Completion(ref, maybeNewTick) =>
              ref shouldBe loadAgent.toTyped
              maybeNewTick shouldBe None
              None
            case ScheduleActivation(ref, tick, unlockKey) =>
              // em agent schedules itself
              tick shouldBe 0
              unlockKey shouldBe None
              Some(ref)
            case unexpected =>
              fail(s"Received unexpected message $unexpected")
          }
          .headOption
          .value

        // pv
        pvAgent ! Activation(INIT_SIM_TICK)

        primaryServiceProxy.expectMessage(
          PrimaryServiceRegistrationMessage(pvInput.getUuid)
        )
        pvAgent ! RegistrationFailedMessage(primaryServiceProxy.ref.toClassic)

        // deal with weather service registration
        weatherService.expectMessage(
          RegisterForWeatherMessage(
            pvInput.getNode.getGeoPosition.getY,
            pvInput.getNode.getGeoPosition.getX,
          )
        )

        pvAgent ! RegistrationSuccessfulMessage(
          weatherService.ref.toClassic,
          Some(0L),
        )

        scheduler.expectMessage(Completion(pvAgent))

        // heat pump
        heatPumpAgent ! Activation(INIT_SIM_TICK)

        primaryServiceProxy.expectMessage(
          PrimaryServiceRegistrationMessage(adaptedHpInputModel.getUuid)
        )
        heatPumpAgent ! RegistrationFailedMessage(
          primaryServiceProxy.ref.toClassic
        )

        weatherService.expectMessage(
          RegisterForWeatherMessage(
            adaptedHpInputModel.getNode.getGeoPosition.getY,
            adaptedHpInputModel.getNode.getGeoPosition.getX,
          )
        )

        heatPumpAgent ! RegistrationSuccessfulMessage(
          weatherService.ref.toClassic,
          Some(0L),
        )

        scheduler.expectMessage(Completion(heatPumpAgent))

        val weatherDependentAgents = Seq(pvAgent, heatPumpAgent)

        /* TICK 0
         LOAD: 0.269 kW
         PV:  -5.842 kW
         Heat pump: off, can be turned on or stay off
         -> set point ~3.5 kW (bigger than 50 % rated apparent power): turned on
         -> remaining -0.723 kW
         */

        emAgentActivation ! Activation(0)

        weatherDependentAgents.foreach {
          _ ! ProvideWeatherMessage(
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
              0.0010731200407782782.asMegaVar
            )
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(7200)))

        /* TICK 7200
         LOAD: 0.269 kW (unchanged)
         PV:  -3.791 kW
         Heat pump: running (turned on from last request), can also be turned off
         -> set point ~3.5 kW (bigger than 50 % rated apparent power): stays turned on with unchanged state
         -> remaining 1.327 kW
         */

        emAgentActivation ! Activation(7200)

        weatherDependentAgents.foreach {
          _ ! ProvideWeatherMessage(
            7200,
            weatherService.ref.toClassic,
            WeatherData(
              WattsPerSquareMeter(50d),
              WattsPerSquareMeter(150d),
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
              0.0013266813910157566.asMegaWatt
            )
            emResult.getQ should equalWithTolerance(
              0.0010731200407782782.asMegaVar
            )
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(12108)))

        /* TICK 12108
      LOAD: 0.269 kW (unchanged)
      PV:  -3.791 kW (unchanged)
      We stop here, since thermal house reached target temperature.
      Heat pump: Can still run, since there is surplus energy from PV, can still be turned off
      -> set point ~3.5 kW (bigger than 50 % rated apparent power): stays turned on with unchanged state
      -> remaining 1.327 kW
         */

        emAgentActivation ! Activation(12108)

        resultListener.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 12108.toDateTime
            emResult.getP should equalWithTolerance(
              0.0013266813910157566.asMegaWatt
            )
            emResult.getQ should equalWithTolerance(
              0.0010731200407782782.asMegaVar
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
          _ ! ProvideWeatherMessage(
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
            emResult.getTime shouldBe 14400L.toDateTime
            emResult.getP should equalWithTolerance(
              0.00019892577822992104.asMegaWatt
            )
            emResult.getQ should equalWithTolerance(
              0.0000882855367033582.asMegaVar
            )
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(21600)))

        /* TICK 21600
         LOAD: 0.269 kW (unchanged)
         PV:  -0.023 kW
         Heat pump: Is not running, can run or stay off
         -> flex signal is 0 MW: Heat pump is turned off
         -> remaining 0.245 kW
         */

        emAgentActivation ! Activation(21600)

        weatherDependentAgents.foreach {
          _ ! ProvideWeatherMessage(
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
              0.0002450436827011999.asMegaWatt
            )
            emResult.getQ should equalWithTolerance(
              0.0000882855367033582.asMegaVar
            )
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(28800)))
      }
    }

  }
}
