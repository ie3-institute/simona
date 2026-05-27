/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.em

import edu.ie3.datamodel.models.result.system.EmResult
import edu.ie3.simona.agent.participant.ParticipantAgentInit
import edu.ie3.simona.agent.participant.ParticipantAgentInit.{
  ParticipantRefs,
  SimulationParameters,
}
import edu.ie3.simona.config.RuntimeConfig.*
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.*
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.Data.SecondaryData.WeatherData
import edu.ie3.simona.service.{DataTimeType, ServiceType}
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.service.results.ResultServiceProxy.{
  ExpectResult,
  NoResult,
}
import edu.ie3.simona.service.weather.WeatherService.WeatherRegistrationData
import edu.ie3.simona.service.weather.WeatherService
import edu.ie3.simona.test.common.{TestSpawnerTyped, UnitSpec}
import edu.ie3.simona.test.common.input.EmInputTestData
import edu.ie3.simona.util.Coordinate
import edu.ie3.simona.util.SimonaConstants.{INIT_SIM_TICK, PRE_INIT_TICK}
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.QuantityUtils.*
import edu.ie3.util.scala.quantities.WattsPerSquareMeter
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.scalatestplus.mockito.MockitoSugar
import squants.Each
import squants.motion.MetersPerSecond
import squants.thermal.Celsius

import java.time.ZonedDateTime

class EmAgentIT
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with EmInputTestData
    with MockitoSugar
    with TestSpawnerTyped {

  // start a bit later so the sun is up
  protected given simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T10:00:00Z")
  protected val simulationEndDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-02T02:00:00Z")

  given simulationParams: SimulationParameters = SimulationParameters(
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

  private val modelConfig: EmRuntimeConfig = EmRuntimeConfig(
    uuids = List("default"),
    aggregateFlex = "SELF_OPT",
  )

  private given quantityTolerance: Double = 1e-10d

  "An em agent" when {
    "having load, pv and storage agents connected" should {
      "be initialized correctly and run through some activations" in {
        val resultServiceProxy =
          TestProbe[ResultEvent | ExpectResult | NoResult]("ResultServiceProxy")
        val primaryServiceProxy =
          TestProbe[PrimaryServiceProxy.Message]("PrimaryServiceProxy")
        val weatherService = TestProbe[WeatherService.Message]("WeatherService")
        val scheduler = TestProbe[SchedulerMessage]("Scheduler")

        given ParticipantRefs = ParticipantRefs(
          primaryServiceProxy = primaryServiceProxy.ref,
          resultServiceProxy = resultServiceProxy.ref,
          services = Map(ServiceType.WeatherService -> weatherService.ref),
        )

        val keys = ScheduleLock
          .multiKey(TSpawner, scheduler.ref, PRE_INIT_TICK, 3)
          .iterator
        val lockActivation =
          scheduler.expectMessageType[ScheduleActivation].actor
        lockActivation ! Activation(PRE_INIT_TICK)

        val emAgent = spawn(
          EmAgentInit(
            emInput,
            modelConfig,
            outputConfigOn,
            simulationStartDate,
            parent = Left(scheduler.ref),
            listener = resultServiceProxy.ref,
          ),
          "EmAgent",
        )

        val loadAgent = spawn(
          ParticipantAgentInit(
            loadInputContainer,
            LoadRuntimeConfig(),
            outputConfigOff,
            Right(emAgent),
            keys.next(),
          ),
          "LoadAgent",
        )
        val pvAgent = spawn(
          ParticipantAgentInit(
            pvInputContainer,
            PvRuntimeConfig(),
            outputConfigOff,
            Right(emAgent),
            keys.next(),
          ),
          "PvAgent",
        )
        val storageAgent = spawn(
          ParticipantAgentInit(
            storageInputContainer,
            StorageRuntimeConfig(),
            outputConfigOff,
            Right(emAgent),
            keys.next(),
          ),
          "StorageAgent",
        )

        val emInitSchedule = scheduler.expectMessageType[ScheduleActivation]
        emInitSchedule.tick shouldBe INIT_SIM_TICK
        val emAgentActivation = emInitSchedule.actor

        scheduler.expectNoMessage()

        emInitSchedule.unlockKey.value.unlock()
        scheduler.expectMessage(Completion(lockActivation))

        /* INIT */
        emAgentActivation ! Activation(INIT_SIM_TICK)

        primaryServiceProxy.receiveMessages(3) should contain allOf (
          PrimaryServiceRegistrationMessage(
            loadAgent,
            loadInput.getUuid,
          ),
          PrimaryServiceRegistrationMessage(
            pvAgent,
            pvInput.getUuid,
          ),
          PrimaryServiceRegistrationMessage(
            storageAgent,
            storageInput.getUuid,
          )
        )

        // load
        loadAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

        // pv
        pvAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

        // deal with weather service registration
        weatherService.expectMessage(
          SecondaryServiceRegistrationMessage(
            pvAgent,
            DataTimeType.Current,
            WeatherRegistrationData(
              Coordinate(
                pvInput.getNode.getGeoPosition.getY,
                pvInput.getNode.getGeoPosition.getX,
              )
            ),
          )
        )

        pvAgent ! RegistrationSuccessfulMessage(weatherService.ref, 0L)

        // storage
        storageAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

        scheduler.expectMessage(Completion(emAgentActivation, Some(0)))

        /* TICK 0
         LOAD: 0.269 kW
         PV:  -5.842 kW
         STORAGE: SOC 0 %
         -> charge with 5 kW
         -> remaining -0.573 kW
         */
        emAgentActivation ! Activation(0)

        // we receive a message for each agent that is not waiting for secondary data
        resultServiceProxy.receiveMessages(2) should contain allOf (
          ExpectResult(storageInput.getUuid, 0, true),
          ExpectResult(loadInput.getUuid, 0, true)
        )

        pvAgent ! DataProvision(
          0,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(200d),
            WattsPerSquareMeter(100d),
            Celsius(0d),
            MetersPerSecond(0d),
            None,
            None,
          ),
          Some(7200),
        )

        resultServiceProxy.receiveMessages(4) should contain allOf (
          // we receive a message, since new data arrived
          ExpectResult(pvInput.getUuid, 0, true),
          // we receive update messages, since new set points were provided
          ExpectResult(pvInput.getUuid, 0),
          ExpectResult(storageInput.getUuid, 0),
          ExpectResult(loadInput.getUuid, 0)
        )

        resultServiceProxy.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 0L.toDateTime
            emResult.getP should equalWithTolerance(-0.00057340027.asMegaWatt)
            emResult.getQ should equalWithTolerance(-0.0018318880807.asMegaVar)
        }
        resultServiceProxy.expectNoMessage()
        scheduler.expectMessage(Completion(emAgentActivation, Some(7200)))

        /* TICK 7200
         LOAD: 0.269 kW (unchanged)
         PV:  -3.715 kW
         STORAGE: SOC 63.3 %
         -> charge with 3.522 kW
         -> remaining 0 kW
         */
        emAgentActivation ! Activation(7200)

        // the result proxy will receive ExpectResult messages
        resultServiceProxy.expectMessage(
          ExpectResult(storageInput.getUuid, 7200, true)
        )

        pvAgent ! DataProvision(
          7200,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(45d),
            WattsPerSquareMeter(140d),
            Celsius(0d),
            MetersPerSecond(0d),
            None,
            None,
          ),
          Some(14400),
        )

        resultServiceProxy.receiveMessages(4) should contain allOf (
          // we receive a message, since new data arrived
          ExpectResult(pvInput.getUuid, 7200, true),
          // expect no result, since we are still waiting for a new set point
          NoResult(storageInput.getUuid, 7200),
          // we expect results, since we received new set points
          ExpectResult(pvInput.getUuid, 7200),
          ExpectResult(storageInput.getUuid, 7200)
        )

        resultServiceProxy.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 7200.toDateTime
            emResult.getP should equalWithTolerance(0.asMegaWatt)
            emResult.getQ should equalWithTolerance(-0.00113292701968.asMegaVar)
        }
        resultServiceProxy.expectNoMessage()
        scheduler.expectMessage(Completion(emAgentActivation, Some(13246)))

        /* TICK 13246
         LOAD: 0.269 kW (unchanged)
         PV:  -3.715 kW (unchanged)
         STORAGE: SOC 100 %
         -> charge with 0 kW
         -> remaining -3.447 kW
         */
        emAgentActivation ! Activation(13246)

        resultServiceProxy.receiveMessages(2) should contain allOf (
          // the result proxy will receive ExpectResult messages
          ExpectResult(storageInput.getUuid, 13246, true),
          // we receive an update message, since a new set point were provided
          ExpectResult(storageInput.getUuid, 13246)
        )

        resultServiceProxy.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 13246.toDateTime
            emResult.getP should equalWithTolerance(-0.00344685673.asMegaWatt)
            emResult.getQ should equalWithTolerance(-0.001132927.asMegaVar)
        }
        resultServiceProxy.expectNoMessage()
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
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(0.5d),
            WattsPerSquareMeter(2d),
            Celsius(0d),
            MetersPerSecond(0d),
            None,
            None,
          ),
          Some(21600),
        )

        emAgentActivation ! Activation(14400)

        // we receive update messages, since we received an activation and a new set point was provided
        resultServiceProxy.receiveMessages(3) should contain allOf (
          ExpectResult(pvInput.getUuid, 14400, true),
          ExpectResult(pvInput.getUuid, 14400),
          ExpectResult(storageInput.getUuid, 14400)
        )

        resultServiceProxy.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 14400.toDateTime
            emResult.getP should equalWithTolerance(0.asMegaWatt)
            emResult.getQ should equalWithTolerance(0.000065375.asMegaVar)
        }
        resultServiceProxy.expectNoMessage()
        scheduler.expectMessage(Completion(emAgentActivation, Some(21600)))
      }
    }

    "having load, pv and heat pump agents connected" should {
      "be initialized correctly and run through some activations" in {
        val resultServiceProxy =
          TestProbe[ResultEvent | ExpectResult | NoResult]("ResultServiceProxy")
        val primaryServiceProxy =
          TestProbe[PrimaryServiceProxy.Message]("PrimaryServiceProxy")
        val weatherService = TestProbe[WeatherService.Message]("WeatherService")
        val scheduler = TestProbe[SchedulerMessage]("Scheduler")

        given ParticipantRefs = ParticipantRefs(
          primaryServiceProxy = primaryServiceProxy.ref,
          resultServiceProxy = resultServiceProxy.ref,
          services = Map(ServiceType.WeatherService -> weatherService.ref),
        )

        val keys = ScheduleLock
          .multiKey(TSpawner, scheduler.ref, PRE_INIT_TICK, 3)
          .iterator
        val lockActivation =
          scheduler.expectMessageType[ScheduleActivation].actor
        lockActivation ! Activation(PRE_INIT_TICK)

        val emAgent = spawn(
          EmAgentInit(
            emInput,
            modelConfig,
            outputConfigOn,
            simulationStartDate,
            parent = Left(scheduler.ref),
            listener = resultServiceProxy.ref,
          ),
          "EmAgent1",
        )

        val loadAgent = spawn(
          ParticipantAgentInit(
            loadInputContainer,
            LoadRuntimeConfig(),
            outputConfigOff,
            Right(emAgent),
            keys.next(),
          ),
          "LoadAgent1",
        )
        val pvAgent = spawn(
          ParticipantAgentInit(
            pvInputContainer,
            PvRuntimeConfig(),
            outputConfigOff,
            Right(emAgent),
            keys.next(),
          ),
          "PvAgent1",
        )
        val hpAgent = spawn(
          ParticipantAgentInit(
            withHeatContainerEmIT,
            HpRuntimeConfig(),
            outputConfigOff,
            Right(emAgent),
            keys.next(),
          ),
          "HeatPumpAgent1",
        )

        val emInitSchedule = scheduler.expectMessageType[ScheduleActivation]
        emInitSchedule.tick shouldBe INIT_SIM_TICK
        val emAgentActivation = emInitSchedule.actor

        scheduler.expectNoMessage()

        emInitSchedule.unlockKey.value.unlock()
        scheduler.expectMessage(Completion(lockActivation))

        /* INIT */
        emAgentActivation ! Activation(INIT_SIM_TICK)

        primaryServiceProxy.receiveMessages(3) should contain allOf (
          PrimaryServiceRegistrationMessage(
            hpAgent,
            hpInputModelEmIT.getUuid,
          ),
          PrimaryServiceRegistrationMessage(
            loadAgent,
            loadInput.getUuid,
          ),
          PrimaryServiceRegistrationMessage(
            pvAgent,
            pvInput.getUuid,
          )
        )

        // load
        loadAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

        // pv
        pvAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

        // deal with weather service registration
        weatherService.expectMessage(
          SecondaryServiceRegistrationMessage(
            pvAgent,
            DataTimeType.Current,
            WeatherRegistrationData(
              Coordinate(
                pvInput.getNode.getGeoPosition.getY,
                pvInput.getNode.getGeoPosition.getX,
              )
            ),
          )
        )

        pvAgent ! RegistrationSuccessfulMessage(weatherService.ref, 0L)

        // heat pump
        hpAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

        // deal with weather service registration
        weatherService.expectMessage(
          SecondaryServiceRegistrationMessage(
            hpAgent,
            DataTimeType.Current,
            WeatherRegistrationData(
              Coordinate(
                hpInputModelEmIT.getNode.getGeoPosition.getY,
                hpInputModelEmIT.getNode.getGeoPosition.getX,
              )
            ),
          )
        )

        hpAgent ! RegistrationSuccessfulMessage(weatherService.ref, 0L)

        scheduler.expectMessage(Completion(emAgentActivation, Some(0)))

        val weatherDependentAgents = Seq(pvAgent, hpAgent)

        /* TICK 0
         LOAD: 0.269 kW
         PV:  -5.842 kW
         Heat pump: off, can be turned on or stay off
         -> set point = 0 kW: stays off
         -> remaining -5.573 kW
         */
        emAgentActivation ! Activation(0)

        // the result proxy will receive ExpectResult messages
        resultServiceProxy.expectMessage(
          ExpectResult(loadInput.getUuid, 0, true)
        )

        weatherDependentAgents.foreach {
          _ ! DataProvision(
            0,
            weatherService.ref,
            WeatherData(
              WattsPerSquareMeter(200d),
              WattsPerSquareMeter(100d),
              Celsius(0d),
              MetersPerSecond(0d),
              None,
              None,
            ),
            Some(7200),
          )
        }

        resultServiceProxy.receiveMessages(5) should contain allOf (
          // we receive a message, since new data arrived
          ExpectResult(pvInput.getUuid, 0, true),
          ExpectResult(hpInputModelEmIT.getUuid, 0, true),
          // we receive update messages, since a new set point was provided
          ExpectResult(pvInput.getUuid, 0),
          ExpectResult(hpInputModelEmIT.getUuid, 0),
          ExpectResult(loadInput.getUuid, 0)
        )

        resultServiceProxy.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 0.toDateTime
            emResult.getP should equalWithTolerance(-0.0055734002706.asMegaWatt)
            emResult.getQ should equalWithTolerance(-0.0018318880807.asMegaVar)
        }
        resultServiceProxy.expectNoMessage()
        scheduler.expectMessage(Completion(emAgentActivation, Some(75)))

        /* TICK 75
         DomesticHotWaterStorage stopped discharging. Expect same behaviour as before
         LOAD: 0.269 kW (unchanged)
         PV:  -5.842 kW
         Heat pump: running (turned on from last request), can also be turned off
         -> set point ~3.5 kW (bigger than 50 % rated apparent power): stays turned on with unchanged state
         -> remaining -0.723 kW
         */
        emAgentActivation ! Activation(75)

        resultServiceProxy.receiveMessages(2) should contain allOf (
          // we receive a message, since new data arrived
          ExpectResult(hpInputModelEmIT.getUuid, 75, true),
          // we receive update messages, since a new set point was provided
          ExpectResult(hpInputModelEmIT.getUuid, 75)
        )

        resultServiceProxy.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 75.toDateTime
            emResult.getP should equalWithTolerance(-0.0055734002706.asMegaWatt)
            emResult.getQ should equalWithTolerance(-0.00183188808074.asMegaVar)
        }
        resultServiceProxy.expectNoMessage()
        scheduler.expectMessage(Completion(emAgentActivation, Some(3600)))

        /* TICK 3600
        DomesticHotWaterStorage stopped discharging. Expect same behaviour as before
        LOAD: 0.269 kW (unchanged)
        PV:  -3.715 kW
        Heat pump: running (turned on from last request), can also be turned off
        -> set point ~3.5 kW (bigger than 50 % rated apparent power): stays turned on with unchanged state
        -> remaining ~0.0 kW
         */
        emAgentActivation ! Activation(3600)

        resultServiceProxy.receiveMessages(2) should contain allOf (
          // we receive a message, since new data arrived
          ExpectResult(hpInputModelEmIT.getUuid, 3600, true),
          // we receive update messages, since a new set point was provided
          ExpectResult(hpInputModelEmIT.getUuid, 3600)
        )
        resultServiceProxy.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 3600.toDateTime
            emResult.getP should equalWithTolerance(-0.00072340027.asMegaWatt)
            emResult.getQ should equalWithTolerance(-0.00084705357667.asMegaVar)
        }
        resultServiceProxy.expectNoMessage()
        scheduler.expectMessage(Completion(emAgentActivation, Some(3675)))

        /* TICK 3675
        DomesticHotWaterStorage stopped discharging. Expect same behaviour as before
        LOAD: 0.269 kW (unchanged)
        PV:  -5.842 kW
        Heat pump: running (turned on from last request), can also be turned off
        -> set point ~3.5 kW (bigger than 50 % rated apparent power): stays turned on with unchanged state
        -> remaining -0.723 kW
         */
        emAgentActivation ! Activation(3675)

        resultServiceProxy.receiveMessages(2) should contain allOf (
          // we receive a message, since new data arrived
          ExpectResult(hpInputModelEmIT.getUuid, 3675, true),
          // we receive update messages, since a new set point was provided
          ExpectResult(hpInputModelEmIT.getUuid, 3675)
        )

        resultServiceProxy.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 3675.toDateTime
            emResult.getP should equalWithTolerance(
              -0.00072340027059.asMegaWatt
            )
            emResult.getQ should equalWithTolerance(
              -0.00084705357666777.asMegaVar
            )
        }
        resultServiceProxy.expectNoMessage()
        scheduler.expectMessage(Completion(emAgentActivation, Some(6056)))

        /* TICK 6056
        DomesticHotWaterStorage stopped discharging. Expect same behaviour as before
        LOAD: 0.269 kW (unchanged)
        PV:  -5.842 kW
        Heat pump: running (turned on from last request), can also be turned off
        -> set point ~3.5 kW (bigger than 50 % rated apparent power): stays turned on with unchanged state
        -> remaining -0.723 kW
         */
        emAgentActivation ! Activation(6056)

        resultServiceProxy.receiveMessages(2) should contain allOf (
          // we receive a message, since new data arrived
          ExpectResult(hpInputModelEmIT.getUuid, 6056, true),
          // we receive update messages, since a new set point was provided
          ExpectResult(hpInputModelEmIT.getUuid, 6056)
        )

        resultServiceProxy.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 6056.toDateTime
            emResult.getP should equalWithTolerance(-0.00557340027.asMegaWatt)
            emResult.getQ should equalWithTolerance(-0.00183188808074.asMegaVar)
        }
        resultServiceProxy.expectNoMessage()
        scheduler.expectMessage(Completion(emAgentActivation, Some(7200)))

        /* TICK 7200
         LOAD: 0.269 kW (unchanged)
         PV:  -3.715 kW
         Heat pump: off, can be turned on or stay off
         -> set point ~3.5 kW (bigger than 50 % rated apparent power): turned on
         -> remaining 1.403 kW
         */
        emAgentActivation ! Activation(7200)

        weatherDependentAgents.foreach {
          _ ! DataProvision(
            7200,
            weatherService.ref,
            WeatherData(
              WattsPerSquareMeter(45d),
              WattsPerSquareMeter(140d),
              Celsius(0d),
              MetersPerSecond(0d),
              None,
              None,
            ),
            Some(10800),
          )
        }

        resultServiceProxy.receiveMessages(4) should contain allOf (
          // we receive a message, since new data arrived
          ExpectResult(pvInput.getUuid, 7200, true),
          ExpectResult(hpInputModelEmIT.getUuid, 7200, true),
          // we receive update messages, since a new set point was provided
          ExpectResult(pvInput.getUuid, 7200),
          ExpectResult(hpInputModelEmIT.getUuid, 7200)
        )

        resultServiceProxy.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 7200.toDateTime
            emResult.getP should equalWithTolerance(0.001403143271.asMegaWatt)
            emResult.getQ should equalWithTolerance(-0.0001480925156.asMegaVar)
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(7278)))

        /* TICK 7278
         DomesticHotWaterStorage stopped discharging. Expect same behaviour as before
         LOAD: 0.269 kW (unchanged)
         PV:  -3.791 kW
         Heat pump: running (turned on from last request), can also be turned off
         -> set point ~3.5 kW (bigger than 50 % rated apparent power): stays turned on with unchanged state
         -> remaining 0 MW
         */
        emAgentActivation ! Activation(7278)

        resultServiceProxy.receiveMessages(2) should contain allOf (
          // we receive a message, since new data arrived
          ExpectResult(hpInputModelEmIT.getUuid, 7278, true),
          // we receive update messages, since a new set point was provided
          ExpectResult(hpInputModelEmIT.getUuid, 7278)
        )

        resultServiceProxy.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 7278.toDateTime
            emResult.getP should equalWithTolerance(0.00140314327091.asMegaWatt)
            emResult.getQ should equalWithTolerance(-0.00014809252.asMegaVar)
        }

        scheduler.expectMessage(Completion(emAgentActivation, Some(7981)))

        /* TICK 7981
         DomesticHotWaterStorage stopped discharging. Expect same behaviour as before
         LOAD: 0.269 kW (unchanged)
         PV:  -3.791 kW
         Heat pump: running (turned on from last request), can also be turned off
         -> set point ~3.5 kW (bigger than 50 % rated apparent power): stays turned on with unchanged state
         -> remaining 0 MW
         */
        emAgentActivation ! Activation(7981)

        resultServiceProxy.receiveMessages(2) should contain allOf (
          // we receive a message, since new data arrived
          ExpectResult(hpInputModelEmIT.getUuid, 7981, true),
          // we receive update messages, since a new set point was provided
          ExpectResult(hpInputModelEmIT.getUuid, 7981)
        )

        resultServiceProxy.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 7981.toDateTime
            emResult.getP should equalWithTolerance(-0.003446856729.asMegaWatt)
            emResult.getQ should equalWithTolerance(-0.00113292702.asMegaVar)
        }
        resultServiceProxy.expectNoMessage()
        scheduler.expectMessage(Completion(emAgentActivation, Some(10800)))

        /* TICK 10800
       LOAD: 0.269 kW (unchanged)
       PV:  -4.008 kW
       Heat pump: running (turned on from last request), can also be turned off
       -> set point ~3.7 kW (bigger than 50 % rated apparent power): stays turned on with unchanged state
       -> remaining 1.111 kW
         */
        emAgentActivation ! Activation(10800)

        weatherDependentAgents.foreach {
          _ ! DataProvision(
            10800,
            weatherService.ref,
            WeatherData(
              WattsPerSquareMeter(45d),
              WattsPerSquareMeter(140d),
              Celsius(0d),
              MetersPerSecond(0d),
              None,
              None,
            ),
            Some(11000),
          )
        }

        resultServiceProxy.receiveMessages(4) should contain allOf (
          // we receive a message, since new data arrived
          ExpectResult(pvInput.getUuid, 10800, true),
          ExpectResult(hpInputModelEmIT.getUuid, 10800, true),
          // we receive update messages, since a new set point was provided
          ExpectResult(pvInput.getUuid, 10800),
          ExpectResult(hpInputModelEmIT.getUuid, 10800)
        )

        resultServiceProxy.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 10800.toDateTime
            emResult.getP should equalWithTolerance(0.0011098586291.asMegaWatt)
            emResult.getQ should equalWithTolerance(-0.000244490516.asMegaVar)
        }
        resultServiceProxy.expectNoMessage()
        scheduler.expectMessage(Completion(emAgentActivation, Some(10879)))

        /* TICK 10879
        DomesticHotWaterStorage stopped discharging. Expect same behaviour as before
         LOAD: 0.269 kW (unchanged)
         PV:  -4.008 kW
         Heat pump: running (turned on from last request), can also be turned off
         -> set point ~3.7 kW (bigger than 50 % rated apparent power): stays turned on with unchanged state
         -> remaining 1.111 kW
         */
        emAgentActivation ! Activation(10879)

        resultServiceProxy.receiveMessages(2) should contain allOf (
          // we receive a message, since new data arrived
          ExpectResult(hpInputModelEmIT.getUuid, 10879, true),
          // we receive update messages, since a new set point was provided
          ExpectResult(hpInputModelEmIT.getUuid, 10879)
        )

        resultServiceProxy.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 10879.toDateTime
            emResult.getP should equalWithTolerance(0.0011098586291.asMegaWatt)
            emResult.getQ should equalWithTolerance(-0.000244490516.asMegaVar)
        }
        resultServiceProxy.expectNoMessage()
        scheduler.expectMessage(Completion(emAgentActivation, Some(11000)))

        /* TICK 11000
         LOAD: 0.269 kW (unchanged)
         PV:  -0.06 kW
         Heat pump: Is still running, can't be turned off
         (was running in last state, house has some demand, no storage available -> we would like to force running Hp,
         even in theory it could be turned off for flex purposes)
         -> flex signal is 4.85 kW: Heat pump stays on
         */
        emAgentActivation ! Activation(11000)

        // it got cloudy now...
        weatherDependentAgents.foreach {
          _ ! DataProvision(
            11000,
            weatherService.ref,
            WeatherData(
              WattsPerSquareMeter(0.5d),
              WattsPerSquareMeter(2d),
              Celsius(0d),
              MetersPerSecond(0d),
              None,
              None,
            ),
            Some(11500),
          )
        }

        resultServiceProxy.receiveMessages(4) should contain allOf (
          // we receive a message, since new data arrived
          ExpectResult(pvInput.getUuid, 11000, true),
          ExpectResult(hpInputModelEmIT.getUuid, 11000, true),
          // we receive update messages, since a new set point was provided
          ExpectResult(pvInput.getUuid, 11000),
          ExpectResult(hpInputModelEmIT.getUuid, 11000)
        )

        resultServiceProxy.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 11000.toDateTime
            emResult.getP should equalWithTolerance(0.0050603789402.asMegaWatt)
            emResult.getQ should equalWithTolerance(0.0010539827178.asMegaVar)
        }
        resultServiceProxy.expectNoMessage()
        scheduler.expectMessage(Completion(emAgentActivation, Some(11500)))

        /* TICK 11500
         LOAD: 0.269 kW (unchanged)
         PV:  -0.133 kW
         Heat pump: Is still running, can't be turned off
         (was running in last state, house has some demand, no storage available -> we would like to force running Hp,
         even in theory it could be turned off for flex purposes)
         -> flex signal is 4.85 kW: Heat pump stays on
         */
        emAgentActivation ! Activation(11500)

        weatherDependentAgents.foreach {
          _ ! DataProvision(
            11500,
            weatherService.ref,
            WeatherData(
              // Same irradiation, but different angle of the sun
              WattsPerSquareMeter(2d),
              WattsPerSquareMeter(4d),
              Celsius(0d),
              MetersPerSecond(0d),
              None,
              None,
            ),
            Some(28800),
          )
        }

        resultServiceProxy.receiveMessages(4) should contain allOf (
          // we receive a message, since new data arrived
          ExpectResult(pvInput.getUuid, 11500, true),
          ExpectResult(hpInputModelEmIT.getUuid, 11500, true),
          // we receive update messages, since a new set point was provided
          ExpectResult(pvInput.getUuid, 11500),
          ExpectResult(hpInputModelEmIT.getUuid, 11500)
        )

        resultServiceProxy.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 11500.toDateTime
            emResult.getP should equalWithTolerance(0.0049850525.asMegaWatt)
            emResult.getQ should equalWithTolerance(0.0010292241.asMegaVar)
        }
        resultServiceProxy.expectNoMessage()
        scheduler.expectMessage(Completion(emAgentActivation, Some(12725)))
      }
    }

    "having a pv and a load agent connected" should {
      "have correct values also for agents with limited operation time" in {
        val resultServiceProxy =
          TestProbe[ResultEvent | ExpectResult | NoResult]("ResultServiceProxy")
        val primaryServiceProxy =
          TestProbe[PrimaryServiceProxy.Message]("PrimaryServiceProxy")
        val weatherService = TestProbe[WeatherService.Message]("WeatherService")
        val scheduler = TestProbe[SchedulerMessage]("Scheduler")

        given ParticipantRefs = ParticipantRefs(
          primaryServiceProxy = primaryServiceProxy.ref,
          resultServiceProxy = resultServiceProxy.ref,
          services = Map(ServiceType.WeatherService -> weatherService.ref),
        )

        val keys = ScheduleLock
          .multiKey(TSpawner, scheduler.ref, PRE_INIT_TICK, 2)
          .iterator
        val lockActivation =
          scheduler.expectMessageType[ScheduleActivation].actor
        lockActivation ! Activation(PRE_INIT_TICK)

        val emAgent = spawn(
          EmAgentInit(
            emInput,
            modelConfig,
            outputConfigOn,
            simulationStartDate,
            parent = Left(scheduler.ref),
            listener = resultServiceProxy.ref,
          ),
          "EmAgentReactivePower",
        )

        val pvAgent = spawn(
          ParticipantAgentInit(
            pvInputContainerLimitedOperationTime,
            PvRuntimeConfig(calculateMissingReactivePowerWithModel = true),
            outputConfigOff,
            Right(emAgent),
            keys.next(),
          ),
          "PvAgentReactivePower",
        )
        val loadAgent = spawn(
          ParticipantAgentInit(
            loadInputContainerWithLimitedOperationTime,
            LoadRuntimeConfig(calculateMissingReactivePowerWithModel = true),
            outputConfigOff,
            Right(emAgent),
            keys.next(),
          ),
          "LoadAgentReactivePower",
        )

        val emInitSchedule = scheduler.expectMessageType[ScheduleActivation]
        emInitSchedule.tick shouldBe INIT_SIM_TICK
        val emAgentActivation = emInitSchedule.actor

        scheduler.expectNoMessage()

        emInitSchedule.unlockKey.value.unlock()
        scheduler.expectMessage(Completion(lockActivation))

        /* INIT */
        emAgentActivation ! Activation(INIT_SIM_TICK)

        // load
        loadAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

        // pv
        pvAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

        primaryServiceProxy.receiveMessages(2) should contain allOf (
          PrimaryServiceRegistrationMessage(
            loadAgent,
            loadInputWithLimitedOperationTime.getUuid,
          ),
          PrimaryServiceRegistrationMessage(
            pvAgent,
            pvInputLimitedOperationTime.getUuid,
          )
        )

        // load
        loadAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

        // pv
        pvAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

        // deal with weather service registration
        weatherService.expectMessage(
          SecondaryServiceRegistrationMessage(
            pvAgent,
            DataTimeType.Current,
            WeatherRegistrationData(
              Coordinate(
                pvInputLimitedOperationTime.getNode.getGeoPosition.getY,
                pvInputLimitedOperationTime.getNode.getGeoPosition.getX,
              )
            ),
          )
        )

        pvAgent ! RegistrationSuccessfulMessage(weatherService.ref, 0L)

        scheduler.expectMessage(Completion(emAgentActivation, Some(0)))

        val weatherDependentAgents = Seq(pvAgent)

        /* TICK 0
         Load: 282.74 VA, cosPhi: 0.95, P: 268.603 W, Q: 88.2855 var
         PV:  0 kW (not yet in operation)
         -> expect load p and q values as em p and q values
         */
        emAgentActivation ! Activation(0)

        // the result proxy will receive ExpectResult messages
        resultServiceProxy.expectMessage(
          ExpectResult(loadInputWithLimitedOperationTime.getUuid, 0, true)
        )

        weatherDependentAgents.foreach {
          _ ! DataProvision(
            0,
            weatherService.ref,
            WeatherData(
              WattsPerSquareMeter(0d),
              WattsPerSquareMeter(0d),
              Celsius(0d),
              MetersPerSecond(0d),
              None,
              None,
            ),
            Some(3600),
          )
        }

        // we receive update messages, since a new set point was provided
        resultServiceProxy.expectMessage(
          ExpectResult(loadInputWithLimitedOperationTime.getUuid, 0)
        )

        resultServiceProxy.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 0.toDateTime
            emResult.getP should equalWithTolerance(0.000268603.asMegaWatt)
            emResult.getQ should equalWithTolerance(0.0000882855367.asMegaVar)
        }
        resultServiceProxy.expectNoMessage()
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
            weatherService.ref,
            WeatherData(
              WattsPerSquareMeter(0d),
              WattsPerSquareMeter(0d),
              Celsius(0d),
              MetersPerSecond(0d),
              None,
              None,
            ),
            Some(7200),
          )
        }

        // we receive a message, since new data arrived
        resultServiceProxy.expectMessage(
          ExpectResult(pvInputLimitedOperationTime.getUuid, 3600, true)
        )

        // we receive an update message, since a new set point was provided
        resultServiceProxy.expectMessage(
          ExpectResult(pvInputLimitedOperationTime.getUuid, 3600)
        )

        resultServiceProxy.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 3600.toDateTime
            emResult.getP should equalWithTolerance(0.000268603.asMegaWatt)
            emResult.getQ should equalWithTolerance(0.0000882855367.asMegaVar)
        }
        resultServiceProxy.expectNoMessage()
        scheduler.expectMessage(Completion(emAgentActivation, Some(7200)))

        /* TICK 7200
         Load: P: 268.603 W, Q: 88.2855 var (unchanged)
         PV:  P: -8692.167 W  Q: -2856.98 var
         -> expect P:-8423.564 Q: -2768.69 var
         */
        weatherDependentAgents.foreach {
          _ ! DataProvision(
            7200,
            weatherService.ref,
            WeatherData(
              WattsPerSquareMeter(300d),
              WattsPerSquareMeter(200d),
              Celsius(0d),
              MetersPerSecond(0d),
              None,
              None,
            ),
            Some(21800),
          )
        }

        emAgentActivation ! Activation(7200)

        // we receive a message, since new data arrived
        resultServiceProxy.expectMessage(
          ExpectResult(pvInputLimitedOperationTime.getUuid, 7200, true)
        )

        // we receive an update message, since a new set point was provided
        resultServiceProxy.expectMessage(
          ExpectResult(pvInputLimitedOperationTime.getUuid, 7200)
        )

        resultServiceProxy.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 7200.toDateTime
            emResult.getP should equalWithTolerance(-0.008423564.asMegaWatt)
            emResult.getQ should equalWithTolerance(-0.0027686916118.asMegaVar)
        }
        resultServiceProxy.expectNoMessage()
        scheduler.expectMessage(Completion(emAgentActivation, Some(10800)))

        /* TICK 10800
        LOAD: P: 0 W, Q: 0 var (limited OperationTime)
        PV:  P: -8692.167 W  Q: -2856.98 var
        -> expect P and Q values of PV
         */
        emAgentActivation ! Activation(10800)

        // we receive a message, since new data arrived
        resultServiceProxy.expectMessage(
          ExpectResult(loadInputWithLimitedOperationTime.getUuid, 10800, true)
        )

        // we receive an update message, since a new set point was provided
        resultServiceProxy.expectMessage(
          ExpectResult(loadInputWithLimitedOperationTime.getUuid, 10800)
        )

        resultServiceProxy.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 10800.toDateTime
            emResult.getP should equalWithTolerance(-0.008692167.asMegaWatt)
            emResult.getQ should equalWithTolerance(-0.00285697715.asMegaVar)
        }
        resultServiceProxy.expectNoMessage()
        scheduler.expectMessage(Completion(emAgentActivation, Some(14400)))

        /* TICK 14400
        LOAD: P: 0 W, Q: 0 var (limited OperationTime)
        PV: P: 0 W, Q: 0 var (limited OperationTime)
        -> expect P: 0 W Q: 0 var
         */
        emAgentActivation ! Activation(14400)

        // we receive a message, since new data arrived
        resultServiceProxy.expectMessage(
          ExpectResult(pvInputLimitedOperationTime.getUuid, 14400, true)
        )

        // we receive an update message, since a new set point was provided
        resultServiceProxy.expectMessage(
          ExpectResult(pvInputLimitedOperationTime.getUuid, 14400)
        )

        resultServiceProxy.expectMessageType[ParticipantResultEvent] match {
          case ParticipantResultEvent(emResult: EmResult) =>
            emResult.getInputModel shouldBe emInput.getUuid
            emResult.getTime shouldBe 14400.toDateTime
            emResult.getP should equalWithTolerance(0.asMegaWatt)
            emResult.getQ should equalWithTolerance(0.asMegaVar)
        }
        resultServiceProxy.expectNoMessage()
        scheduler.expectMessage(Completion(emAgentActivation, None))
      }
    }
  }
}
