/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.em.EmAgentInit
import edu.ie3.simona.agent.participant.ParticipantAgentInit
import edu.ie3.simona.agent.participant.ParticipantAgentInit.{
  ParticipantRefs,
  SimulationParameters,
}
import edu.ie3.simona.config.RuntimeConfig.{
  EmRuntimeConfig,
  HpRuntimeConfig,
  PvRuntimeConfig,
}
import squants.radio.WattsPerSquareMeter
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.*
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.model.thermal.ThermalHouseTestData
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.*
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.Data.SecondaryData.WeatherData
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.service.results.ResultServiceProxy.{
  ExpectResult,
  NoResult,
}
import edu.ie3.simona.service.weather.WeatherService
import edu.ie3.simona.service.weather.WeatherService.WeatherRegistrationData
import edu.ie3.simona.service.{DataTimeType, ServiceType}
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.test.common.input.{
  EmInputTestData,
  ThermalGridITInputTestData,
}
import edu.ie3.simona.test.matchers.QuantityMatchers
import edu.ie3.simona.util.Coordinate
import edu.ie3.simona.util.SimonaConstants.{INIT_SIM_TICK, PRE_INIT_TICK}
import edu.ie3.simona.util.TickUtil.toDateTime
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.QuantityUtils.*
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.mockito.MockitoSugar
import squants.Each
import squants.motion.MetersPerSecond
import squants.thermal.Celsius
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.time.ZonedDateTime
import javax.measure.quantity.Temperature
import scala.language.postfixOps

/** Test to ensure the functions that a thermal grid and its connected assets is
  * capable.
  */
class ThermalGridIT
    extends ScalaTestWithActorTestKit
    with ThermalHouseTestData
    with AnyWordSpecLike
    with should.Matchers
    with QuantityMatchers
    with ThermalGridITInputTestData
    with EmInputTestData
    with MockitoSugar
    with TestSpawnerTyped {
  protected given temperatureTolerance: ComparableQuantity[Temperature] =
    Quantities.getQuantity(0.01, Units.CELSIUS)

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

  "A Thermal Grid with thermal house, storage and heat pump not under the control of energy management" should {
    "be initialized correctly and run through some activations" in {

      implicit val simulationStartDate: ZonedDateTime =
        TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z")
      val simulationEndDate: ZonedDateTime =
        TimeUtil.withDefaults.toZonedDateTime("2020-01-02T02:00:00Z")

      given SimulationParameters = SimulationParameters(
        expectedPowerRequestTick = Long.MaxValue,
        requestVoltageDeviationTolerance = Each(1e-14d),
        simulationStart = simulationStartDate,
        simulationEnd = simulationEndDate,
      )

      val resultServiceProxy =
        TestProbe[ResultEvent | ExpectResult | NoResult]("ResultProxy")
      val scheduler: TestProbe[SchedulerMessage] = TestProbe("scheduler")
      val primaryServiceProxy =
        TestProbe[PrimaryServiceProxy.Message]("PrimaryServiceProxy")
      val weatherService = TestProbe[WeatherService.Message]("WeatherService")

      given ParticipantRefs = ParticipantRefs(
        primaryServiceProxy = primaryServiceProxy.ref,
        resultServiceProxy = resultServiceProxy.ref,
        services = Map(ServiceType.WeatherService -> weatherService.ref),
      )

      val key = ScheduleLock.singleKey(TSpawner, scheduler.ref, PRE_INIT_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]

      val hpAgent = spawn(
        ParticipantAgentInit(
          hpInputContainerLittleWaterStorage,
          HpRuntimeConfig(),
          outputConfigOn,
          Left(scheduler.ref),
          key,
        ),
        "HeatPumpAgent1",
      )

      val pRunningHp = 0.0038.asMegaWatt
      val qRunningHp = 0.0012489995996796802.asMegaVar

      val hpInitSchedule = scheduler.expectMessageType[ScheduleActivation]
      hpInitSchedule.tick shouldBe INIT_SIM_TICK
      val heatPumpAgent = hpInitSchedule.actor

      /** Helper Method        * */
      def performMultipleActivations(
          activationActor: ActorRef[Activation],
          tickPairs: Seq[(Long, Long)],
      ): Unit = {
        tickPairs.foreach { case (currentTick, nextTick) =>
          activationActor ! Activation(currentTick)

          resultServiceProxy.expectMessage(
            ExpectResult(typicalHpInputModel.getUuid, currentTick)
          )

          Range(0, 2)
            .map { _ => resultServiceProxy.expectMessageType[ResultEvent] }
            .foreach {
              case ParticipantResultEvent(_) =>
              case ThermalResultEvent(_)     =>
            }

          resultServiceProxy.expectNoMessage()
          scheduler.expectMessage(Completion(activationActor, Some(nextTick)))
        }
      }

      /* INIT */

      heatPumpAgent ! Activation(INIT_SIM_TICK)

      primaryServiceProxy.expectMessage(
        PrimaryServiceRegistrationMessage(
          hpAgent,
          typicalHpInputModel.getUuid,
        )
      )

      // heat pump
      hpAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

      weatherService.expectMessage(
        SecondaryServiceRegistrationMessage(
          hpAgent,
          DataTimeType.Current,
          WeatherRegistrationData(
            Coordinate(
              typicalHpInputModel.getNode.getGeoPosition.getY,
              typicalHpInputModel.getNode.getGeoPosition.getX,
            )
          ),
        )
      )

      hpAgent ! RegistrationSuccessfulMessage(
        weatherService.ref,
        0,
      )
      val weatherDependentAgents = Seq(hpAgent)

      scheduler.expectMessage(Completion(heatPumpAgent, Some(0)))
      resultServiceProxy.expectNoMessage()

      /* TICK 0
      Start of Simulation
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      House demand water   : requiredDemand ~ 0.0674 kWh, possibleDemand ~ 0.067 kWh
      HeatStorage          : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: turned on - to serve the heat storage demand
       */
      heatPumpAgent ! Activation(0)

      // no message, since we are still waiting for secondary data
      resultServiceProxy.expectNoMessage()

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          0,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(-5d),
            MetersPerSecond(0d),
            None,
            None,
          ),
          Some(3600),
        )
      }

      val events0 = resultServiceProxy.receiveMessages(5)

      events0 should contain(
        ExpectResult(typicalHpInputModel.getUuid, 0)
      )

      events0.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 0.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 0.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          indoorTemp should equalWithTolerance(20.asDegreeCelsius)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) if inputModel == typicalHeatStorage.getUuid =>
          time shouldBe 0.toDateTime
          qDot should equalWithTolerance(0.011.asMegaWatt)
          energy should equalWithTolerance(0.asMegaWattHour)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) if inputModel == littleDomesticHotWaterStorageInput.getUuid =>
          time shouldBe 0.toDateTime
          qDot should equalWithTolerance(-0.005405957260274.asMegaWatt)
          energy should equalWithTolerance(0.000522.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(45)))

      /* TICK 45
      Domestic hot water storage stops discharging
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 10.3 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.067 kWh
      Heat pump: stays on to serve the heat storage demand
       */
      heatPumpAgent ! Activation(45)

      val events45 = resultServiceProxy.receiveMessages(3)
      // we receive update messages, since a new set point was provided
      events45 should contain(
        ExpectResult(typicalHpInputModel.getUuid, 45)
      )

      events45.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 45.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
          time shouldBe 45.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(0.0004544255342.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(3416)))

      /* TICK 3416
      Heat storage is fully heated up
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 2.36 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.067 kWh
      Heat pump: stays on since it was on and the house has possible demand
       */
      heatPumpAgent ! Activation(3416)

      val events3416 = resultServiceProxy.receiveMessages(4)

      events3416 should contain(ExpectResult(typicalHpInputModel.getUuid, 3416))

      events3416.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 3416.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 3416.toDateTime
          qDot should equalWithTolerance(0.011.asMegaWatt)
          indoorTemp should equalWithTolerance(19.68.asDegreeCelsius)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          time shouldBe 3416.toDateTime
          inputModel shouldBe typicalHeatStorage.getUuid
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(0.01044.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(3600)))

      /* TICK 3600
      New weather data (unchanged) incoming + Domestic hot water storage will cover hot water demand
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.92 kWh
      House demand water   : requiredDemand = 0.037 kWh, possibleDemand = 0.037 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.067 kWh
      Heat pump: stays on, we got triggered by incoming weather data. So we continue with same behaviour as before
       */
      heatPumpAgent ! Activation(3600)

      // no message, since we are still waiting for secondary data
      resultServiceProxy.expectNoMessage()

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          3600,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(1d),
            WattsPerSquareMeter(1d),
            Celsius(-5d),
            MetersPerSecond(0d),
            None,
            None,
          ),
          Some(21600),
        )
      }

      val events3600 = resultServiceProxy.receiveMessages(3)

      events3600 should contain(ExpectResult(typicalHpInputModel.getUuid, 3600))

      events3600.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 3600.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
          time shouldBe 3600.toDateTime
          qDot should equalWithTolerance(-0.005405957260273974.asMegaWatt)
          energy should equalWithTolerance(0.00045442553424658.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(3625)))

      /* TICK 3625
      Domestic hot water storage will stop discharging.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.86 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.1 kWh
      Heat pump: stays on - continue with same behaviour as before
       */
      heatPumpAgent ! Activation(3625)

      val events3625 = resultServiceProxy.receiveMessages(3)

      events3625 should contain(ExpectResult(typicalHpInputModel.getUuid, 3625))

      events3625.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 3625.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
          time shouldBe 3625.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(0.00041688416.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(4412)))

      /* TICK 4412
      House reaches target temperature boundary
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.1 kWh
      Heat pump: turned off
       */
      heatPumpAgent ! Activation(4412)

      val events4412 = resultServiceProxy.receiveMessages(3)

      events4412 should contain(ExpectResult(typicalHpInputModel.getUuid, 4412))

      events4412.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 4412.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 4412.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          indoorTemp should equalWithTolerance(19.99.asDegreeCelsius)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(7200)))

      /* We'll jump through a bunch of activations caused from DomesticHotWaterStorage being active.
      The results are checked implicitly through the state of stored energy at the next result check.
       */
      val activationTicksBlock =
        Seq(7200L, 7215L, 10800L, 10808L, 14400L, 14410L, 18000L, 18015L,
          21600L)
      val tickPairs = activationTicksBlock.zipWithIndex.collect {
        case (tick, index) if index < activationTicksBlock.length - 1 =>
          (tick, activationTicksBlock(index + 1))
      }

      performMultipleActivations(
        heatPumpAgent,
        tickPairs,
      )

      /* TICK 21600
      House would reach lowerTempBoundary at tick 50797.
      But now it's getting colder which should decrease inner temp of house faster.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 11.57 kWh
      House demand water   : requiredDemand = 0.09 kWh, possibleDemand = 0.09 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.18 kWh
      Heat pump: stays off
       */
      heatPumpAgent ! Activation(21600)

      // no message, since we are still waiting for secondary data
      resultServiceProxy.expectNoMessage()

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          21600,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(2d),
            WattsPerSquareMeter(2d),
            Celsius(-55d),
            MetersPerSecond(0d),
            None,
            None,
          ),
          Some(25000),
        )
      }
      val events21600 = resultServiceProxy.receiveMessages(3)

      events21600 should contain(
        ExpectResult(typicalHpInputModel.getUuid, 21600)
      )

      events21600.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 21600.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
          time shouldBe 21600.toDateTime
          qDot should equalWithTolerance(-0.005497583655.asMegaWatt)
          energy should equalWithTolerance(0.00034555556.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(21659)))

      /* TICK 21659
     Domestic storage stops discharging
     House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 11.7 kWh
     House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
     HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
     DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.27 kWh
     Heat pump: stays off
       */
      heatPumpAgent ! Activation(21659)

      val events21659 = resultServiceProxy.receiveMessages(3)

      // we receive update messages, since a new set point was provided
      events21659 should contain(
        ExpectResult(typicalHpInputModel.getUuid, 21659)
      )

      events21659.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 21659.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
          time shouldBe 21659.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(
            0.00025545627397260273.asMegaWattHour
          )
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(23288)))

      /* TICK 23288
      House reach lowerTemperatureBoundary
      House demand heating : requiredDemand = 15.0 kWh, possibleDemand = 15.0 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.27 kWh
      Heat pump: stays off, demand should be covered by storage
       */
      heatPumpAgent ! Activation(23288)

      val events23288 = resultServiceProxy.receiveMessages(4)

      events23288 should contain(
        ExpectResult(typicalHpInputModel.getUuid, 23288)
      )

      events23288.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 23288.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 23288.toDateTime
          qDot should equalWithTolerance(0.011.asMegaWatt)
          indoorTemp should equalWithTolerance(18.asDegreeCelsius)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe typicalHeatStorage.getUuid
          time shouldBe 23288.toDateTime
          qDot should equalWithTolerance(-0.011.asMegaWatt)
          energy should equalWithTolerance(0.01044.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(25000)))

      /* TICK 25000
      Additional trigger caused by (unchanged) weather data should not change this
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 13.24 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 5.23 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.27 kWh
      Heat pump: stays off, demand should be covered by storage
       */
      heatPumpAgent ! Activation(25000)

      // no message, since we are still waiting for secondary data
      resultServiceProxy.expectNoMessage()

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          25000,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(3d),
            WattsPerSquareMeter(3d),
            Celsius(-55d),
            MetersPerSecond(0d),
            None,
            None,
          ),
          Some(28000),
        )
      }

      val events25000 = resultServiceProxy.receiveMessages(2)

      events25000 should contain(
        ExpectResult(typicalHpInputModel.getUuid, 25000)
      )

      events25000.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 25000.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(25200)))

      /* TICK 25200
      DomesticHotWaterStorage
      House demand heating : requiredDemand = 0.0kWh, possibleDemand = 13.04 kWh
      House demand water   : requiredDemand = 0.18 kWh, possibleDemand = 0.18 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 5.84 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.27 kWh
      Heat pump: stays off
       */
      heatPumpAgent ! Activation(25200)

      val events25200 = resultServiceProxy.receiveMessages(3)

      // we receive update messages, since a new set point was provided
      events25200 should contain(
        ExpectResult(typicalHpInputModel.getUuid, 25200)
      )

      events25200.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 25200.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
          time shouldBe 25200.toDateTime
          qDot should equalWithTolerance(-0.00547586188.asMegaWatt)
          energy should equalWithTolerance(0.000255456274.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(25316)))

      /* TICK 25316
      DomesticHotWaterStorage stops discharging.
      House demand heating : requiredDemand = 0.0kWh, possibleDemand = 12.92 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 6.2 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.44 kWh
      Heat pump: stays off
       */
      heatPumpAgent ! Activation(25316)

      val events25316 = resultServiceProxy.receiveMessages(3)

      // we receive update messages, since a new set point was provided
      events25316 should contain(
        ExpectResult(typicalHpInputModel.getUuid, 25316)
      )

      events25316.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 25316.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
          time shouldBe 25316.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(0.000079011836.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(26704)))

      /* TICK 26704
      Heat storage is empty
      House demand heating : requiredDemand = 0.0kWh, possibleDemand = 11.51 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.44 kWh
      Heat pump: turned on - to serve the remaining heat demand of house (and refill storage later)
       */
      heatPumpAgent ! Activation(26704)

      val events26704 = resultServiceProxy.receiveMessages(3)

      events26704 should contain(
        ExpectResult(typicalHpInputModel.getUuid, 26704)
      )

      events26704.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 26704.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe typicalHeatStorage.getUuid
          time shouldBe 26704.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(0.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(28000)))

      /* TICK 28000
        New weather data: it's getting warmer again
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 10.19 kWh
        House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        HeatStorage          : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.44 kWh
        Heat pump: stays on
       */
      heatPumpAgent ! Activation(28000)

      // no message, since we are still waiting for secondary data
      resultServiceProxy.expectNoMessage()

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          28000,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(4d),
            WattsPerSquareMeter(4d),
            Celsius(5d),
            MetersPerSecond(0d),
            None,
            None,
          ),
          Some(151200),
        )
      }

      val events28000 = resultServiceProxy.receiveMessages(2)

      events28000 should contain(
        ExpectResult(typicalHpInputModel.getUuid, 28000)
      )

      events28000.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 28000.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
      }

      // Since this activation is caused by new weather data, we don't expect any
      // message for house or storage since there is no change of their operating
      // point nor one of it reached any boundary.
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(28800)))

      /* TICK 28800
      DomesticHotWaterStorage
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 8.06 kWh
      House demand water   : requiredDemand = 0.25 kWh, possibleDemand = 0.25 kWh
      HeatStorage          : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.44 kWh
      Heat pump: stays on to recharge the HeatStorage now
       */
      heatPumpAgent ! Activation(28800)

      val events28800 = resultServiceProxy.receiveMessages(3)

      // we receive update messages, since a new set point was provided
      events28800 should contain(
        ExpectResult(typicalHpInputModel.getUuid, 28800)
      )

      events28800.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 28800.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
          time shouldBe 28800.toDateTime
          qDot should equalWithTolerance(-0.0054700501581.asMegaWatt)
          energy should equalWithTolerance(
            0.00007901183561643826.asMegaWattHour
          )
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(28852)))

      /* TICK 28852
     DomesticHotWaterStorage is empty
     House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 7.92 kWh
     House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
     HeatStorage          : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
     DomesticWaterStorage : requiredDemand = 0.522 kWh, possibleDemand = 0.522 kWh
     Heat pump: stays on, qDot should be split between DomesticHotWaterStorage and House
       */
      heatPumpAgent ! Activation(28852)

      val events28852 = resultServiceProxy.receiveMessages(4)
      // we receive update messages, since a new set point was provided
      events28852 should contain(
        ExpectResult(typicalHpInputModel.getUuid, 28852)
      )

      events28852.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 28852.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 28852.toDateTime
          qDot should equalWithTolerance(0.0055.asMegaWatt)
          indoorTemp should equalWithTolerance(18.94.asDegreeCelsius)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
          time shouldBe 28852.toDateTime
          qDot should equalWithTolerance(0.0055.asMegaWatt)
          energy should equalWithTolerance(0.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(29193)))

      /* TICK 29193
      DomesticWaterStorage is fully charged
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 7.53 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays on to heat the house alone now
       */
      heatPumpAgent ! Activation(29193)

      val events29193 = resultServiceProxy.receiveMessages(4)

      // we receive update messages, since a new set point was provided
      events29193 should contain(
        ExpectResult(typicalHpInputModel.getUuid, 29193)
      )

      events29193.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 29193.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 29193.toDateTime
          qDot should equalWithTolerance(0.011.asMegaWatt)
          indoorTemp should equalWithTolerance(19.asDegreeCelsius)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
          time shouldBe 29193.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(0.000522.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(32032)))

      /* TICK 32032
        House will reach the upperTemperatureBoundary
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        HeatStorage          : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        Heat pump: stays on to recharge the HeatStorage now
       */
      heatPumpAgent ! Activation(32032)

      val events32032 = resultServiceProxy.receiveMessages(4)

      // we receive update messages, since a new set point was provided
      events32032 should contain(
        ExpectResult(typicalHpInputModel.getUuid, 32032)
      )

      events32032.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 32032.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 32032.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          indoorTemp should equalWithTolerance(19.99.asDegreeCelsius)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe typicalHeatStorage.getUuid
          time shouldBe 32032.toDateTime
          qDot should equalWithTolerance(0.011.asMegaWatt)
          energy should equalWithTolerance(0.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(32400)))

      /* TICK 32400
       DomesticHotWaterStorage will cover demand
       House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.15 kWh
       House demand water   : requiredDemand = 0.21 kWh, possibleDemand = 0.21 kWh
       HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 9.32 kWh
       DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
       Heat pump: stays on
       */
      heatPumpAgent ! Activation(32400)

      val events32400 = resultServiceProxy.receiveMessages(3)

      // we receive update messages, since a new set point was provided
      events32400 should contain(
        ExpectResult(typicalHpInputModel.getUuid, 32400)
      )

      events32400.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 32400.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
          time shouldBe 32400.toDateTime
          qDot should equalWithTolerance(-0.005463467444.asMegaWatt)
          energy should equalWithTolerance(0.000522.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(32541)))

      /* TICK 32541
      DomesticHotWaterStorage stops discharging.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.21 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 8.9 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.21 kWh
      Heat pump: stays on
       */
      heatPumpAgent ! Activation(32541)

      val events32541 = resultServiceProxy.receiveMessages(3)

      // we receive update messages, since a new set point was provided
      events32541 should contain(
        ExpectResult(typicalHpInputModel.getUuid, 32541)
      )

      events32541.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 32541.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe littleDomesticHotWaterStorageInput.getUuid
          time shouldBe 32541.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(0.0003080141917.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(35448)))

      /* TICK 35448
      Storage will be fully charged, but meanwhile the house cooled a bit
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.4 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.21 kWh
      Heat pump: stays on to again heat the house
       */
      heatPumpAgent ! Activation(35448)

      val events35448 = resultServiceProxy.receiveMessages(4)

      // we receive update messages, since a new set point was provided
      events35448 should contain(
        ExpectResult(typicalHpInputModel.getUuid, 35448)
      )

      events35448.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 35448.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 35448.toDateTime
          qDot should equalWithTolerance(0.011.asMegaWatt)
          indoorTemp should equalWithTolerance(19.81.asDegreeCelsius)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe typicalHeatStorage.getUuid
          time shouldBe 35448.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(0.01044.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(35983)))

      /* TICK 35983
      Thermal House reaches target temperature
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.21 kWh
      Heat pump: turned off - since neither house nor any storage have any demand
       */
      heatPumpAgent ! Activation(35983)

      val events35983 = resultServiceProxy.receiveMessages(3)

      // we receive update messages, since a new set point was provided
      events35983 should contain(
        ExpectResult(typicalHpInputModel.getUuid, 35983)
      )
      events35983.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 35983.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 35983.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          indoorTemp should equalWithTolerance(20.asDegreeCelsius)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(heatPumpAgent, Some(36000)))
    }
  }

  "A Thermal Grid with thermal house, thermal storage and heat pump that is controlled by an energy management" should {
    "be initialized correctly and run through some activations" in {
      implicit val simulationStartWithPv: ZonedDateTime =
        TimeUtil.withDefaults.toZonedDateTime("2020-06-01T10:00:00Z")
      val simulationEndWithPv: ZonedDateTime =
        TimeUtil.withDefaults.toZonedDateTime("2020-06-12T10:00:00Z")

      given SimulationParameters = SimulationParameters(
        expectedPowerRequestTick = Long.MaxValue,
        requestVoltageDeviationTolerance = Each(1e-14d),
        simulationStart = simulationStartWithPv,
        simulationEnd = simulationEndWithPv,
      )

      val resultServiceProxy: TestProbe[ResultEvent | ExpectResult | NoResult] =
        TestProbe("resultServiceProxy")
      val scheduler: TestProbe[SchedulerMessage] = TestProbe("scheduler")
      val primaryServiceProxy =
        TestProbe[PrimaryServiceProxy.Message]("PrimaryServiceProxy")
      val weatherService = TestProbe[WeatherService.Message]("WeatherService")

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
          EmRuntimeConfig(),
          outputConfigOn,
          simulationStartWithPv,
          parent = Left(scheduler.ref),
          listener = resultServiceProxy.ref,
        ),
        "EmAgent",
      )

      val pvAgent = spawn(
        ParticipantAgentInit(
          pvInputContainer,
          PvRuntimeConfig(calculateMissingReactivePowerWithModel = true),
          outputConfigOff,
          Right(emAgent),
          keys.next(),
        ),
        "PvAgent",
      )

      val hpAgent = spawn(
        ParticipantAgentInit(
          hpInputContainerSmallWaterStorage,
          HpRuntimeConfig(),
          outputConfigOn,
          Right(emAgent),
          keys.next(),
        ),
        "HeatPumpAgentWithEm",
      )

      val pRunningHp = 0.0038.asMegaWatt
      val qRunningHp = 0.0012489995996796802.asMegaVar

      val emInitSchedule = scheduler.expectMessageType[ScheduleActivation]
      emInitSchedule.tick shouldBe INIT_SIM_TICK
      val emAgentActivation = emInitSchedule.actor

      /** Helper Method * */
      def performMultipleActivations(
          activationActor: ActorRef[Activation],
          tickPairs: Seq[(Long, Long)],
      ): Unit = {
        tickPairs.foreach { case (currentTick, nextTick) =>
          activationActor ! Activation(currentTick)

          resultServiceProxy.receiveMessages(2) should contain allOf (
            // we receive a message, since new data arrived
            ExpectResult(typicalHpInputModel.getUuid, currentTick, true),
            // we receive update messages, since a new set point was provided
            ExpectResult(typicalHpInputModel.getUuid, currentTick)
          )

          Range(0, 3)
            .map { _ => resultServiceProxy.expectMessageType[ResultEvent] }
            .foreach {
              case ParticipantResultEvent(_) =>
              case ThermalResultEvent(_)     =>
            }

          resultServiceProxy.expectNoMessage()
          scheduler.expectMessage(Completion(activationActor, Some(nextTick)))
        }
      }

      scheduler.expectNoMessage()

      emInitSchedule.unlockKey.value.unlock()
      scheduler.expectMessage(Completion(lockActivation))

      /* INIT */

      emAgentActivation ! Activation(INIT_SIM_TICK)

      primaryServiceProxy.receiveMessages(2) should contain allOf (
        PrimaryServiceRegistrationMessage(
          hpAgent,
          typicalHpInputModel.getUuid,
        ),
        PrimaryServiceRegistrationMessage(
          pvAgent,
          pvInput.getUuid,
        )
      )

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

      pvAgent ! RegistrationSuccessfulMessage(
        weatherService.ref,
        0L,
      )

      // heat pump
      hpAgent ! RegistrationFailedMessage(primaryServiceProxy.ref)

      // deal with weather service registration
      weatherService.expectMessage(
        SecondaryServiceRegistrationMessage(
          hpAgent,
          DataTimeType.Current,
          WeatherRegistrationData(
            Coordinate(
              typicalHpInputModel.getNode.getGeoPosition.getY,
              typicalHpInputModel.getNode.getGeoPosition.getX,
            )
          ),
        )
      )

      hpAgent ! RegistrationSuccessfulMessage(
        weatherService.ref,
        0L,
      )
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(0)))

      val weatherDependentAgents = Seq(hpAgent, pvAgent)

      /* TICK 0
        Start of Simulation, No sun at the moment.
        PV: 0.0 kW
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        House demand water   : requiredDemand = 0.23 kWh, possibleDemand = 0.23 kWh
        HeatStorage          : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        Heat pump: stays out - since requiredDemand of HeatStorage not necessarily demand hp operation.
       */
      emAgentActivation ! Activation(0)

      // no message, since we are still waiting for secondary data
      resultServiceProxy.expectNoMessage()

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          0,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(0),
            WattsPerSquareMeter(0),
            Celsius(-5d),
            MetersPerSecond(0d),
            None,
            None,
          ),
          Some(1800),
        )
      }

      val msg0 = resultServiceProxy.receiveMessages(9)

      msg0 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 0, true),
        ExpectResult(pvInput.getUuid, 0, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 0),
        ExpectResult(pvInput.getUuid, 0)
      )

      msg0.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 0.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 0.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 0.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          indoorTemp should equalWithTolerance(20.asDegreeCelsius)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) if inputModel == typicalHeatStorage.getUuid =>
          time shouldBe 0.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(0.asMegaWattHour)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) if inputModel == smallDomesticHotWaterStorageInput.getUuid =>
          time shouldBe 0.toDateTime
          qDot should equalWithTolerance(-0.005496056547945205.asMegaWatt)
          energy should equalWithTolerance(0.00149814.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(150)))

      /* TICK 150
      Domestic hot water storage stops discharging.
      PV: 0.0 kW
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.1 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.23 kWh
      Heat pump: stays out - since requiredDemand of HeatStorage not necessarily demand hp operation.
       */
      emAgentActivation ! Activation(150)

      val msg150 = resultServiceProxy.receiveMessages(5)

      msg150 should contain allOf (
        // we receive a message, since new data arrived
        ExpectResult(typicalHpInputModel.getUuid, 150, true),
        // we receive update messages, since a new set point was provided
        ExpectResult(typicalHpInputModel.getUuid, 150)
      )

      msg150.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 150.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 150.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
          time shouldBe 150.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(0.0012691376438.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(1800)))

      /* TICK 1800
        New Weather: The sun comes out, PV will produce.
        PV: -6.7 kW
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.25 kWh
        House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        HeatStorage          : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.23 kWh
        Heat pump: turns on - since now we have flexibility potential available which can be
        used by hp to serve the reqDemand of ThermalStorage
       */
      emAgentActivation ! Activation(1800)

      // no message, since we are still waiting for secondary data
      resultServiceProxy.expectNoMessage()

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          1800,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(540),
            WattsPerSquareMeter(400),
            Celsius(-5d),
            MetersPerSecond(0d),
            None,
            None,
          ),
          Some(5400),
        )
      }

      val msg1800 = resultServiceProxy.receiveMessages(7)

      msg1800 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 1800, true),
        ExpectResult(pvInput.getUuid, 1800, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 1800),
        ExpectResult(pvInput.getUuid, 1800)
      )

      msg1800.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 1800.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 1800.toDateTime
          p should equalWithTolerance(-0.00292865377.asMegaWatt)
          q should equalWithTolerance(-0.00096260194.asMegaVar)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe typicalHeatStorage.getUuid
          time shouldBe 1800.toDateTime
          qDot should equalWithTolerance(0.011.asMegaWatt)
          energy should equalWithTolerance(0.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(3600)))

      /* TICK 3600
      DomesticHotWaterStorage will serve the water demand of the house.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 2.5 kWh
      House demand water   : requiredDemand = 0.23 kWh, possibleDemand = 0.23 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 4.9 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.23 kWh
      Heat pump: stays on
       */
      emAgentActivation ! Activation(3600)

      val msg3600 = resultServiceProxy.receiveMessages(5)

      msg3600 should contain allOf (
        // we receive a message, since new data arrived
        ExpectResult(typicalHpInputModel.getUuid, 3600, true),
        // we receive update messages, since a new set point was provided
        ExpectResult(typicalHpInputModel.getUuid, 3600)
      )

      msg3600.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 3600.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 3600.toDateTime
          p should equalWithTolerance(-0.00292865377.asMegaWatt)
          q should equalWithTolerance(-0.00096260194.asMegaVar)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
          time shouldBe 3600.toDateTime
          qDot should equalWithTolerance(-0.005496056547945205.asMegaWatt)
          energy should equalWithTolerance(0.0012691376438356.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(3750)))

      /* TICK 3750
      DomesticHotWaterStorage stops discharging.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 2.6 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.44 kWh, possibleDemand = 4.4 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.46 kWh
      Heat pump: stays on - since now we have flexibility potential available which
      can be used by hp to serve the reqDemand of ThermalStorage
       */
      emAgentActivation ! Activation(3750)

      val msg3750 = resultServiceProxy.receiveMessages(5)

      msg3750 should contain allOf (
        // we receive a message, since new data arrived
        ExpectResult(typicalHpInputModel.getUuid, 3750, true),
        // we receive update messages, since a new set point was provided
        ExpectResult(typicalHpInputModel.getUuid, 3750)
      )

      msg3750.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 3750.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 3750.toDateTime
          p should equalWithTolerance(-0.00292865377.asMegaWatt)
          q should equalWithTolerance(-0.0009626019.asMegaVar)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
          time shouldBe 3750.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(0.001040135288.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(5216)))

      /* TICK 5216
      Storage is fully heated up, meanwhile house cooled a bit.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 3.59 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays on since it was on and the house has possible demand
       */
      emAgentActivation ! Activation(5216)

      val msg5216 = resultServiceProxy.receiveMessages(6)

      msg5216 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 5216, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 5216)
      )

      msg5216.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 5216.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 5216.toDateTime
          p should equalWithTolerance(-0.00292865377.asMegaWatt)
          q should equalWithTolerance(-0.000962601944.asMegaVar)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 5216.toDateTime
          qDot should equalWithTolerance(0.011.asMegaWatt)
          indoorTemp should equalWithTolerance(19.52.asDegreeCelsius)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe typicalHeatStorage.getUuid
          time shouldBe 5216.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(0.01044.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(5400)))

      /* TICK 5400
      PV: 0.0 kW
      New weather data, sun is gone again, thus we should now heat the house by storage.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 3.15 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.45 kWh
      Heat pump: turns off
       */
      emAgentActivation ! Activation(5400)

      // no message, since we are still waiting for secondary data
      resultServiceProxy.expectNoMessage()

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          5400,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(0),
            WattsPerSquareMeter(0),
            Celsius(-5d),
            MetersPerSecond(0d),
            None,
            None,
          ),
          Some(9200),
        )
      }

      val msg5400 = resultServiceProxy.receiveMessages(7)

      msg5400 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 5400, true),
        ExpectResult(pvInput.getUuid, 5400, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 5400),
        ExpectResult(pvInput.getUuid, 5400)
      )

      msg5400.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 5400.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 5400.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe typicalHeatStorage.getUuid
          time shouldBe 5400.toDateTime
          qDot should equalWithTolerance(-0.011.asMegaWatt)
          energy should equalWithTolerance(0.01044.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(6731)))

      /* TICK 6731
     The house reaches target temperature
     House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
     House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
     HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 4.07 kWh
     DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.45 kWh
     Heat pump: stays off
       */
      emAgentActivation ! Activation(6731)

      val msg6731 = resultServiceProxy.receiveMessages(6)

      msg6731 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 6731, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 6731)
      )

      msg6731.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 6731.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 6731.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 6731.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          indoorTemp should equalWithTolerance(19.99.asDegreeCelsius)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe typicalHeatStorage.getUuid
          time shouldBe 6731.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(0.0063730555556.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(7200)))

      /* TICK 7200
     DomesticHotWaterStorage will serve the water demand of the house.
     House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.32 kWh
     House demand water   : requiredDemand = 0.24 kWh, possibleDemand = 0.24 kWh
     HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 4.07 kWh
     DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.45 kWh
     Heat pump: stays off
       */
      emAgentActivation ! Activation(7200)

      val msg7200 = resultServiceProxy.receiveMessages(5)

      msg7200 should contain allOf (
        // we receive a message, since new data arrived
        ExpectResult(typicalHpInputModel.getUuid, 7200, true),
        // we receive update messages, since a new set point was provided
        ExpectResult(typicalHpInputModel.getUuid, 7200)
      )

      msg7200.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 7200.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 7200.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 5216.toDateTime
          qDot should equalWithTolerance(0.011.asMegaWatt)
          indoorTemp should equalWithTolerance(19.52.asDegreeCelsius)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
          time shouldBe 7200.toDateTime
          qDot should equalWithTolerance(-0.00549315011931065.asMegaWatt)
          energy should equalWithTolerance(0.001040135288.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(7355)))

      /* TICK 7355
      DomesticHotWaterStorage stops discharging
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.43 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 4.07 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.69 kWh
      Heat pump: stays off
       */
      emAgentActivation ! Activation(7355)

      val msg7355 = resultServiceProxy.receiveMessages(5)

      msg7355 should contain allOf (
        // we receive a message, since new data arrived
        ExpectResult(typicalHpInputModel.getUuid, 7355, true),
        // we receive update messages, since a new set point was provided
        ExpectResult(typicalHpInputModel.getUuid, 7355)
      )

      msg7355.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 7355.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 7355.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
          time shouldBe 7355.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(0.0008036246575.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(9200)))

      /* TICK 9200
      The sun is back again, storage first.
      PV: -5.2 kW
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.7 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 4.07 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.69 kWh
      Heat pump: turned on
       */
      emAgentActivation ! Activation(9200)

      // no message, since we are still waiting for secondary data
      resultServiceProxy.expectNoMessage()

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          9200,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(450),
            WattsPerSquareMeter(250),
            Celsius(-5d),
            MetersPerSecond(0d),
            None,
            None,
          ),
          Some(12000),
        )
      }

      val msg9200 = resultServiceProxy.receiveMessages(7)

      msg9200 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 9200, true),
        ExpectResult(pvInput.getUuid, 9200, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 9200),
        ExpectResult(pvInput.getUuid, 9200)
      )

      msg9200.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 9200.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 9200.toDateTime
          p should equalWithTolerance(-0.0014023707.asMegaWatt)
          q should equalWithTolerance(-0.00046093696.asMegaVar)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe typicalHeatStorage.getUuid
          time shouldBe 9200.toDateTime
          qDot should equalWithTolerance(0.011.asMegaWatt)
          energy should equalWithTolerance(0.0063730555556.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(10531)))

      /* TICK 10531
      Storage is full, now heating the house till target temperature.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 2.62 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays on
       */
      emAgentActivation ! Activation(10531)

      val msg10531 = resultServiceProxy.receiveMessages(6)

      msg10531 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 10531, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 10531)
      )

      msg10531.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 10531.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 10531.toDateTime
          p should equalWithTolerance(-0.0014023707.asMegaWatt)
          q should equalWithTolerance(-0.00046093696.asMegaVar)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 10531.toDateTime
          qDot should equalWithTolerance(0.011.asMegaWatt)
          indoorTemp should equalWithTolerance(19.65.asDegreeCelsius)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe typicalHeatStorage.getUuid
          time shouldBe 10531.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(0.01044.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(10800)))

      /* TICK 10800
      House has water demand
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.98 kWh
      House demand water   : requiredDemand = 0.24 kWh, possibleDemand = 0.24 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.69 kWh
      Heat pump: stays on
       */
      emAgentActivation ! Activation(10800)

      val msg10800 = resultServiceProxy.receiveMessages(5)

      msg10800 should contain allOf (
        // we receive a message, since new data arrived
        ExpectResult(typicalHpInputModel.getUuid, 10800, true),
        // we receive update messages, since a new set point was provided
        ExpectResult(typicalHpInputModel.getUuid, 10800)
      )

      msg10800.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 10800.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 10800.toDateTime
          p should equalWithTolerance(-0.0014023707.asMegaWatt)
          q should equalWithTolerance(-0.000460937.asMegaVar)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
          time shouldBe 10800.toDateTime
          qDot should equalWithTolerance(-0.005474387099011617.asMegaWatt)
          energy should equalWithTolerance(0.000803624658.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(10958)))

      /* TICK 10958
      DomesticHotWaterStorage stops discharging
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.61 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.93 kWh
      Heat pump: stays on
       */
      emAgentActivation ! Activation(10958)

      val msg10958 = resultServiceProxy.receiveMessages(5)

      msg10958 should contain allOf (
        // we receive a message, since new data arrived
        ExpectResult(typicalHpInputModel.getUuid, 10958, true),
        // we receive update messages, since a new set point was provided
        ExpectResult(typicalHpInputModel.getUuid, 10958)
      )

      msg10958.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 10958.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 10958.toDateTime
          p should equalWithTolerance(-0.0014023706967.asMegaWatt)
          q should equalWithTolerance(-0.000460936958.asMegaVar)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
          time shouldBe 10958.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(0.00056335989.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(11638)))

      /* TICK 11638
      House reaches target temperature boundary.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.93 kWh
      Heat pump: turned off
       */
      emAgentActivation ! Activation(11638)

      val msg11638 = resultServiceProxy.receiveMessages(5)

      msg11638 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 11638, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 11638)
      )

      msg11638.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 11638.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 11638.toDateTime
          p should equalWithTolerance(-0.0052023707.asMegaWatt)
          q should equalWithTolerance(-0.0017099366.asMegaVar)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 11638.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          indoorTemp should equalWithTolerance(19.99.asDegreeCelsius)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(12000)))

      /* TICK 12000
      House would reach lowerTempBoundary at tick 23809,
      but now it's getting colder which should decrease inner temp of house faster, but the sun is still there.
      PV: -5.16 kW
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.25 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.93 kWh
      Heat pump: turned on, since there is possibleDemand and setPower is 3800 W which is > 0.5 sRated of Hp
       */
      emAgentActivation ! Activation(12000)

      // no message, since we are still waiting for secondary data
      resultServiceProxy.expectNoMessage()

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          12000,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(450),
            WattsPerSquareMeter(250),
            Celsius(-25d),
            MetersPerSecond(0d),
            None,
            None,
          ),
          Some(12500),
        )
      }

      val msg12000 = resultServiceProxy.receiveMessages(7)

      msg12000 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 12000, true),
        ExpectResult(pvInput.getUuid, 12000, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 12000),
        ExpectResult(pvInput.getUuid, 12000)
      )

      msg12000.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 12000.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 12000.toDateTime
          p should equalWithTolerance(-0.00135705894.asMegaWatt)
          q should equalWithTolerance(-0.0004460437.asMegaVar)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 12000.toDateTime
          qDot should equalWithTolerance(0.011.asMegaWatt)
          indoorTemp should equalWithTolerance(19.96.asDegreeCelsius)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(12139)))

      /* TICK 12139
      House reaches the target temperature.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.93 kWh
      Heat pump: turned off
       */
      emAgentActivation ! Activation(12139)

      val msg12139 = resultServiceProxy.receiveMessages(5)

      msg12139 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 12139, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 12139)
      )

      msg12139.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 12139.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 12139.toDateTime
          p should equalWithTolerance(-0.00515705894.asMegaWatt)
          q should equalWithTolerance(-0.00169504330.asMegaVar)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 12139.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          indoorTemp should equalWithTolerance(20.asDegreeCelsius)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(12500)))

      /* TICK 12500
       PV: 0.0 kW
       Inner temperature of the house is decreasing but above the lower boundary.
       Thus, updated weather data (sun is gone) should not change behaviour.
       House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.45 kWh
       House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
       HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
       DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.93 kWh
       Heat pump: stays off
       */
      emAgentActivation ! Activation(12500)

      // no message, since we are still waiting for secondary data
      resultServiceProxy.expectNoMessage()

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          12500,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(-25d),
            MetersPerSecond(0d),
            None,
            None,
          ),
          Some(25200),
        )
      }

      val msg12500 = resultServiceProxy.receiveMessages(6)

      msg12500 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 12500, true),
        ExpectResult(pvInput.getUuid, 12500, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 12500)
      )

      msg12500.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 12500.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 12500.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(14400)))

      /* We'll jump through a bunch of activations caused from DomesticHotWaterStorage being active.
     The results are checked implicitly through the state of stored energy at the next result check.
       */
      val firstActivationTicksBlock =
        Seq(14400L, 14526L, 18000L, 18109L, 21600L, 21706L, 24412L)

      val firstTickPairs = firstActivationTicksBlock.zipWithIndex.collect {
        case (tick, index) if index < firstActivationTicksBlock.length - 1 =>
          (tick, firstActivationTicksBlock(index + 1))
      }

      performMultipleActivations(
        emAgentActivation,
        firstTickPairs,
      )

      /* TICK 24412
        House reaches lower boundary, since we don't have surplus energy from pv, we would use the energy from storage to heat the house.
        House demand heating : requiredDemand = 15.0 kWh, possibleDemand = 15.0 kWh
        House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 1.45 kWh
        Heat pump: stays off
       */
      emAgentActivation ! Activation(24412)

      val msg24412 = resultServiceProxy.receiveMessages(6)

      msg24412 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 24412, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 24412)
      )

      msg24412.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 24412.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 24412.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 24412.toDateTime
          qDot should equalWithTolerance(0.011.asMegaWatt)
          indoorTemp should equalWithTolerance(18.asDegreeCelsius)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe typicalHeatStorage.getUuid
          time shouldBe 24412.toDateTime
          qDot should equalWithTolerance(-0.011.asMegaWatt)
          energy should equalWithTolerance(0.01044.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(25200)))

      /* TICK 25200
        The sun comes out and it's getting warmer.
        PV: -3.4 kW
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 13.53 kWh
        House demand water   : requiredDemand = 0.18 kWh, possibleDemand = 0.18 kWh
        HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 2.4 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 1.45 kWh
        Heat pump: will be turned on and will continue heating the house
       */
      emAgentActivation ! Activation(25200)

      // no message, since we are still waiting for secondary data
      resultServiceProxy.expectNoMessage()

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          25200,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(450),
            WattsPerSquareMeter(250),
            Celsius(-5d),
            MetersPerSecond(0d),
            None,
            None,
          ),
          Some(27500),
        )
      }

      val msg25200 = resultServiceProxy.receiveMessages(8)

      // expect messages due to flex activation
      msg25200 should contain allOf (
        ExpectResult(typicalHpInputModel.getUuid, 25200, true),
        ExpectResult(pvInput.getUuid, 25200, true),
        ExpectResult(typicalHpInputModel.getUuid, 25200),
        ExpectResult(pvInput.getUuid, 25200)
      )

      msg25200.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 25200.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 25200.toDateTime
          p should equalWithTolerance(0.000368226548.asMegaWatt)
          q should equalWithTolerance(0.000121030214.asMegaVar)
      }

      msg25200
        .collect {
          case ThermalResultEvent(
                AbstractThermalStorageResult(time, inputModel, qDot, energy)
              ) =>
            (inputModel, time, qDot, energy)
        }
        .foreach {
          case (uuid, time, qDot, energy)
              if uuid == typicalHeatStorage.getUuid =>
            time shouldBe 25200.toDateTime
            qDot should equalWithTolerance(0.asMegaWatt)
            energy should equalWithTolerance(0.0080322222222.asMegaWattHour)
          case (uuid, time, qDot, energy)
              if uuid == smallDomesticHotWaterStorageInput.getUuid =>
            time shouldBe 25200.toDateTime
            qDot should equalWithTolerance(-0.00543467835616.asMegaWatt)
            energy should equalWithTolerance(0.000045288986.asMegaWattHour)
        }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(25230)))

      /* TICK 25230
      DomesticHotWaterStorage is empty.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 13.46 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 2.41 kWh
      DomesticWaterStorage : requiredDemand = 1.5 kWh, possibleDemand = 1.5 kWh
      Heat pump: Stays on, but qDot will be split between house and hot water storage
       */
      emAgentActivation ! Activation(25230)

      val msg25230 = resultServiceProxy.receiveMessages(6)

      msg25230 should contain allOf (
        // we receive a message, since new data arrived
        ExpectResult(typicalHpInputModel.getUuid, 25230, true),
        // we receive update messages, since a new set point was provided
        ExpectResult(typicalHpInputModel.getUuid, 25230)
      )

      msg25230.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 25230.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 25230.toDateTime
          p should equalWithTolerance(0.00036822655.asMegaWatt)
          q should equalWithTolerance(0.00012103021.asMegaVar)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 25230.toDateTime
          qDot should equalWithTolerance(0.0055.asMegaWatt)
          indoorTemp should equalWithTolerance(18.20.asDegreeCelsius)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
          time shouldBe 25230.toDateTime
          qDot should equalWithTolerance(0.0055.asMegaWatt)
          energy should equalWithTolerance(0.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(26210)))

      /* TICK 26210
      DomesticHotWaterStorage is full
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 12.6 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 2.41 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays on - will continue heating the house only
       */
      emAgentActivation ! Activation(26210)

      val msg26210 = resultServiceProxy.receiveMessages(6)

      msg26210 should contain allOf (
        // we receive a message, since new data arrived
        ExpectResult(typicalHpInputModel.getUuid, 26210, true),
        // we receive update messages, since a new set point was provided
        ExpectResult(typicalHpInputModel.getUuid, 26210)
      )

      msg26210.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 26210.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 26210.toDateTime
          p should equalWithTolerance(0.00036822655.asMegaWatt)
          q should equalWithTolerance(0.00012103021.asMegaVar)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 26210.toDateTime
          qDot should equalWithTolerance(0.011.asMegaWatt)
          indoorTemp should equalWithTolerance(18.32.asDegreeCelsius)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
          time shouldBe 26210.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(0.00149814.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(27500)))

      /* TICK 27500
        Additional trigger caused by (unchanged) weather data should not change this.
        PV: -2.12 kW
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 9.5 kWh
        House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 2.41 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        Heat pump: stays on
       */
      emAgentActivation ! Activation(27500)

      // no message, since we are still waiting for secondary data
      resultServiceProxy.expectNoMessage()

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          27500,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(450),
            WattsPerSquareMeter(250),
            Celsius(-5d),
            MetersPerSecond(0d),
            None,
            None,
          ),
          Some(31000),
        )
      }

      val msg27500 = resultServiceProxy.receiveMessages(6)

      msg27500 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 27500, true),
        ExpectResult(pvInput.getUuid, 27500, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 27500),
        ExpectResult(pvInput.getUuid, 27500)
      )

      msg27500.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 27500.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 27500.toDateTime
          p should equalWithTolerance(0.001674489028.asMegaWatt)
          q should equalWithTolerance(0.00055037793.asMegaVar)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(28800)))

      /* TICK 28800
      DomesticHotWaterStorage discharges
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 6.39 kWh
      House demand water   : requiredDemand = 0.21 kWh, possibleDemand = 0.21 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 2.41 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      Heat pump: stays on
       */
      emAgentActivation ! Activation(28800)

      val msg28800 = resultServiceProxy.receiveMessages(5)

      msg28800 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 28800, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 28800)
      )

      msg28800.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 28800.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 28800.toDateTime
          p should equalWithTolerance(0.001674489028.asMegaWatt)
          q should equalWithTolerance(0.0005503779277.asMegaVar)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
          time shouldBe 28800.toDateTime
          qDot should equalWithTolerance(-0.0054634674439.asMegaWatt)
          energy should equalWithTolerance(0.00149814.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(28941)))

      /* TICK 28941
      DomesticHotWaterStorage stops discharging.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 6.06 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 2.41 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.21 kWh
      Heat pump: stays on
       */
      emAgentActivation ! Activation(28941)

      val msg28941 = resultServiceProxy.receiveMessages(5)

      msg28941 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 28941, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 28941)
      )

      msg28941.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 28941.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 28941.toDateTime
          p should equalWithTolerance(0.001674489028.asMegaWatt)
          q should equalWithTolerance(0.000550377928.asMegaVar)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
          time shouldBe 28941.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(0.001284154192.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(31000)))

      /* TICK 31000
        The sun is gone again, it's getting colder as well.
        PV: 0.0 kW
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 1.17 kWh
        House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 2.41 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.21 kWh
        Heat pump: Will be turned off since setPower of EM is zero and
         the heating of the house can be continued from storage.
       */
      emAgentActivation ! Activation(31000)

      // no message, since we are still waiting for secondary data
      resultServiceProxy.expectNoMessage()

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          31000,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(0),
            WattsPerSquareMeter(0),
            Celsius(-35d),
            MetersPerSecond(0d),
            None,
            None,
          ),
          Some(46800),
        )
      }
      val msg31000 = resultServiceProxy.receiveMessages(7)

      // expect messages due to flex activation
      msg31000 should contain allOf (
        ExpectResult(typicalHpInputModel.getUuid, 31000, true),
        ExpectResult(pvInput.getUuid, 31000, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 31000),
        ExpectResult(pvInput.getUuid, 31000)
      )

      msg31000.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 31000.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 31000.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe typicalHeatStorage.getUuid
          time shouldBe 31000.toDateTime
          qDot should equalWithTolerance(-0.011.asMegaWatt)
          energy should equalWithTolerance(0.00803222222.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(31762)))

      /* TICK 31762
      House reaches target temperature.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 4.7 kW
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.21 kWh
      Heat pump: stays off.
       */
      emAgentActivation ! Activation(31762)

      val msg31762 = resultServiceProxy.receiveMessages(6)

      msg31762 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 31762, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 31762)
      )

      msg31762.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 31762.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 31762.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 31762.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          indoorTemp should equalWithTolerance(19.99.asDegreeCelsius)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe typicalHeatStorage.getUuid
          time shouldBe 31762.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(0.0057038888889.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(32400)))

      /* We'll jump through a bunch of activations caused from DomesticHotWaterStorage being active.
 The results are checked implicitly through the state of stored energy at the next result check.
       */

      val secondActivationTicksBlock =
        Seq(32400L, 32560L, 36000L, 36163L, 39600L, 39743L, 41762L)

      val secondTickPairs = secondActivationTicksBlock.zipWithIndex.collect {
        case (tick, index) if index < secondActivationTicksBlock.length - 1 =>
          (tick, secondActivationTicksBlock(index + 1))
      }

      performMultipleActivations(
        emAgentActivation,
        secondTickPairs,
      )

      /* TICK 41762
      House reaches lower temperature.
      House demand heating : requiredDemand = 15.0 kWh, possibleDemand = 15.0 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 4.7 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 0.92 kWh
      Heat pump: stays off - demand will be served by storage.
       */
      emAgentActivation ! Activation(41762)

      val msg41762 = resultServiceProxy.receiveMessages(6)

      msg41762 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 41762, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 41762)
      )

      msg41762.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 41762.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 41762.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 41762.toDateTime
          qDot should equalWithTolerance(0.011.asMegaWatt)
          indoorTemp should equalWithTolerance(18.asDegreeCelsius)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe typicalHeatStorage.getUuid
          time shouldBe 41762.toDateTime
          qDot should equalWithTolerance(-0.011.asMegaWatt)
          energy should equalWithTolerance(0.005703888889.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(43200)))

      /* We'll jump through a bunch of activations caused from DomesticHotWaterStorage being active.
         The results are checked implicitly through the state of stored energy at the next result check.
       */
      val thirdActivationTicksBlock =
        Seq(43200L, 43311L)

      val thirdTickPairs = thirdActivationTicksBlock.zipWithIndex.collect {
        case (tick, index) if index < thirdActivationTicksBlock.length - 1 =>
          (tick, thirdActivationTicksBlock(index + 1))
      }

      performMultipleActivations(
        emAgentActivation,
        thirdTickPairs,
      )

      /* TICK 43311
        Domestic hot water storage stops discharging
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 12.55 kWh
        House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        HeatStorage          : requiredDemand = 0.0 kWh, possibleDemand = 9.46 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 1.09 kWh
        Heat pump: stays off
       */
      emAgentActivation ! Activation(43311)

      val msg43311 = resultServiceProxy.receiveMessages(5)

      msg43311 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 43311, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 43311)
      )

      msg43311.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 43311.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 43311.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
          time shouldBe 43311.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(0.0004056861.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(43628)))

      /* TICK 43628
      Storage is empty now.
      Note: One could argue, that the Hp now should be started to continue heating of the house,
      but actually we don't support this. So the house is cooling down now.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 12.05 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 1.09 kWh
      Heat pump: stays off.
       */
      emAgentActivation ! Activation(43628)

      val msg43628 = resultServiceProxy.receiveMessages(6)

      msg43628 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 43628, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 43628)
      )

      msg43628.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 43628.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 43628.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 43628.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          indoorTemp should equalWithTolerance(18.39.asDegreeCelsius)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe typicalHeatStorage.getUuid
          time shouldBe 43628.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          energy should equalWithTolerance(0.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(45620)))

      /* TICK 45620
        House reaches lower temperature.
        House demand heating : requiredDemand = 15.0 kWh, possibleDemand = 15.0 kWh
        House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
        HeatStorage          : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 1.09 kWh
        Heat pump: turned on to heat the house
       */
      emAgentActivation ! Activation(45620)

      val msg45620 = resultServiceProxy.receiveMessages(5)

      msg45620 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 45620, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 45620)
      )

      msg45620.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 45620.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 45620.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 45620.toDateTime
          qDot should equalWithTolerance(0.011.asMegaWatt)
          indoorTemp should equalWithTolerance(18.asDegreeCelsius)
      }

      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(46800)))

      /* TICK 46800
        New weather data should not change behaviour.
        PV: 0.0 kW
        House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 13.13 kWh
        House demand water   : requiredDemand = 0.12 kWh, possibleDemand = 0.12 kWh
        HeatStorage          : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
        DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 1.09 kWh
        Heat pump: stays on
       */
      emAgentActivation ! Activation(46800)

      weatherDependentAgents.foreach {
        _ ! DataProvision(
          46800,
          weatherService.ref,
          WeatherData(
            WattsPerSquareMeter(0),
            WattsPerSquareMeter(0),
            Celsius(-35d),
            MetersPerSecond(0d),
            None,
            None,
          ),
          Some(57600),
        )
      }

      val msg46800 = resultServiceProxy.receiveMessages(7)

      msg46800 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 46800, true),
        ExpectResult(pvInput.getUuid, 46800, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 46800),
        ExpectResult(pvInput.getUuid, 46800)
      )

      msg46800.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 46800.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 46800.toDateTime
          p should equalWithTolerance(pRunningHp)
          q should equalWithTolerance(qRunningHp)
        case ThermalResultEvent(
              AbstractThermalStorageResult(time, inputModel, qDot, energy)
            ) =>
          inputModel shouldBe smallDomesticHotWaterStorageInput.getUuid
          time shouldBe 46800.toDateTime
          qDot should equalWithTolerance(-0.005474387099.asMegaWatt)
          energy should equalWithTolerance(0.000405686137.asMegaWattHour)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(46879)))

      /* We'll jump through a bunch of activations caused from DomesticHotWaterStorage being active.
    The results are checked implicitly through the state of stored energy at the next result check.
       */
      val fourthActivationTicksBlock =
        Seq(46879L, 50400L, 50445L, 54000L, 54025L, 55263L)

      val fourthTickPairs = fourthActivationTicksBlock.zipWithIndex.collect {
        case (tick, index) if index < fourthActivationTicksBlock.length - 1 =>
          (tick, fourthActivationTicksBlock(index + 1))
      }

      performMultipleActivations(
        emAgentActivation,
        fourthTickPairs,
      )

      /* TICK 55263
      House reaches target temperature.
      House demand heating : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      House demand water   : requiredDemand = 0.0 kWh, possibleDemand = 0.0 kWh
      HeatStorage          : requiredDemand = 10.44 kWh, possibleDemand = 10.44 kWh
      DomesticWaterStorage : requiredDemand = 0.0 kWh, possibleDemand = 1.32 kWh
      Heat pump: turned off, storage won't be recharged since EM setPower is zero.
       */

      emAgentActivation ! Activation(55263)

      val msg55263 = resultServiceProxy.receiveMessages(5)

      msg55263 should contain allOf (
        // expect messages due to flex activation
        ExpectResult(typicalHpInputModel.getUuid, 55263, true),
        // expect messages due to new set point
        ExpectResult(typicalHpInputModel.getUuid, 55263)
      )

      msg55263.collect {
        case ParticipantResultEvent(HpResult(time, uuid, p, q)) =>
          uuid shouldBe typicalHpInputModel.getUuid
          time shouldBe 55263.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ParticipantResultEvent(EmResult(time, uuid, p, q)) =>
          uuid shouldBe emInput.getUuid
          time shouldBe 55263.toDateTime
          p should equalWithTolerance(0.asMegaWatt)
          q should equalWithTolerance(0.asMegaVar)
        case ThermalResultEvent(
              ThermalHouseResult(time, inputModel, qDot, indoorTemp)
            ) =>
          inputModel shouldBe typicalThermalHouse.getUuid
          time shouldBe 55263.toDateTime
          qDot should equalWithTolerance(0.asMegaWatt)
          indoorTemp should equalWithTolerance(20.asDegreeCelsius)
      }
      resultServiceProxy.expectNoMessage()
      scheduler.expectMessage(Completion(emAgentActivation, Some(57600)))
    }
  }
}
