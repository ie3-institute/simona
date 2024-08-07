/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorWeatherService
import edu.ie3.simona.agent.participant.hp.HpAgent
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.config.SimonaConfig.HpRuntimeConfig
import edu.ie3.simona.event.ResultEvent.{
  CylindricalThermalStorageResult,
  ParticipantResultEvent,
  ThermalHouseResult,
}
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.model.thermal.ThermalHouseTestData
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
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
import edu.ie3.simona.test.common.DefaultTestData
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
import org.apache.pekko.actor.typed.scaladsl.adapter.{TypedActorRefOps, _}
import org.apache.pekko.testkit.TestActorRef
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.mockito.MockitoSugar
import squants.motion.MetersPerSecond
import squants.thermal.Celsius

import java.time.ZonedDateTime
import scala.language.postfixOps

/** Test to ensure the functions that a thermal grid and its connected assets is
  * capable.
  */
class ThermalGridIT
    extends ScalaTestWithActorTestKit
    with ThermalHouseTestData
    with AnyWordSpecLike
    with should.Matchers
    with EmInputTestData
    with MockitoSugar
    with DefaultTestData {
  private implicit val classicSystem: ActorSystem = system.toClassic
  protected implicit val simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z")
  protected val simulationEndDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-02T02:00:00Z")

  private val resolution =
    simonaConfig.simona.powerflow.resolution.getSeconds

  private val outputConfigOn = NotifierConfig(
    simulationResultInfo = true,
    powerRequestReply = false,
    flexResult = false,
  )

  val scheduler: TestProbe[SchedulerMessage] = TestProbe("scheduler")
  val runtimeEvents: TestProbe[RuntimeEvent] =
    TestProbe("runtimeEvents")
  val primaryServiceProxy =
    TestProbe[ServiceMessage]("PrimaryServiceProxy")

  val weatherService = TestProbe[ServiceMessage]("WeatherService")

  val resultListener: TestProbe[ResultEvent] = TestProbe("resultListener")

  "A Thermal Grid with thermal house, storage and heat pump not under the control of an energy management" should {
    "be initialized correctly and run through some activations" in {
      val heatPumpAgent = TestActorRef(
        new HpAgent(
          scheduler = scheduler.ref.toClassic,
          initStateData = ParticipantInitializeStateData(
            typicalHpInputModel,
            typicalThermalGrid,
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
            outputConfigOn,
            None,
          ),
          listener = Iterable(resultListener.ref.toClassic),
        ),
        "HeatPumpAgent1",
      )

      scheduler.expectNoMessage()

      /* INIT */

      // heat pump
      heatPumpAgent ! Activation(INIT_SIM_TICK)

      primaryServiceProxy.expectMessage(
        PrimaryServiceRegistrationMessage(typicalHpInputModel.getUuid)
      )
      heatPumpAgent ! RegistrationFailedMessage(
        primaryServiceProxy.ref.toClassic
      )

      weatherService.expectMessage(
        RegisterForWeatherMessage(
          typicalHpInputModel.getNode.getGeoPosition.getY,
          typicalHpInputModel.getNode.getGeoPosition.getX,
        )
      )

      heatPumpAgent ! RegistrationSuccessfulMessage(
        weatherService.ref.toClassic,
        Some(0L),
      )
      val weatherDependentAgents = Seq(heatPumpAgent)

      /* TICK 0

      Start of Simulation

     House demand heating : requiredDemand = 0.0 kWh, additionalDemand ~ 15 kWh
     House demand water   : tba
     ThermalStorage       : requiredDemand = 5.22 kWh, additionalDemand = 10.44 kWh
     DomesticWaterStorage : tba

     Heat pump: on - to serve the storage demand, but will heat up the house first
      */

      heatPumpAgent ! Activation(0L)

      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          0,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(0d),
            WattsPerSquareMeter(0d),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(7200),
        )
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 0.toDateTime
          hpResult.getP should equalWithTolerance(0.0038.asMegaWatt)
          hpResult.getQ should equalWithTolerance(
            0.0012489995996796802.asMegaVar
          )
      }

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe (-1).toDateTime
              qDot should equalWithTolerance(0.0.asMegaWatt)
              indoorTemperature should equalWithTolerance(20.asDegreeCelsius)
            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe (-1).toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(0.asMegaWattHour)
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(0L)))


      /* TICK 6353
      House is fully heated up

       House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
     House demand water   : tba
     ThermalStorage       : requiredDemand = 5.22 kWh, additionalDemand = 10.44 kWh
     DomesticWaterStorage : tba

      Heat pump: on - to serve storage demand
      */

      heatPumpAgent ! Activation(6353L)

      // Results of tick 6353 for hp
      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 6353.toDateTime
          hpResult.getP should equalWithTolerance(0.0038.asMegaWatt)
          hpResult.getQ should equalWithTolerance(
            0.0012489995996796802.asMegaVar
          )
      }

      // Results of 0 for house and storage
      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 0.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                19.9999074074074.asDegreeCelsius
              )
            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 0.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(0.asMegaWattHour)
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      // FIXME? Why next tick 6353?
      scheduler.expectMessage(Completion(heatPumpAgent, Some(6353L)))

      /* TICK 7200
New weather data (unchanged) incoming

     House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
     House demand water   : tba
     ThermalStorage       : requiredDemand = 2.63 kWh, additionalDemand = 7.85 kWh
     DomesticWaterStorage : tba

Heat pump: on
*/


      heatPumpAgent ! Activation(7200L)


      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          7200L,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(1d),
            WattsPerSquareMeter(1d),
            Celsius(-5d),
            MetersPerSecond(0d),
          ),
          Some(28800L),
        )
      }

      // Results of 7200 for hp
      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 7200.toDateTime
          hpResult.getP should equalWithTolerance(0.0038.asMegaWatt)
          hpResult.getQ should equalWithTolerance(
            0.0012489995996796802.asMegaVar
          )
      }

      // Results of 6353 for house and storage
      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 6353.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                21.99992810459535.asDegreeCelsius
              )

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 6353.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              energy should equalWithTolerance(0.asMegaWattHour)
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      // FIXME? Why next tick 7200?
      scheduler.expectMessage(Completion(heatPumpAgent, Some(7200L)))

      /* TICK 7200
Storage will be full

     House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
     House demand water   : tba
     ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
DomesticWaterStorage : tba

Heat pump: off
*/

      heatPumpAgent ! Activation(9770L)

      // Results of 9770 for hp
      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 9770.toDateTime
          hpResult.getP should equalWithTolerance(0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.asMegaVar)
      }

      // Results of 7200 for house and storage
      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 7200.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                21.9152283301339.asDegreeCelsius
              )
            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 7200.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              energy should equalWithTolerance(0.002588055555555.asMegaWattHour)
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      // FIXME? Why next tick 9770?
      scheduler.expectMessage(Completion(heatPumpAgent, Some(9770L)))

      heatPumpAgent ! Activation(28800L)

      /* TICK 28800
      House would reach lowerTempBoundary at tick 47518
      but now it's getting colder which should decrease inner temp of house faster

     House demand heating : requiredDemand = 0.0 kWh, additionalDemand = 27.22 kWh
     House demand water   : tba
     ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
DomesticWaterStorage : tba

Heat pump: off
*/

      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          28800L,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(2d),
            WattsPerSquareMeter(2d),
            Celsius(-25d),
            MetersPerSecond(0d),
          ),
          Some(50400L),
        )
      }


      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 28800.toDateTime
          hpResult.getP should equalWithTolerance(0.0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.0.asMegaVar)
      }

      // Results of 9770 for house and storage
      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 9770.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                21.65903523084334.asDegreeCelsius
              )
            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 9770.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              energy should equalWithTolerance(0.01044.asMegaWattHour)
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(28800L)))

      /* TICK 31106

?


     House demand heating : requiredDemand = 15.0 kWh, additionalDemand = 30.00 kWh
     House demand water   : tba
     ThermalStorage       : requiredDemand = 0.0 kWh, additionalDemand = 0.0 kWh
DomesticWaterStorage : tba

Heat pump: on
*/

      heatPumpAgent ! Activation(31106L)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 31106.toDateTime
          hpResult.getP should equalWithTolerance(0.0038.asMegaWatt)
          hpResult.getQ should equalWithTolerance(
            0.0012489995996796802.asMegaVar
          )
      }

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 28800.toDateTime
              qDot should equalWithTolerance(0.0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                18.3704373032769.asDegreeCelsius
              )

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 28800.toDateTime
              qDot should equalWithTolerance(0.0.asMegaWatt)
              energy should equalWithTolerance(0.01044.asMegaWattHour)
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(31106)))

      /* TICK 47225

?

House demand heating :
House demand water   : tba
ThermalStorage       : empty
DomesticWaterStorage : tba

Heat pump: on
*/

      heatPumpAgent ! Activation(47225L)

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 47225.toDateTime
          hpResult.getP should equalWithTolerance(0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(
            0.asMegaVar
          )
      }

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 31106.toDateTime
              qDot should equalWithTolerance(0.011.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                18.00002164245703.asDegreeCelsius
              )

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 31106.toDateTime
              qDot should equalWithTolerance(
                0.0.asMegaWatt
              )
              energy should equalWithTolerance(
                0.01044.asMegaWattHour
              )
          }
        }

      scheduler.expectMessage(Completion(heatPumpAgent, Some(47225)))

      /* TICK 50400

New weather data: it's getting warmer again

House demand heating :
House demand water   : tba
ThermalStorage       : empty
DomesticWaterStorage : tba

Heat pump: on
*/

      heatPumpAgent ! Activation(50400L)


      weatherDependentAgents.foreach {
        _ ! ProvideWeatherMessage(
          50400L,
          weatherService.ref.toClassic,
          WeatherData(
            WattsPerSquareMeter(3d),
            WattsPerSquareMeter(3d),
            Celsius(5d),
            MetersPerSecond(0d),
          ),
          Some(151200L),
        )
      }

      resultListener.expectMessageType[ParticipantResultEvent] match {
        case ParticipantResultEvent(hpResult) =>
          hpResult.getInputModel shouldBe typicalHpInputModel.getUuid
          hpResult.getTime shouldBe 50400.toDateTime
          hpResult.getP should equalWithTolerance(0.0.asMegaWatt)
          hpResult.getQ should equalWithTolerance(0.0.asMegaVar)
      }

      // results of tick 47225 where house reached the upper boundary
      // should be heated by using thermal energy from the storage FIXME

      Range(0, 2)
        .map { _ =>
          resultListener.expectMessageType[ResultEvent]
        }
        .foreach { case ResultEvent.ThermalResultEvent(thermalUnitResult) =>
          thermalUnitResult match {
            case ThermalHouseResult(
                  time,
                  inputModel,
                  qDot,
                  indoorTemperature,
                ) =>
              inputModel shouldBe typicalThermalHouse.getUuid
              time shouldBe 47225.toDateTime
              qDot should equalWithTolerance(0.asMegaWatt)
              indoorTemperature should equalWithTolerance(
                21.99992035040234.asDegreeCelsius
              )

            case CylindricalThermalStorageResult(
                  time,
                  inputModel,
                  qDot,
                  energy,
                ) =>
              inputModel shouldBe typicalThermalStorage.getUuid
              time shouldBe 47225.toDateTime
              qDot should equalWithTolerance(0.0.asMegaWatt)
              energy should equalWithTolerance(0.01044.asMegaWattHour)
            case _ =>
              fail(
                "Expected a ThermalHouseResult and a ThermalStorageResult but got something else"
              )
          }
        }
      scheduler.expectMessage(Completion(heatPumpAgent, Some(50400L)))
    }
  }
}
